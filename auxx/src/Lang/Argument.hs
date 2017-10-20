{-# LANGUAGE GADTs #-}

{-

{-# LANGUAGE ApplicativeDo #-}
acExample1 :: ArgumentConsumer String (String, Integer)
acExample1 = do
    x <- getArg ("Type1", Just) "kek-mem"
    y <- getArg ("Type2", readMaybe) "patak"
    return (x, y)

-}

module Lang.Argument
       ( ArgumentError(..)
       , isEmptyArgumentError
       , TypeName(..)
       , TypeError(..)
       , ProcError(..)
       , isEmptyProcError
       , ArgumentConsumer
       , consumeArguments
       , getArg
       ) where

import           Universum

import           Control.Lens    (_Right)
import qualified Data.Set        as Set
import           Numeric.Natural (Natural)

import           Lang.Name       (Name (..))
import           Lang.Syntax     (Arg (..))

data ArgumentError = ArgumentError
    { aeMissingKeys    :: !(Set Name)
    , aeIrrelevantKeys :: !(Set Name)
    , aeIrrelevantPos  :: !Natural
    } deriving (Eq, Ord, Show)

isEmptyArgumentError :: ArgumentError -> Bool
isEmptyArgumentError ArgumentError{..} =
    Set.null aeMissingKeys && Set.null aeIrrelevantKeys && aeIrrelevantPos == 0

instance Monoid ArgumentError where
    mempty = ArgumentError Set.empty Set.empty 0
    mappend (ArgumentError m1 i1 p1) (ArgumentError m2 i2 p2) =
        ArgumentError (Set.union m1 m2) (Set.union i1 i2) (p1 + p2)

newtype TypeName = TypeName Text
    deriving (Eq, Ord, Show, IsString)

data TypeError v = TypeError
    { teExpectedType :: !TypeName
    , teActualValue  :: !v
    } deriving (Eq, Ord, Show)

data ProcError v = ProcError
    { peArgumentError :: !ArgumentError
    , peTypeErrors    :: !(Set (TypeError v))
    } deriving (Eq, Ord, Show)

isEmptyProcError :: ProcError v -> Bool
isEmptyProcError ProcError{..} =
    isEmptyArgumentError peArgumentError && Set.null peTypeErrors

instance Ord v => Monoid (ProcError v) where
    mempty = ProcError mempty Set.empty
    mappend (ProcError a1 t1) (ProcError a2 t2) =
        ProcError (mappend a1 a2) (Set.union t1 t2)

data ArgumentConsumerState v = ACS
    { acsRemaining :: ![Arg v]
    , acsError     :: !(ProcError v)
    } deriving (Eq, Ord, Show)

data ArgumentConsumer v a where
    GetArg :: (TypeName, v -> Maybe a) -> Name -> ArgumentConsumer v a
    AcPure :: a -> ArgumentConsumer v a
    AcAp :: ArgumentConsumer v (a -> b) -> ArgumentConsumer v a -> ArgumentConsumer v b

instance Functor (ArgumentConsumer v) where
    fmap f x = pure f <*> x

instance Applicative (ArgumentConsumer v) where
    pure = AcPure
    (<*>) = AcAp

getArg :: (TypeName, v -> Maybe a) -> Name -> ArgumentConsumer v a
getArg = GetArg

runArgumentConsumer :: forall v a.
       Ord v
    => ArgumentConsumer v a
    -> ArgumentConsumerState v
    -> (Maybe a, ArgumentConsumerState v)
runArgumentConsumer ac acs = case ac of
    GetArg (typeName, convert) key ->
        case lookupArg key (acsRemaining acs) of
            Left argError ->
                let
                    procError = mempty { peArgumentError = argError }
                    acs' = acs { acsError = acsError acs `mappend` procError }
                in
                    (Nothing, acs')
            Right (v, remaining') ->
                let
                    acs' = acs { acsRemaining = remaining'
                               , acsError = addProcError $ acsError acs }
                    addProcError =
                        if isJust mResult
                        then identity
                        else mappend procError
                    typeError = TypeError { teExpectedType = typeName
                                          , teActualValue = v }
                    procError = (mempty :: ProcError v)
                                          { peTypeErrors = Set.singleton typeError }
                    mResult = convert v
                in
                    (mResult, acs')
    AcPure a -> (Just a, acs)
    AcAp ac1 ac2 ->
        let
            (mResult1, acs') = runArgumentConsumer ac1 acs
            (mResult2, acs'') = runArgumentConsumer ac2 acs'
            mResult = mResult1 <*> mResult2
        in
            (mResult, acs'')

lookupArg :: Name -> [Arg v] -> Either ArgumentError (v, [Arg v])
lookupArg name = \case
    [] -> Left mempty { aeMissingKeys = Set.singleton name }
    ArgPos a : args -> Right (a, args)
    arg@(ArgKw name' a) : args ->
        if name == name'
        then Right (a, args)
        else over (_Right._2) (arg:) $ lookupArg name args

consumeArguments :: Ord v => ArgumentConsumer v a -> [Arg v] -> Either (ProcError v) a
consumeArguments ac args =
    let
        acs = ACS { acsRemaining = args, acsError = mempty }
        (mResult, acs') = runArgumentConsumer ac acs
        procError = acsError acs' `mappend` irrelevanceError
        irrelevanceError = mempty { peArgumentError = toIrrelevanceError (acsRemaining acs') }
        mResult' = mResult <* guard (isEmptyProcError procError)
    in
        case mResult' of
            Nothing -> Left procError
            Just a  -> Right a

toIrrelevanceError :: [Arg v] -> ArgumentError
toIrrelevanceError = foldMap $ \case
    ArgPos _ -> mempty { aeIrrelevantPos = 1 }
    ArgKw key _ -> mempty { aeIrrelevantKeys = Set.singleton key }
