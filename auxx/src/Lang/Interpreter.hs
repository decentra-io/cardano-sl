{-# LANGUAGE RankNTypes #-}

module Lang.Interpreter
       ( Value(..)
       , evaluate
       , EvalError(..)
       ) where

import           Universum

import           Control.Monad.Except (throwError)

import           Lang.Argument        (ProcError, consumeArguments)
import           Lang.Command         (CommandProc (..))
import           Lang.Name            (Name)
import           Lang.Syntax          (Expr (..), Lit (..), ProcCall (..))
import           Lang.Value           (Value (..))

data EvalCtx m = EvalCtx
    { ecCommandProcs :: [CommandProc m]
    } deriving ()

data EvalError
    = CommandNotSupported Name
    | InvalidArguments Name (ProcError Value)
    deriving (Eq, Ord, Show)

instance Exception EvalError

type T m a = Monad m =>
    ReaderT (EvalCtx m) (ExceptT EvalError m) a

evaluate :: Monad m => [CommandProc m] -> Expr -> m (Either EvalError Value)
evaluate commandProcs expr = runExceptT $ runReaderT (eval expr) ctx
  where
    ctx = EvalCtx { ecCommandProcs = commandProcs }

eval :: Expr -> T m Value
eval = \case
    ExprUnit -> return ValueUnit
    ExprLit l -> return (literalToValue l)
    ExprGroup exprs -> evalExprGroup exprs
    ExprProcCall procCall ->
        evalProcCall =<< traverse eval procCall

evalExprGroup :: NonEmpty Expr -> T m Value
evalExprGroup (x :| xs) = case nonEmpty xs of
    Nothing  -> eval x
    Just xs' -> eval x *> evalExprGroup xs'

evalProcCall :: ProcCall Value -> T m Value
evalProcCall (ProcCall procName args) = do
    CommandProc{..} <- lookupCommandProc procName
    e <- either (throwError . InvalidArguments cpName) return $
         consumeArguments cpArgumentConsumer args
    lift . lift $ cpExec e

lookupCommandProc :: Name -> T m (CommandProc m)
lookupCommandProc name = do
    commandProcs <- asks ecCommandProcs
    maybe (throwError $ CommandNotSupported name) return $
        find (\cp -> cpName cp == name) commandProcs

literalToValue :: Lit -> Value
literalToValue = \case
    LitNumber   a -> ValueNumber   a
    LitString   a -> ValueString   a
    LitAddress  a -> ValueAddress  a
    LitFilePath a -> ValueFilePath a
