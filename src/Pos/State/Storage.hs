{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Storage with node local state which should be persistent.

module Pos.State.Storage
       (
         Storage
       , storageFromUtxo

       , Query
       , getBlock
       , getBlockByDepth
       , getHeadBlock
       , getBestChain
       , getChainPart
       , getGlobalSscState
       , getGlobalSscStateByDepth
       , getLeaders

       , getUtxo
       , getUtxoByDepth
       , getOldestUtxo
       , isTxVerified
       , processTx

       , getOurShares
       , getParticipants
       , getThreshold
       , mayBlockBeUseful

       , ProcessBlockRes (..)
       , ProcessTxRes (..)

       , Update
       , createNewBlock
       , processBlock
       , processNewSlot
       ) where

import           Universum

import           Control.Lens                 (makeClassy, use, (.=), (^.))
import           Control.Monad.TM             ((.=<<.))
import           Data.Acid                    ()
import           Data.Default                 (Default, def)
import           Data.List.NonEmpty           (NonEmpty ((:|)))
import           Data.Maybe                   (fromJust)
import           Data.SafeCopy                (SafeCopy (..), contain, safeGet, safePut)
import           Data.Tagged                  (untag)
import           Formatting                   (build, sformat, (%))
import           Serokell.AcidState           ()
import           Serokell.Util                (VerificationRes (..))
import           System.Wlog                  (WithLogger, logDebug)

import           Pos.Constants                (k)
import           Pos.Crypto                   (EncShare, ProxySecretKey, SecretKey,
                                               VssPublicKey)
import           Pos.Genesis                  (genesisUtxo)
import           Pos.Ssc.Class.Helpers        (SscHelpersClass (..))
import           Pos.Ssc.Class.Storage        (HasSscStorage (..), SscStorageClass (..))
import           Pos.Ssc.Class.Types          (Ssc (..))
import           Pos.Ssc.GodTossing.Functions (getThreshold)
import           Pos.State.Storage.Block      (BlockStorage,
                                               HasBlockStorage (blockStorage), blkCleanUp,
                                               blkCreateGenesisBlock, blkCreateNewBlock,
                                               blkProcessBlock, blkRollback, blkSetHead,
                                               getBestChain, getBlock, getBlockByDepth,
                                               getChainPart, getHeadBlock, getLeaders,
                                               getSlotDepth, mayBlockBeUseful,
                                               mkBlockStorage)
import           Pos.State.Storage.Types      (AltChain, ProcessBlockRes (..),
                                               ProcessTxRes (..), mkPBRabort)
import           Pos.Txp.Storage              (HasTxStorage (txStorage), TxStorage,
                                               filterLocalTxs, getOldestUtxo, getUtxo,
                                               getUtxoByDepth, isTxVerified, processTx,
                                               txApplyBlocks, txRollback,
                                               txStorageFromUtxo, txVerifyBlocks)
import           Pos.Types                    (Address, Block, EpochIndex,
                                               EpochOrSlot (..), GenesisBlock,
                                               IdTxWitness, MainBlock, SlotId (..),
                                               SlotLeaders, Utxo, blockMpc, blockTxs,
                                               epochIndexL, epochOrSlot, flattenSlotId,
                                               gbHeader, getEpochOrSlot, headerHashG,
                                               slotIdF, unflattenSlotId, verifyTxAlone)
import           Pos.Util                     (AsBinary, readerToState, _neLast)


-- | Main cardano-sl state, combining sub-states into single one.
data Storage ssc = Storage
    { -- | State of MPC.
      __mpcStorage   :: !(SscStorage ssc)
    , -- | Transactions part of /static-state/.
      __txStorage    :: !TxStorage
    , -- | Blockchain part of /static-state/.
      __blockStorage :: !(BlockStorage ssc)
    , -- | Id of last seen slot.
      _slotId        :: !SlotId
    }

-- | Classy lenses generated by TH
makeClassy ''Storage

-- | Type alias for MonadReader of storage that supports SSC.
type Query  ssc a = forall m . (Ssc ssc, MonadReader (Storage ssc) m) => m a

-- | Type alias for MonadState of storage that supports SSC.
type Update ssc a = forall m . (Ssc ssc, MonadState (Storage ssc) m) => m a

-- | Type alias for MonadState of storage that supports SSC.
type UpdateWithLog ssc a = forall m . ( Ssc ssc
                                      , MonadState (Storage ssc) m
                                      , WithLogger m
                                      ) => m a

instance Ssc ssc => SafeCopy (Storage ssc) where
    putCopy Storage {..} =
        contain $
        do safePut __mpcStorage
           safePut __txStorage
           safePut __blockStorage
           safePut _slotId
    getCopy =
        contain $
        do __mpcStorage <- safeGet
           __txStorage <- safeGet
           __blockStorage <- safeGet
           _slotId <- safeGet
           return $! Storage {..}

instance HasSscStorage ssc (Storage ssc) where
    sscStorage = _mpcStorage
instance HasTxStorage (Storage ssc) where
    txStorage = _txStorage
instance HasBlockStorage (Storage ssc) ssc where
    blockStorage = _blockStorage

instance (Ssc ssc, Default (SscStorage ssc)) => Default (Storage ssc) where
    def = storageFromUtxo $ genesisUtxo def

-- | Create default storage with specified utxo
storageFromUtxo
    :: (Ssc ssc, Default (SscStorage ssc))
    => Utxo -> Storage ssc
storageFromUtxo u =
    Storage
    { __mpcStorage = def
    , __txStorage = txStorageFromUtxo u
    , __blockStorage = mkBlockStorage u
    , _slotId = unflattenSlotId 0
    }

getHeadSlot :: Query ssc EpochOrSlot
getHeadSlot = getEpochOrSlot <$> getHeadBlock

-- | Get global SSC data.
getGlobalSscState
    :: forall ssc.
       SscStorageClass ssc
    => Query ssc (SscGlobalState ssc)
getGlobalSscState = sscGetGlobalState @ssc

-- | Get global SSC data that was observed N blocks ago.
getGlobalSscStateByDepth
    :: forall ssc.
       SscStorageClass ssc
    => Word -> Query ssc (Maybe (SscGlobalState ssc))
getGlobalSscStateByDepth = sscGetGlobalStateByDepth @ssc

-- | Create a new block on top of best chain if possible.
-- Block can be created if:
-- • we know genesis block for epoch from given SlotId
-- • last known block is not more than k slots away from
-- given SlotId
createNewBlock
    :: (SscStorageClass ssc)
    => [IdTxWitness]
    -> SecretKey
    -> Maybe (ProxySecretKey (EpochIndex,EpochIndex))
    -> SlotId
    -> SscPayload ssc
    -> Update ssc (Either Text (MainBlock ssc))
createNewBlock localTxs sk pSk sId sscPayload =
    maybe (Right <$> createNewBlockDo localTxs sk pSk sId sscPayload) (pure . Left) =<<
    readerToState (canCreateBlock sId)

createNewBlockDo
    :: forall ssc.
       (SscStorageClass ssc)
    => [IdTxWitness]
    -> SecretKey
    -> Maybe (ProxySecretKey (EpochIndex,EpochIndex))
    -> SlotId
    -> SscPayload ssc
    -> Update ssc (MainBlock ssc)
createNewBlockDo localTxs sk pSk sId sscPayload = do
    globalPayload <- readerToState $ getGlobalSscState
    let filteredPayload = sscFilterPayload @ssc sscPayload globalPayload
    headUtxo <- readerToState $ fromJust <$> getUtxoByDepth 0
    let filteredTxs = filterLocalTxs localTxs headUtxo
    blk <- blkCreateNewBlock sk pSk sId (map snd filteredTxs) filteredPayload
    let blocks = Right blk :| []
    sscApplyBlocks blocks
    blk <$ txApplyBlocks filteredTxs blocks

canCreateBlock :: SlotId -> Query ssc (Maybe Text)
canCreateBlock sId = do
    headSlot <- getHeadSlot
    let maxSlotId = addKSafe $ epochOrSlot (`SlotId` 0) identity headSlot
    let retRes = return . Just
    if | sId > maxSlotId ->
           retRes "slot id is too big, we don't know recent block"
       | (EpochOrSlot $ Right sId) < headSlot ->
           retRes "slot id is not biger than one from last known block"
       | otherwise -> return Nothing
  where

    addKSafe si = si {siSlot = min (6 * k - 1) (siSlot si + k)}

-- | Do all necessary changes when a block is received.
processBlock :: (SscHelpersClass ssc, SscStorageClass ssc)
    =>[IdTxWitness] -> SlotId -> Block ssc -> Update ssc (ProcessBlockRes ssc)
processBlock localTxs curSlotId blk = do
    let txs =
            case blk of
                Left _        -> []
                Right mainBlk -> toList $ mainBlk ^. blockTxs
    let txRes = foldMap verifyTxAlone txs
    case txRes of
        VerSuccess        -> processBlockDo localTxs curSlotId blk
        VerFailure errors -> return $ mkPBRabort errors

processBlockDo
    :: forall ssc.
       (SscHelpersClass ssc, SscStorageClass ssc)
    => [IdTxWitness] -> SlotId -> Block ssc -> Update ssc (ProcessBlockRes ssc)
processBlockDo localTxs curSlotId blk = do
    let verifyMpc mainBlk =
            untag sscVerifyPayload (mainBlk ^. gbHeader) (mainBlk ^. blockMpc)
    let mpcResPure = either (const mempty) verifyMpc blk
    r <- blkProcessBlock curSlotId blk (pure mpcResPure)
    case r of
        PBRgood (toRollback, chain) -> do
            mpcRes <- readerToState $ sscVerifyBlocks @ssc toRollback chain
            txRes <- readerToState $ txVerifyBlocks toRollback chain
            case mpcRes <> eitherToVerResult txRes of
                VerSuccess        -> processBlockFinally localTxs toRollback chain
                VerFailure errors -> return $ mkPBRabort errors
        -- if we need block which we already know, we just use it
        PBRmore h ->
            maybe (pure r) (processBlockDo localTxs curSlotId) =<<
            readerToState (getBlock h)
        _ -> return r
  where
    eitherToVerResult = either (VerFailure . (:[])) (const VerSuccess)

-- At this point all checks have been passed and we know that we can
-- adopt this AltChain.
processBlockFinally :: forall ssc . SscStorageClass ssc
                    => [IdTxWitness]
                    -> Word
                    -> AltChain ssc
                    -> Update ssc (ProcessBlockRes ssc)
processBlockFinally localTxs toRollback blocks = do
    sscRollback @ssc toRollback
    sscApplyBlocks @ssc blocks
    txRollback localTxs toRollback
    txApplyBlocks localTxs blocks
    blkRollback toRollback
    blkSetHead (blocks ^. _neLast . headerHashG)
    knownEpoch <- use (slotId . epochIndexL)
    -- When we adopt alternative chain, it may revert genesis block
    -- already created for current epoch. And we will be in situation
    -- where best chain doesn't have genesis block for current epoch.
    -- If then we need to create block in current epoch, it will be
    -- definitely invalid. To prevent it we create genesis block after
    -- possible revert. Note that createGenesisBlock function will
    -- create block only for epoch which is one more than epoch of
    -- head, so we don't perform such check here.  Also note that it
    -- is not strictly necessary, because we have `canCreateBlock`
    -- which prevents us from creating block when we are not ready,
    -- but it is still good as an optimization. Even if later we see
    -- that there were other valid blocks in old epoch, we will
    -- replace chain and everything will be fine.
    _ <- createGenesisBlock knownEpoch
    return $ PBRgood (toRollback, blocks)

-- | Do all necessary changes when new slot starts.  Specifically this
-- function creates genesis block if necessary and does some clean-up.
processNewSlot
    :: forall ssc.
       (SscStorageClass ssc)
    => SlotId -> UpdateWithLog ssc (Maybe (GenesisBlock ssc))
processNewSlot sId = do
    knownSlot <- use slotId
    logDebug $ sformat ("Known slot = "%slotIdF) knownSlot
    if sId > knownSlot
       then processNewSlotDo sId
       else pure Nothing

processNewSlotDo
    :: forall ssc .
       (SscStorageClass ssc)
    => SlotId -> Update ssc (Maybe (GenesisBlock ssc))
processNewSlotDo sId@SlotId {..} = do
    slotId .= sId
    mGenBlock <-
      if siSlot == 0
         then createGenesisBlock @ssc siEpoch
         else pure Nothing
    mGenBlock <$ blkCleanUp sId

-- We create genesis block for i-th epoch when head of currently known
-- best chain is MainBlock corresponding to one of last `k` slots of
-- (i - 1)-th epoch. Main check is that epoch is (last stored epoch +
-- 1), but we also don't want to create genesis block on top of blocks
-- from previous epoch which are not from last k slots, because it's
-- practically impossible for them to be valid.
shouldCreateGenesisBlock :: EpochIndex -> Query ssc Bool
-- Genesis block for 0-th epoch is hardcoded.
shouldCreateGenesisBlock 0 = pure False
shouldCreateGenesisBlock epoch =
    doCheck . epochOrSlot (`SlotId` 0) identity <$> getHeadSlot
  where
    doCheck SlotId {..} = siEpoch == epoch - 1 && siSlot >= 5 * k

createGenesisBlock
    :: forall ssc.
       SscStorageClass ssc
    => EpochIndex -> Update ssc (Maybe (GenesisBlock ssc))
createGenesisBlock epoch =
    ifM (readerToState $ shouldCreateGenesisBlock epoch)
        (Just <$> createGenesisBlockDo epoch)
        (pure Nothing)

createGenesisBlockDo
    :: forall ssc.
       SscStorageClass ssc
    => EpochIndex -> Update ssc (GenesisBlock ssc)
createGenesisBlockDo epoch = do
    leaders <- readerToState $ calculateLeaders epoch
    genBlock <- blkCreateGenesisBlock epoch leaders
    -- Genesis block contains no transactions,
    --    so we should update only SSC
    sscApplyBlocks $ Left genBlock :| []
    pure genBlock

calculateLeaders
    :: forall ssc.
       SscStorageClass ssc
    => EpochIndex -> Query ssc SlotLeaders
calculateLeaders epoch = do
    depth <- fromMaybe onErrorGetDepth <$> getMpcCrucialDepth epoch
    utxo <- fromMaybe onErrorGetUtxo <$> getUtxoByDepth depth
    -- TODO: overall 'calculateLeadersDo' gets utxo twice, could be optimised
    threshold <-
        getThreshold . length . fromMaybe onErrorGetParticipants <$>
        getParticipants epoch
    either onErrorCalcLeaders identity <$> sscCalculateLeaders @ssc epoch utxo threshold
  where
    onErrorGetDepth = panic "Depth of MPC crucial slot isn't reasonable"
    onErrorGetUtxo =
        panic "Failed to get utxo necessary for leaders calculation"
    onErrorGetParticipants =
        panic "Failed to get participants necessary for leaders calculation"
    onErrorCalcLeaders e =
        panic (sformat ("Leaders calculation reported error: " % build) e)

-- | Get keys of nodes participating in an epoch. A node participates if,
-- when there were 'k' slots left before the end of the previous epoch, both
-- of these were true:
--
--   1. It was a stakeholder.
--   2. It had already sent us its VSS key by that time.
getParticipants
    :: forall ssc.
       SscStorageClass ssc
    => EpochIndex -> Query ssc (Maybe (NonEmpty (AsBinary VssPublicKey)))
getParticipants epoch = do
    mDepth <- getMpcCrucialDepth epoch
    mUtxo <- getUtxoByDepth .=<<. mDepth
    case (,) <$> mDepth <*> mUtxo of
        Nothing            -> return Nothing
        Just (depth, utxo) -> sscGetParticipants @ssc depth utxo

-- slot such that data after it is used for MPC in given epoch
mpcCrucialSlot :: EpochIndex -> SlotId
mpcCrucialSlot 0     = SlotId {siEpoch = 0, siSlot = 0}
mpcCrucialSlot epoch = SlotId {siEpoch = epoch - 1, siSlot = 5 * k - 1}

getMpcCrucialDepth :: EpochIndex -> Query ssc (Maybe Word)
getMpcCrucialDepth epoch = do
    let crucialSlot = mpcCrucialSlot epoch
    (depth, slot) <- getSlotDepth crucialSlot
    if flattenSlotId slot + 2 * k < flattenSlotId (SlotId epoch 0)
        then return Nothing
        else return (Just depth)

-- | Decrypt shares (in commitments) that are intended for us and that we can
-- decrypt.
getOurShares
    :: forall ssc.
       SscStorageClass ssc
    => AsBinary VssPublicKey -- ^ Our VSS key
    -> Query ssc (HashMap Address (AsBinary EncShare))
getOurShares = sscGetOurShares @ssc
