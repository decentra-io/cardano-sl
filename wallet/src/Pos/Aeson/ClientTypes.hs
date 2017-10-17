module Pos.Aeson.ClientTypes
       (
       ) where

import           Universum

import           Data.Aeson                   (ToJSON (..), ToJSONKey (..))
import           Data.Aeson.TH                (defaultOptions, deriveJSON, deriveToJSON)
import           Data.Aeson.Types             (toJSONKeyText)
import           Data.Time.Units              (Millisecond)
import           Formatting                   (sformat)
import           Serokell.Util.Base16         (base16F)

import           Pos.Core.Fee                 (Coeff, TxFeePolicy, TxSizeLinear)
import           Pos.Core.Types               (SoftforkRule, SoftwareVersion (..))
import           Pos.Update.Core.Types        (BlockVersionModifier, SystemTag,
                                               UpdateData, UpdateProposal, VoteState,
                                               getSystemTag)
import           Pos.Update.Poll.Types        (DecidedProposalState, DpsExtra,
                                               ProposalState, UndecidedProposalState,
                                               UpsExtra)
import           Pos.Util.BackupPhrase        (BackupPhrase)
import           Pos.Wallet.Web.ClientTypes   (Addr, CAccount, CAccountId, CAccountInit,
                                               CAccountMeta, CAddress, CCoin,
                                               CConfirmedProposalState, CHash, CId,
                                               CInitialized, CInitialized,
                                               CPaperVendWalletRedeem, CProfile, CProfile,
                                               CPtxCondition, CTExMeta, CTx, CTxId,
                                               CTxMeta, CWAddressMeta, CWallet,
                                               CWalletAssurance, CWalletInit, CWalletMeta,
                                               CWalletRedeem, SyncProgress, Wal)
import           Pos.Wallet.Web.Error         (WalletError)
import           Pos.Wallet.Web.Sockets.Types (NotifyEvent)

deriveJSON defaultOptions ''CAccountId
deriveJSON defaultOptions ''CWAddressMeta
deriveJSON defaultOptions ''CWalletAssurance
deriveJSON defaultOptions ''CAccountMeta
deriveJSON defaultOptions ''CAccountInit
deriveJSON defaultOptions ''CWalletRedeem
deriveJSON defaultOptions ''CWalletMeta
deriveJSON defaultOptions ''CWalletInit
deriveJSON defaultOptions ''CPaperVendWalletRedeem
deriveJSON defaultOptions ''CTxMeta
deriveJSON defaultOptions ''CProfile
deriveJSON defaultOptions ''BackupPhrase
deriveJSON defaultOptions ''CId
deriveJSON defaultOptions ''Wal
deriveJSON defaultOptions ''Addr
deriveJSON defaultOptions ''CHash
deriveJSON defaultOptions ''CInitialized

deriveJSON defaultOptions ''CCoin
deriveJSON defaultOptions ''CTxId
deriveJSON defaultOptions ''CAddress
deriveJSON defaultOptions ''CAccount
deriveJSON defaultOptions ''CWallet
deriveJSON defaultOptions ''CPtxCondition
deriveJSON defaultOptions ''CTx
deriveJSON defaultOptions ''CTExMeta
deriveJSON defaultOptions ''SoftwareVersion
deriveJSON defaultOptions ''CConfirmedProposalState

deriveToJSON defaultOptions ''SyncProgress
deriveToJSON defaultOptions ''NotifyEvent
deriveToJSON defaultOptions ''WalletError

instance ToJSON ByteString where
    toJSON = toJSON . sformat base16F

instance ToJSONKey SystemTag where
    toJSONKey = toJSONKeyText getSystemTag

deriveToJSON defaultOptions ''Coeff
deriveToJSON defaultOptions ''TxSizeLinear
deriveToJSON defaultOptions ''TxFeePolicy
deriveToJSON defaultOptions ''SoftforkRule

deriveToJSON defaultOptions ''Millisecond
deriveToJSON defaultOptions ''UpdateProposal
deriveToJSON defaultOptions ''VoteState
deriveToJSON defaultOptions ''DpsExtra
deriveToJSON defaultOptions ''UpsExtra
deriveToJSON defaultOptions ''BlockVersionModifier
deriveToJSON defaultOptions ''UpdateData
deriveToJSON defaultOptions ''UndecidedProposalState
deriveToJSON defaultOptions ''DecidedProposalState
deriveToJSON defaultOptions ''ProposalState
