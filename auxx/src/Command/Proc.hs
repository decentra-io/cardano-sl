{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-unused-imports #-} -- TODO: remove

module Command.Proc
       ( createCommandProcs
       ) where

import           Universum

import           Data.ByteString.Base58     (bitcoinAlphabet, encodeBase58)
import           Data.List                  ((!!))
import           Data.Text.Lens             (packed)
import           Formatting                 (build, int, sformat, stext, (%))
import           Formatting                 (build, int, sformat, stext, (%))
import           Pos.Client.Txp.Balances    (getBalance)
import           Pos.Communication          (SendActions)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Types                  (Address, coinF)
import           Pos.Util.CompileInfo       (HasCompileInfo)
import           System.Wlog                (logError, logInfo)
import qualified Text.JSON.Canonical        as CanonicalJSON

import           Pos.Binary                 (serialize')
import           Pos.Client.KeyStorage      (addSecretKey, getSecretKeysPlain)
import           Pos.Client.Txp.Balances    (getBalance)
import           Pos.Communication          (MsgType (..), Origin (..), SendActions,
                                             dataFlow, immediateConcurrentConversations)
import           Pos.Core                   (addressHash, coinF)
import           Pos.Core.Address           (makeAddress)
import           Pos.Core.Configuration     (genesisSecretKeys)
import           Pos.Core.Types             (AddrAttributes (..), AddrSpendingData (..))
import           Pos.Crypto                 (emptyPassphrase, encToPublic,
                                             fullPublicKeyHexF, hashHexF, noPassEncrypt,
                                             safeCreatePsk, withSafeSigner)
import           Pos.DB.Class               (MonadGState (..))
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util.CompileInfo       (HasCompileInfo)
import           Pos.Util.UserSecret        (WalletUserSecret (..), readUserSecret,
                                             usKeys, usWallet, userSecret)

import           Command.Help               (helpMessage)
import qualified Command.Parser             as OldP
import qualified Command.Tx                 as Tx
import qualified Command.Update             as Update
import           Command.Types              (PrintAction)
import           Lang.Argument              (TypeName, getArg)
import           Lang.Command               (CommandProc (..))
import           Lang.Value                 (Value (..), _ValueAddress, _ValueFilePath)
import           Mode                       (AuxxMode, CmdCtx (..), deriveHDAddressAuxx,
                                             getCmdCtx, makePubKeyAddressAuxx)

createCommandProcs ::
       (HasConfigurations, HasCompileInfo)
    => PrintAction AuxxMode
    -> SendActions AuxxMode
    -> [CommandProc AuxxMode]
createCommandProcs printAction sendActions = [
    CommandProc
    { cpName = "balance"
    , cpArgumentConsumer = getArg tyAddress "addr"
    , cpExec = \addr -> do
        balance <- getBalance addr
        printAction $ sformat ("Current balance: "%coinF) balance
        return ValueUnit
    },

    CommandProc
    { cpName = "print-bvd"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> do
        bvd <- gsAdoptedBVData
        printAction $ pretty bvd
        return ValueUnit
    },

    -- FIXME: implement
    CommandProc
    { cpName = "send-to-all-genesis"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> return ValueUnit
    },

    CommandProc
    { cpName = "send-from-file"
    , cpArgumentConsumer = getArg tyFilePath "file"
    , cpExec = \filePath -> do
        Tx.sendTxsFromFile sendActions filePath
        return ValueUnit
    },

    -- FIXME: implement
    CommandProc
    { cpName = "send"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> return ValueUnit
    },

    -- FIXME: implement
    CommandProc
    { cpName = "vote"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> return ValueUnit
    },

    -- FIXME: implement
    CommandProc
    { cpName = "propose-update"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> return ValueUnit
    },

    -- FIXME: implement
    CommandProc
    { cpName = "propose-unlock-stake-epoch"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> return ValueUnit
    },

    CommandProc
    { cpName = "hash-installer"
    , cpArgumentConsumer = getArg tyFilePath "file"
    , cpExec = \filePath -> do
        Update.hashInstaller filePath
        return ValueUnit
    },

    -- FIXME: implement
    CommandProc
    { cpName = "delegate-light"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> return ValueUnit
    },

    -- FIXME: implement
    CommandProc
    { cpName = "delegate-heavy"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> return ValueUnit
    },

    -- FIXME: implement
    CommandProc
    { cpName = "generate-blocks"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> return ValueUnit
    },

    -- FIXME: implement
    CommandProc
    { cpName = "add-key-pool"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> return ValueUnit
    },

    CommandProc
    { cpName = "add-key"
    , cpArgumentConsumer = getArg tyFilePath "file"
    , cpExec = \filePath -> do
        secret <- readUserSecret filePath
        mapM_ addSecretKey $ secret ^. usKeys
        return ValueUnit
    },

    -- FIXME: implement
    CommandProc
    { cpName = "addr-distr"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> return ValueUnit
    },

    -- FIXME: implement
    CommandProc
    { cpName = "rollback"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> return ValueUnit
    },

    -- FIXME: implement
    CommandProc
    { cpName = "listaddr"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> do
        let toBase58Text = decodeUtf8 . encodeBase58 bitcoinAlphabet . serialize'
        sks <- getSecretKeysPlain
        printAction "Available addresses:"
        for_ (zip [0 :: Int ..] sks) $ \(i, sk) -> do
            let pk = encToPublic sk
            addr <- makePubKeyAddressAuxx pk
            addrHD <- deriveHDAddressAuxx sk
            printAction $
                sformat ("    #"%int%":   addr:      "%build%"\n"%
                         "          pk base58: "%stext%"\n"%
                         "          pk hex:    "%fullPublicKeyHexF%"\n"%
                         "          pk hash:   "%hashHexF%"\n"%
                         "          HD addr:   "%build)
                    i addr (toBase58Text pk) pk (addressHash pk) addrHD
        walletMB <- (^. usWallet) <$> (view userSecret >>= atomically . readTVar)
        whenJust walletMB $ \wallet -> do
            addrHD <- deriveHDAddressAuxx (_wusRootKey wallet)
            printAction $
                sformat ("    Wallet address:\n"%
                         "          HD addr:   "%build)
                    addrHD
        return ValueUnit
    },

    CommandProc
    { cpName = "help"
    , cpArgumentConsumer = do pure ()
    , cpExec = \() -> do
        printAction helpMessage
        return ValueUnit
    }]

type Ty a = (TypeName, Value -> Maybe a)

tyAddress :: Ty Address
tyAddress = ("Address", preview _ValueAddress)

tyFilePath :: Ty FilePath
tyFilePath = ("FilePath", preview _ValueFilePath)
