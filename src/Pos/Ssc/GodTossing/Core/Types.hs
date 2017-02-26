{-# LANGUAGE TypeFamilies    #-}

-- | Core types of GodTossing SSC.

module Pos.Ssc.GodTossing.Core.Types
       (
         -- * Commitments
         Commitment (..)
       , CommitmentSignature
       , SignedCommitment
       , CommitmentsMap (getCommitmentsMap)
       , mkCommitmentsMap
       , mkCommitmentsMapUnsafe

       -- * Openings
       , Opening (..)
       , OpeningsMap

         -- * Shares
       , InnerSharesMap
       , SharesMap
       , SharesDistribution

         -- * Vss certificates
       , VssCertificate (vcVssKey, vcExpiryEpoch, vcSignature, vcSigningKey)
       , mkVssCertificate
       , recreateVssCertificate
       , getCertId
       , VssCertificatesMap
       , mkVssCertificatesMap

       -- * Payload
       , GtPayload (..)
       , GtProof (..)

         -- * Misc
       , NodeSet
       ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, int, (%))
import           Universum

import           Pos.Binary.Class    (AsBinary (..))
import           Pos.Binary.Types    ()
import           Pos.Crypto          (EncShare, Hash, PublicKey, Secret, SecretKey,
                                      SecretProof, SecretSharingExtra, Share, Signature,
                                      VssPublicKey, checkSig, sign, toPublic)
import           Pos.Types.Address   (addressHash)
import           Pos.Types.Core      (EpochIndex, StakeholderId)

type NodeSet = HashSet StakeholderId

----------------------------------------------------------------------------
-- Commitments
----------------------------------------------------------------------------

-- | Commitment is a message generated during the first stage of
-- GodTossing. It contains encrypted shares and proof of secret.
-- Invariant which must be ensured: commShares is not empty.
data Commitment = Commitment
    { commExtra  :: !(AsBinary SecretSharingExtra)
    , commProof  :: !(AsBinary SecretProof)
    , commShares :: !(HashMap (AsBinary VssPublicKey) (NonEmpty (AsBinary EncShare)))
    } deriving (Show, Eq, Generic)

instance NFData Commitment

-- | Signature which ensures that commitment was generated by node
-- with given public key for given epoch.
type CommitmentSignature = Signature (EpochIndex, Commitment)

type SignedCommitment = (PublicKey, Commitment, CommitmentSignature)

-- | 'CommitmentsMap' is a wrapper for 'HashMap StakeholderId SignedCommitment'
-- which ensures that keys are consistent with values, i. e. 'PublicKey'
-- from 'SignedCommitment' corresponds to key which is 'StakeholderId'.
newtype CommitmentsMap = CommitmentsMap
    { getCommitmentsMap :: HashMap StakeholderId SignedCommitment
    } deriving (Semigroup, Monoid, Show, Eq, Container, NFData)

type instance Element CommitmentsMap = SignedCommitment

-- | Safe constructor of 'CommitmentsMap'.
mkCommitmentsMap :: [SignedCommitment] -> CommitmentsMap
mkCommitmentsMap = CommitmentsMap . HM.fromList . map toCommPair
  where
    toCommPair signedComm@(pk, _, _) = (addressHash pk, signedComm)

-- | Unsafe straightforward constructor of 'CommitmentsMap'.
mkCommitmentsMapUnsafe :: HashMap StakeholderId SignedCommitment
                       -> CommitmentsMap
mkCommitmentsMapUnsafe = CommitmentsMap

----------------------------------------------------------------------------
-- Openings
----------------------------------------------------------------------------

-- | Opening reveals secret.
newtype Opening = Opening
    { getOpening :: (AsBinary Secret)
    } deriving (Show, Eq, Generic, Buildable, NFData)

type OpeningsMap = HashMap StakeholderId Opening

----------------------------------------------------------------------------
-- Shares
----------------------------------------------------------------------------

-- | Each node generates several 'SharedSeed's, breaks every 'SharedSeed' into 'Share's,
-- and sends those encrypted shares to other nodes
-- (for i-th commitment at i-th element of NonEmpty list)
-- In a 'SharesMap', for each node we collect shares which said node has
-- received and decrypted.
--
-- Specifically, if node identified by 'Address' X has received NonEmpty list of shares
-- from node identified by key Y,
-- this NonEmpty list will be at @sharesMap ! X ! Y@.

type InnerSharesMap = HashMap StakeholderId (NonEmpty (AsBinary Share))

type SharesMap = HashMap StakeholderId InnerSharesMap

type SharesDistribution = HashMap StakeholderId Word16

instance Buildable (StakeholderId, Word16) where
    build (id, c) = bprint ("("%build%": "%build%" shares)") id c

----------------------------------------------------------------------------
-- Vss certificates
----------------------------------------------------------------------------

-- | VssCertificate allows VssPublicKey to participate in MPC.
-- Each stakeholder should create a Vss keypair, sign VSS public key with signing
-- key and send it into blockchain.
--
-- A public key of node is included in certificate in order to
-- enable validation of it using only node's P2PKH address.
-- Expiry epoch is last epoch when certificate is valid, expiry epoch is included
-- in certificate and signature.
--
-- Other nodes accept this certificate if it is valid and if node has
-- enough stake.
--
-- Invariant: 'checkSig vcSigningKey (vcVssKey, vcExpiryEpoch) vcSignature'.
data VssCertificate = VssCertificate
    { vcVssKey      :: !(AsBinary VssPublicKey)
    , vcExpiryEpoch :: !EpochIndex
    -- ^ Epoch up to which certificates is valid.
    , vcSignature   :: !(Signature (AsBinary VssPublicKey, EpochIndex))
    , vcSigningKey  :: !PublicKey
    } deriving (Show, Eq, Generic)

instance NFData VssCertificate

instance Ord VssCertificate where
    compare a b = toTuple a `compare` toTuple b
      where
        toTuple VssCertificate {..} =
            (vcExpiryEpoch, vcVssKey, vcSigningKey, vcSignature)

instance Buildable VssCertificate where
    build VssCertificate {..} = bprint
        ("vssCert:"%build%":"%int) vcSigningKey vcExpiryEpoch

-- | Make VssCertificate valid up to given epoch using 'SecretKey' to sign data.
mkVssCertificate :: SecretKey -> AsBinary VssPublicKey -> EpochIndex -> VssCertificate
mkVssCertificate sk vk expiry =
    VssCertificate vk expiry (sign sk (vk, expiry)) $ toPublic sk

-- | Recreate 'VssCertificate' from its contents. This function main
-- 'fail' if data is invalid.
recreateVssCertificate
    :: MonadFail m
    => AsBinary VssPublicKey
    -> EpochIndex
    -> Signature (AsBinary VssPublicKey, EpochIndex)
    -> PublicKey
    -> m VssCertificate
recreateVssCertificate vssKey epoch sig pk =
    res <$
    (unless (checkCertSign res) $ fail "recreateVssCertificate: invalid sign")
  where
    res =
        VssCertificate
        { vcVssKey = vssKey
        , vcExpiryEpoch = epoch
        , vcSignature = sig
        , vcSigningKey = pk
        }

-- CHECK: @checkCertSign
-- | Check that the VSS certificate is signed properly
-- #checkPubKeyAddress
-- #checkSig
checkCertSign :: VssCertificate -> Bool
checkCertSign VssCertificate {..} =
    checkSig vcSigningKey (vcVssKey, vcExpiryEpoch) vcSignature

getCertId :: VssCertificate -> StakeholderId
getCertId = addressHash . vcSigningKey

-- | VssCertificatesMap contains all valid certificates collected
-- during some period of time.
type VssCertificatesMap = HashMap StakeholderId VssCertificate

-- | Safe constructor of 'VssCertificatesMap'. TODO: wrap into newtype.
mkVssCertificatesMap :: [VssCertificate] -> VssCertificatesMap
mkVssCertificatesMap = HM.fromList . map toCertPair
  where
    toCertPair vc = (getCertId vc, vc)

----------------------------------------------------------------------------
-- Payload and proof
----------------------------------------------------------------------------

-- | Payload included into blocks.
data GtPayload
    = CommitmentsPayload  !CommitmentsMap !VssCertificatesMap
    | OpeningsPayload     !OpeningsMap    !VssCertificatesMap
    | SharesPayload       !SharesMap      !VssCertificatesMap
    | CertificatesPayload !VssCertificatesMap
    deriving (Eq, Show, Generic)

-- | Proof of GtPayload.
data GtProof
    = CommitmentsProof !(Hash CommitmentsMap) !(Hash VssCertificatesMap)
    | OpeningsProof !(Hash OpeningsMap) !(Hash VssCertificatesMap)
    | SharesProof !(Hash SharesMap) !(Hash VssCertificatesMap)
    | CertificatesProof !(Hash VssCertificatesMap)
    deriving (Show, Eq, Generic)

instance NFData GtPayload
instance NFData GtProof
