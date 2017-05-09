-- | Module for manipulating/writing DNS zone files.
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveAnyClass #-}
module Nexus.DNS.Types
    ( module Nexus.DNS.Types.Domain
    , module Nexus.DNS.Types.IpAddr
    , module Nexus.DNS.Types.NatInt32

    , Record(..)
    , AsRecord(..)

    , ResourceRecord(..)
    , HasResourceRecord(..)

    , SOARecord(..)
    , HasSOARecord(..)

    , Zone(..)
    , HasZone(..)
    ) where

import Control.Lens (makeClassy, makeClassyPrisms)
import Data.Data (Data(..))
import Data.Maybe (catMaybes)
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)

import qualified Data.SafeCopy as SC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Nexus.DNS.Types.Domain
import Nexus.DNS.Types.IpAddr
import Nexus.DNS.Types.NatInt32

-- | This represents a zone file typically used in BIND or NSD.
data Zone = Zone
    { _zoneDomain  :: FQDN             -- ^ The FQDN of the zone file
    , _zoneTTL     :: Word32           -- ^ The default time-to-live in seconds
    , _zoneSOA     :: SOARecord        -- ^ The Start of Authority record
    , _zoneRecords :: [ResourceRecord] -- ^ All the resource records of the zone
    } deriving (Show, Serialize, Typeable, Generic)

-- | The DNS record type and its data as an ADT.
data Record
    = IPv4Address    IPv4                        -- ^ @A@ record
    | IPv6Address    IPv6                        -- ^ @AAAA@ record
    | TextRecord     T.Text                      -- ^ @TXT@ record
    | CanonicalName  RRName                      -- ^ @CNAME@ record
    | DelegationName RRName                      -- ^ @DNAME@ record
    | MailExchange   Word16 RRName               -- ^ @MX@ record
    | Nameserver     RRName                      -- ^ @NS@ record
    | Pointer        RRName                      -- ^ @PTR@ record
    | ServiceLocator Word16 Word16 Word16 RRName -- ^ @SRV@ record
    deriving (Show, Data, Serialize, Typeable, Generic)

-- | A DNS resource record with the most common fields.
--
-- The record class is not included here, because we're only interested in
-- Internet (@IN@) records.
--
-- The record type and data are both encoded in 'Record'.
data ResourceRecord = ResourceRecord
    { _rrName  :: RRName       -- ^ The resource record name
    , _rrTTL   :: Maybe Word32 -- ^ The time-to-live in seconds or the zone-wide
                               --   default if 'Nothing'
    , _rrRecord :: Record      -- ^ The record type and data
    } deriving (Show, Serialize, Typeable, Generic)

-- | The Start of Authority record.
data SOARecord = SOARecord {
    -- | The primary master of the 'Zone'
    _soaPrimary     :: RRName,
    -- | Mail address of the one responsible
    _soaEmail       :: RRName,
    -- | The serial number of the 'Zone'
    _soaSerial      :: Word32,
    -- | The time interval before the 'Zone' should be refreshed
    _soaRefresh     :: NatInt32,
    -- | The time interval that should elapse before a failed refresh should be
    --   retried
    _soaRetry       :: NatInt32,
    -- | The upper limit on the time interval that can elapse before the zone
    --   is no longer authoritative
    _soaExpire      :: NatInt32,
    -- | The caching time for NXDOMAIN errors
    _soaNXDomainTTL :: Word32
} deriving (Show, Serialize, Typeable, Generic)

-- | Lenses for 'Zone'
makeClassy ''Zone

-- | Lenses for 'ResourceRecord'
makeClassy ''ResourceRecord

-- | Lenses for 'SOARecord'
makeClassy ''SOARecord

-- | Prisms for 'Record'
makeClassyPrisms ''Record

-- | Obsolete version of 'Zone' for safecopy migrations.
data Zone_v0 = Zone_v0
    { zone_v0_FQDN :: [T.Text]
    , zone_v0_Email :: [T.Text]
    , zone_v0_Nameservers :: [[T.Text]]
    , zone_v0_Serial :: Word32
    , zone_v0_IPv4Address :: Maybe IPv4
    , zone_v0_IPv6Address :: Maybe IPv6
    } deriving (Typeable, Generic)

instance SC.Migrate Zone where
    type MigrateFrom Zone = Zone_v0
    migrate oldZone = Zone
        { _zoneDomain  = migrateFQDN $ zone_v0_FQDN oldZone
        , _zoneTTL     = 0
        , _zoneSOA     = SOARecord
            { _soaPrimary     = toRRName $ head nameservers
            , _soaEmail       = toRRName email
            , _soaSerial      = zone_v0_Serial oldZone
            , _soaRefresh     = 60
            , _soaRetry       = 60
            , _soaExpire      = 14400
            , _soaNXDomainTTL = 0
            }
        , _zoneRecords = fmap (ResourceRecord Origin Nothing) $
            (Nameserver . toRRName <$> nameservers) ++ addrRRs
        }
      where
        oldFqdnToBS = TE.encodeUtf8 . T.intercalate (T.singleton '.')
        migrateFQDN = either error id . parseDomain . oldFqdnToBS
        nameservers = migrateFQDN <$> zone_v0_Nameservers oldZone
        email = migrateFQDN $ zone_v0_Email oldZone
        addrRRs = catMaybes [ IPv4Address <$> zone_v0_IPv4Address oldZone
                            , IPv6Address <$> zone_v0_IPv6Address oldZone
                            ]

SC.deriveSafeCopy 0 'SC.base ''Record
SC.deriveSafeCopy 0 'SC.base ''ResourceRecord
SC.deriveSafeCopy 0 'SC.base ''SOARecord
SC.deriveSafeCopy 0 'SC.base ''Zone_v0
SC.deriveSafeCopy 1 'SC.extension ''Zone
