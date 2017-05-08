-- | Module for manipulating/writing DNS zone files.
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Nexus.DNS.Types
    ( Domain(..)
    , IPv4
    , IPv6
    , NatInt32
    , DomainName

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
import Data.String (IsString(..))
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)

import qualified Data.IP as IP
import qualified Data.SafeCopy as SC
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Nexus.DNS.Types.NatInt32
import Nexus.DNS.Types.DomainName

-- | This represents a zone file typically used in BIND or NSD.
data Zone = Zone
    { _zoneDomain  :: DomainName       -- ^ The FQDN of the zone file
    , _zoneTTL     :: Word32           -- ^ The default time-to-live in seconds
    , _zoneSOA     :: SOARecord        -- ^ The Start of Authority record
    , _zoneRecords :: [ResourceRecord] -- ^ All the resource records of the zone
    } deriving (Show, Typeable, Generic)

-- | A domain reference as used in a zone file.
data Domain
    = FullDomain DomainName -- ^ A fully qualified domain name
    | SubDomain DomainName  -- ^ A domain relative to the 'Origin'
    | Origin                -- ^ The root of the current zone
    deriving (Show, Eq, Data, Typeable, Generic)

-- | An IP version 4 address
newtype IPv4 = IPv4 IP.IPv4
    deriving (Show, Eq, Bounded, Enum, Data, Typeable, Generic)

instance SC.SafeCopy IPv4 where
    putCopy (IPv4 i) = SC.contain . SC.safePut $ IP.fromIPv4 i
    getCopy = SC.contain $ fmap (IPv4 . IP.toIPv4) SC.safeGet

instance S.Serialize IPv4 where
    put (IPv4 i) = S.put $ IP.fromIPv4 i
    get = fmap (IPv4 . IP.toIPv4) S.get

instance IsString IPv4 where
    fromString = IPv4 . fromString

-- | An IP version 6 address
newtype IPv6 = IPv6 IP.IPv6
    deriving (Show, Eq, Bounded, Enum, Data, Typeable, Generic)

instance SC.SafeCopy IPv6 where
    putCopy (IPv6 i) = SC.contain . SC.safePut $ IP.fromIPv6 i
    getCopy = SC.contain $ fmap (IPv6 . IP.toIPv6) SC.safeGet

instance S.Serialize IPv6 where
    put (IPv6 i) = S.put $ IP.fromIPv6 i
    get = fmap (IPv6 . IP.toIPv6) S.get

instance IsString IPv6 where
    fromString = IPv6 . fromString

-- | The DNS record type and its data as an ADT.
data Record
    = IPv4Address    IPv4                        -- ^ @A@ record
    | IPv6Address    IPv6                        -- ^ @AAAA@ record
    | TextRecord     T.Text                      -- ^ @TXT@ record
    | CanonicalName  Domain                      -- ^ @CNAME@ record
    | DelegationName Domain                      -- ^ @DNAME@ record
    | MailExchange   Word16 Domain               -- ^ @MX@ record
    | Nameserver     Domain                      -- ^ @NS@ record
    | Pointer        Domain                      -- ^ @PTR@ record
    | ServiceLocator Word16 Word16 Word16 Domain -- ^ @SRV@ record
    deriving (Show, Data, Typeable, Generic)

-- | A DNS resource record with the most common fields.
--
-- The record class is not included here, because we're only interested in
-- Internet (@IN@) records.
--
-- The record type and data are both encoded in 'Record'.
data ResourceRecord = ResourceRecord
    { _rrName  :: Domain       -- ^ The resource record name
    , _rrTTL   :: Maybe Word32 -- ^ The time-to-live in seconds or the zone-wide
                               --   default if 'Nothing'
    , _rrRecord :: Record      -- ^ The record type and data
    } deriving (Show, Typeable, Generic)

-- | The Start of Authority record.
data SOARecord = SOARecord
    { _soaPrimary     :: DomainName -- ^ The primary master of the 'Zone'
    , _soaEmail       :: DomainName -- ^ Mail address of the one responsible
    , _soaSerial      :: Word32     -- ^ The serial number of the 'Zone'
    , _soaRefresh     :: NatInt32   -- ^ The time interval before the 'Zone'
                                    --   should be refreshed
    , _soaRetry       :: NatInt32   -- ^ The time interval that should elapse
                                    --   before a failed refresh should be
                                    --   retried
    , _soaExpire      :: NatInt32   -- ^ The upper limit on the time interval
                                    --   that can elapse before the zone is no
                                    --   longer authoritative
    , _soaNXDomainTTL :: Word32     -- ^ The caching time for NXDOMAIN errors
    } deriving (Show, Typeable, Generic)

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
            { _soaPrimary     = head nameservers
            , _soaEmail       = email
            , _soaSerial      = zone_v0_Serial oldZone
            , _soaRefresh     = 60
            , _soaRetry       = 60
            , _soaExpire      = 14400
            , _soaNXDomainTTL = 0
            }
        , _zoneRecords = fmap (ResourceRecord Origin Nothing) $
            (Nameserver . FullDomain <$> nameservers) ++ addrRRs
        }
      where
        oldFqdnToBS = TE.encodeUtf8 . T.intercalate (T.singleton '.')
        migrateFQDN = either error id . fromByteString . oldFqdnToBS
        nameservers = migrateFQDN <$> zone_v0_Nameservers oldZone
        email = migrateFQDN $ zone_v0_Email oldZone
        addrRRs = catMaybes [ IPv4Address <$> zone_v0_IPv4Address oldZone
                            , IPv6Address <$> zone_v0_IPv6Address oldZone
                            ]

SC.deriveSafeCopy 0 'SC.base ''Domain
SC.deriveSafeCopy 0 'SC.base ''Record
SC.deriveSafeCopy 0 'SC.base ''ResourceRecord
SC.deriveSafeCopy 0 'SC.base ''SOARecord
SC.deriveSafeCopy 0 'SC.base ''Zone_v0
SC.deriveSafeCopy 1 'SC.extension ''Zone
