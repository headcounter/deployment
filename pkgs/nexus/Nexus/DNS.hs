-- | Module for manipulating/writing DNS zone files.
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Nexus.DNS
    ( Domain(..)
    , IPv4
    , IPv6

    , Record(..)
    , ResourceRecord(..)
    , SOARecord(..)
    , Zone(..)

    , mkSOA
    , mkTinySOA
    , mkRR
    ) where

import Data.Maybe (catMaybes)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)

import qualified Data.IP as IP
import qualified Data.SafeCopy as SC
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Nexus.DNS.NatInt32
import Nexus.DNS.DomainName

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
        { zoneDomain  = migrateFQDN $ zone_v0_FQDN oldZone
        , zoneSOA     = SOARecord
            { soaPrimary     = head nameservers
            , soaEmail       = email
            , soaSerial      = zone_v0_Serial oldZone
            , soaRefresh     = 60
            , soaRetry       = 60
            , soaExpire      = 14400
            , soaNXDomainTTL = 0
            }
        , zoneRecords = fmap mkRR $
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

-- | This represents a zone file typically used in BIND or NSD.
data Zone = Zone
    { zoneDomain  :: DomainName       -- ^ The FQDN of the zone file
    , zoneSOA     :: SOARecord        -- ^ The Start of Authority record
    , zoneRecords :: [ResourceRecord] -- ^ All the resource records of the zone
    } deriving (Show, Typeable, Generic)

-- | A domain reference as used in a zone file.
data Domain
    = FullDomain DomainName -- ^ A fully qualified domain name
    | SubDomain DomainName  -- ^ A domain relative to the 'Origin'
    | Origin                -- ^ The root of the current zone
    deriving (Show, Typeable, Generic)

-- | An IP version 4 address
newtype IPv4 = IPv4 { unIPv4 :: IP.IPv4 }
    deriving (Show, Eq, Bounded, Enum, Typeable, Generic)

instance SC.SafeCopy IPv4 where
    putCopy = SC.contain . SC.safePut . IP.fromIPv4 . unIPv4
    getCopy = SC.contain $ fmap (IPv4 . IP.toIPv4) SC.safeGet

instance S.Serialize IPv4 where
    put = S.put . IP.fromIPv4 . unIPv4
    get = fmap (IPv4 . IP.toIPv4) S.get

-- | An IP version 6 address
newtype IPv6 = IPv6 { unIPv6 :: IP.IPv6 }
    deriving (Show, Eq, Bounded, Enum, Typeable, Generic)

instance SC.SafeCopy IPv6 where
    putCopy = SC.contain . SC.safePut . IP.fromIPv6 . unIPv6
    getCopy = SC.contain $ fmap (IPv6 . IP.toIPv6) SC.safeGet

instance S.Serialize IPv6 where
    put = S.put . IP.fromIPv6 . unIPv6
    get = fmap (IPv6 . IP.toIPv6) S.get

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
    deriving (Show, Typeable, Generic)

-- | A DNS resource record with the most common fields.
--
-- The record class is not included here, because we're only interested in
-- Internet (@IN@) records.
--
-- The record type and data are both encoded in 'Record'.
data ResourceRecord = ResourceRecord
    { rrName  :: Domain       -- ^ The resource record name
    , rrTTL   :: Maybe Word32 -- ^ The time-to-live in seconds or the zone-wide
                              --   default if 'Nothing'
    , rrRecord :: Record      -- ^ The record type and data
    } deriving (Show, Typeable, Generic)

-- | The Start of Authority record.
data SOARecord = SOARecord
    { soaPrimary     :: DomainName -- ^ The primary master of the 'Zone'
    , soaEmail       :: DomainName -- ^ Mail address of the one responsible
    , soaSerial      :: Word32     -- ^ The serial number of the 'Zone'
    , soaRefresh     :: NatInt32   -- ^ The time interval before the 'Zone'
                                   --   should be refreshed
    , soaRetry       :: NatInt32   -- ^ The time interval that should elapse
                                   --   before a failed refresh should be
                                   --   retried
    , soaExpire      :: NatInt32   -- ^ The upper limit on the time interval
                                   --   that can elapse before the zone is no
                                   --   longer authoritative
    , soaNXDomainTTL :: Word32     -- ^ The caching time for NXDOMAIN errors
    } deriving (Show, Typeable, Generic)

-- | Create a SOA record with some defaults as recommended by
--   <https://www.ripe.net/publications/docs/ripe-203>.
mkSOA :: DomainName -> DomainName -> SOARecord
mkSOA primary email = SOARecord
    { soaPrimary     = primary
    , soaEmail       = email
    , soaSerial      = 0
    , soaRefresh     = 86400
    , soaRetry       = 7200
    , soaExpire      = 3600000
    , soaNXDomainTTL = 172800
    }

-- | Create a SOA record that's tailored for short-lived or frequently updated
--   zones.
mkTinySOA :: DomainName -> DomainName -> SOARecord
mkTinySOA primary email = (mkSOA primary email)
    { soaRefresh     = 60
    , soaRetry       = 60
    , soaExpire      = 14400
    , soaNXDomainTTL = 0
    }

-- | Create a 'ResourceRecord' for the zone's $ORIGIN and with the default TTL.
mkRR :: Record -> ResourceRecord
mkRR record = ResourceRecord
    { rrName   = Origin
    , rrTTL    = Nothing
    , rrRecord = record
    }

SC.deriveSafeCopy 0 'SC.base ''Domain
SC.deriveSafeCopy 0 'SC.base ''Record
SC.deriveSafeCopy 0 'SC.base ''ResourceRecord
SC.deriveSafeCopy 0 'SC.base ''SOARecord
SC.deriveSafeCopy 0 'SC.base ''Zone_v0
SC.deriveSafeCopy 1 'SC.extension ''Zone
