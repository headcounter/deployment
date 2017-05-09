{-# LANGUAGE OverloadedStrings #-}
-- | Module for manipulating/writing DNS zone files.
module Nexus.DNS
    ( module Nexus.DNS.Types

    , mkSOA
    , mkTinySOA
    , mkTempSOA
    , mkRR

    , mkZone
    , mkTinyZone
    , mkTempZone

    , updateRecords

    , renderZone
    , checkZone
    ) where

import Control.Lens ((%~), (^.), (^..))
import Data.ByteString.Char8 (unpack)
import Data.Data (Data(toConstr))
import Data.Function (on)
import Data.List (unionBy)

import Nexus.DNS.Types
import Nexus.DNS.ZoneBuilder

-- | Create a SOA record with some defaults as recommended by
--   <https://www.ripe.net/publications/docs/ripe-203>.
mkSOA :: (Domain a, Domain b) => a -> b -> SOARecord
mkSOA primary email = SOARecord
    { _soaPrimary     = toRRName primary
    , _soaEmail       = toRRName email
    , _soaSerial      = 0
    , _soaRefresh     = 86400
    , _soaRetry       = 7200
    , _soaExpire      = 3600000
    , _soaNXDomainTTL = 172800
    }

-- | Create a SOA record that's tailored for frequently updated zones.
mkTinySOA :: (Domain a, Domain b) => a -> b -> SOARecord
mkTinySOA primary email = (mkSOA primary email)
    { _soaRefresh     = 60
    , _soaRetry       = 60
    , _soaExpire      = 14400
    , _soaNXDomainTTL = 0
    }

-- | Create a SOA record that's tailored for short-lived zones.
mkTempSOA :: (Domain a, Domain b) => a -> b -> SOARecord
mkTempSOA primary email = (mkSOA primary email)
    { _soaRefresh     = 0
    , _soaRetry       = 0
    , _soaExpire      = 0
    , _soaNXDomainTTL = 0
    }

-- | Create a 'ResourceRecord' for the zone's $ORIGIN and with the default TTL.
mkRR :: Record -> ResourceRecord
mkRR record = ResourceRecord
    { _rrName   = Origin
    , _rrTTL    = Nothing
    , _rrRecord = record
    }

mkZoneWith :: (RRName -> a -> SOARecord)
           -> FQDN
           -> a
           -> [ResourceRecord]
           -> Zone
mkZoneWith soaFun fqdn email records = Zone
    { _zoneDomain  = fqdn
    , _zoneTTL     = _soaNXDomainTTL soa
    , _zoneSOA     = soa
    , _zoneRecords = records
    }
  where
    nsRecs = records ^.. traverse . rrRecord . _Nameserver
    primary = head $ nsRecs ++ [mappend (toRRName fqdn) "ns1"]
    soa = soaFun primary email

-- | Create a new 'Zone' with the same default values as in `mkSOA`.
mkZone :: Domain a
       => FQDN             -- ^ The FQDN of the zone
       -> a                -- ^ The email address of the zone owner
       -> [ResourceRecord] -- ^ Initial resource records. The first
                           --   'Nameserver' entry will be used for the primary
                           --   nameserver of the @SOA@ record. If there is no
                           --   such entry, it will be @ns1.FQDN@.
       -> Zone
mkZone = mkZoneWith mkSOA

-- | Alias of mkZone creating a SOA record like in `mkTinySOA`.
mkTinyZone :: Domain a => FQDN -> a -> [ResourceRecord] -> Zone
mkTinyZone = mkZoneWith mkTinySOA

-- | Alias of mkZone creating a SOA record like in `mkTempSOA`.
mkTempZone :: Domain a => FQDN -> a -> [ResourceRecord] -> Zone
mkTempZone = mkZoneWith mkTempSOA

-- | Update the records of a 'Zone' that have the same 'rrName' as the ones
--   specified in the first argument and increment the serial by one.
--
-- If a record doesn't exist in the 'Zone' it's added.
updateRecords :: [ResourceRecord] -> Zone -> Zone
updateRecords newRRs =
    (zoneSOA . soaSerial %~ succ) . (zoneRecords %~ updateRecs)
  where
    updateRecs = unionBy ((==) `on` match) newRRs
    match rr = (_rrName rr, toConstr $ _rrRecord rr)

-- | Check whether the zone and its records are valid.
--
-- Returns 'Nothing' if everything is fine or 'Just' with an error string.
checkZone :: Zone -> Maybe String
checkZone z =
    case nameservers of
         [] -> Just $ "No nameservers found for " ++ show domain ++ "."
         _  -> Nothing
  where
    domain = unpack $ toByteString $ z ^. zoneDomain
    nameservers = z ^. zoneRecords ^.. traverse . rrRecord . _Nameserver
