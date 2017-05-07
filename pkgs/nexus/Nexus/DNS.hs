-- | Module for manipulating/writing DNS zone files.
module Nexus.DNS
    ( module Nexus.DNS.Types

    , mkSOA
    , mkTinySOA
    , mkRR

    , updateRecords
    ) where

import Data.Data (Data(toConstr))
import Data.Function (on)
import Data.List (unionBy)

import Nexus.DNS.Types

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

-- | Update the records of a 'Zone' that have the same 'rrName' as the ones
--   specified in the first argument and increment the serial by one.
--
-- If a record doesn't exist in the 'Zone' it's added.
updateRecords :: [ResourceRecord] -> Zone -> Zone
updateRecords newRRs zone = zone
    { zoneSOA = (zoneSOA zone) { soaSerial = soaSerial (zoneSOA zone) + 1 }
    , zoneRecords = unionBy ((==) `on` match) newRRs $ zoneRecords zone
    }
  where
    match rr = (rrName rr, toConstr $ rrRecord rr)
