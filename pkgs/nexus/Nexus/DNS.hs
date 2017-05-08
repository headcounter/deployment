-- | Module for manipulating/writing DNS zone files.
module Nexus.DNS
    ( module Nexus.DNS.Types

    , mkSOA
    , mkTinySOA
    , mkRR

    , updateRecords
    ) where

import Control.Lens ((%~))
import Data.Data (Data(toConstr))
import Data.Function (on)
import Data.List (unionBy)

import Nexus.DNS.Types

-- | Create a SOA record with some defaults as recommended by
--   <https://www.ripe.net/publications/docs/ripe-203>.
mkSOA :: DomainName -> DomainName -> SOARecord
mkSOA primary email = SOARecord
    { _soaPrimary     = primary
    , _soaEmail       = email
    , _soaSerial      = 0
    , _soaRefresh     = 86400
    , _soaRetry       = 7200
    , _soaExpire      = 3600000
    , _soaNXDomainTTL = 172800
    }

-- | Create a SOA record that's tailored for short-lived or frequently updated
--   zones.
mkTinySOA :: DomainName -> DomainName -> SOARecord
mkTinySOA primary email = (mkSOA primary email)
    { _soaRefresh     = 60
    , _soaRetry       = 60
    , _soaExpire      = 14400
    , _soaNXDomainTTL = 0
    }

-- | Create a 'ResourceRecord' for the zone's $ORIGIN and with the default TTL.
mkRR :: Record -> ResourceRecord
mkRR record = ResourceRecord
    { _rrName   = Origin
    , _rrTTL    = Nothing
    , _rrRecord = record
    }

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
