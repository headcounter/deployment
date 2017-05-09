-- | Parsing and validation of IP addresses.
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveGeneric #-}
module Nexus.DNS.Types.IpAddr
    ( IPv4
    , mkIPv4
    , ip4toByteString
    , mkPublicIPv4
    , isPublicIPv4

    , IPv6
    , mkIPv6
    , ip6toByteString
    , mkPublicIPv6
    , isPublicIPv6
    ) where

import Control.Monad (mfilter)
import Data.ByteString.Char8 (ByteString, unpack, pack)
import Data.Data (Data(..))
import Data.String (IsString(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

import qualified Data.IP as IP
import qualified Data.SafeCopy as SC
import qualified Data.Serialize as S

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

-- | Parse an 'IPv4' address from the given 'ByteString'.
mkIPv4 :: ByteString -> Maybe IPv4
mkIPv4 = fmap IPv4 . readMaybe . unpack

-- | Convert an 'IPv4' to a 'ByteString'.
ip4toByteString :: IPv4 -> ByteString
ip4toByteString (IPv4 i) = pack $ show i

-- | Parse an 'IPv4' address like 'mkIPv4' and return Nothing if it's not a
--   public address ('isPublicIPv4').
mkPublicIPv4 :: ByteString -> Maybe IPv4
mkPublicIPv4 = mfilter isPublicIPv4 . mkIPv4

nonPublicIPv4Ranges :: [IP.AddrRange IP.IPv4]
nonPublicIPv4Ranges = map read
    -- From https://github.com/houseabsolute/Data-Validate-IP
    [ "127.0.0.0/8"     -- loopback
    , "10.0.0.0/8"      -- private
    , "172.16.0.0/12"   -- private
    , "192.168.0.0/16"  -- private
    , "192.0.2.0/24"    -- test network
    , "198.51.100.0/24" -- test network
    , "203.0.113.0/24"  -- test network
    , "192.88.99.0/24"  -- anycast
    , "224.0.0.0/4"     -- multicast
    , "169.254.0.0/16"  -- link local
    , "0.0.0.0/8"       -- unroutable
    , "100.64.0.0/10"   -- unroutable
    , "192.0.0.0/29"    -- unroutable
    , "198.18.0.0/15"   -- unroutable
    , "240.0.0.0/4"     -- unroutable
    ]

-- | Check whether the given 'IPv4' is a valid public IP address.
isPublicIPv4 :: IPv4 -> Bool
isPublicIPv4 (IPv4 i) = not $ any (IP.isMatchedTo i) nonPublicIPv4Ranges

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

-- | Parse an 'IPv6' address from the given 'ByteString'.
mkIPv6 :: ByteString -> Maybe IPv6
mkIPv6 = fmap IPv6 . readMaybe . unpack

-- | Convert an 'IPv6' to a 'ByteString'.
ip6toByteString :: IPv6 -> ByteString
ip6toByteString (IPv6 i) = pack $ show i

-- | Parse an 'IPv6' address like 'mkIPv6' and return Nothing if it's not a
--   public address ('isPublicIPv6').
mkPublicIPv6 :: ByteString -> Maybe IPv6
mkPublicIPv6 = mfilter isPublicIPv6 . mkIPv6

nonPublicIPv6Ranges :: [IP.AddrRange IP.IPv6]
nonPublicIPv6Ranges = map read
    -- From https://github.com/houseabsolute/Data-Validate-IP
    [ "::1/128"         -- loopback
    , "::/128"          -- unroutable
    , "::ffff:0:0/96"   -- IPv4 mapped
    , "100::/64"        -- discard
    , "2001::/23"       -- special
    , "2001::/32"       -- teredo
    , "2001:10::/28"    -- orchid
    , "2001:db8::/32"   -- documentation
    , "fc00::/7"        -- private
    , "fe80::/10"       -- link local
    , "ff00::/8"        -- multicast
    ]

-- | Check whether the given 'IPv6' is a valid public IP address.
isPublicIPv6 :: IPv6 -> Bool
isPublicIPv6 (IPv6 i) = not $ any (IP.isMatchedTo i) nonPublicIPv6Ranges
