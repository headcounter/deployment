-- | A module for rendering 'Zone' data.
module Nexus.DNS.ZoneBuilder (renderZone) where

import Data.ByteString.Builder
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8Builder)

import Nexus.DNS.Types

renderRType :: String -> [Builder] -> Builder
renderRType _ []     = mempty
renderRType t (b:bs) = string7 t <> char7 ' ' <> b
                    <> mconcat (mappend (char7 ' ') <$> bs)
                    <> char7 '\n'

renderSOA :: SOARecord -> Builder
renderSOA soa = string7 "@ IN " <> renderRType "SOA"
    [ renderDomain             $ _soaPrimary soa
    , renderDomain             $ _soaEmail soa
    , word32Dec                $ _soaSerial soa
    , word32Dec . fromIntegral $ _soaRefresh soa
    , word32Dec . fromIntegral $ _soaRetry soa
    , word32Dec . fromIntegral $ _soaExpire soa
    , word32Dec                $ _soaNXDomainTTL soa
    ]

renderRecord :: Record -> Builder
renderRecord (IPv4Address ip) =
    renderRType "A" [byteString $ ip4toByteString ip]
renderRecord (IPv6Address ip) =
    renderRType "AAAA" [byteString $ ip6toByteString ip]
renderRecord (TextRecord text) =
    renderRType "TXT" [encodeUtf8Builder text]
renderRecord (CanonicalName domain) =
    renderRType "CNAME" [renderDomain domain]
renderRecord (DelegationName domain) =
    renderRType "DNAME" [renderDomain domain]
renderRecord (MailExchange pref domain) =
    renderRType "MX" [word16Dec pref, renderDomain domain]
renderRecord (Nameserver domain) =
    renderRType "NS" [renderDomain domain]
renderRecord (Pointer domain) =
    renderRType "PTR" [renderDomain domain]
renderRecord (ServiceLocator prio weight port domain) =
    renderRType "SRV" [ word16Dec prio, word16Dec weight, word16Dec port
                      , renderDomain domain
                      ]

renderRR :: ResourceRecord -> Builder
renderRR (ResourceRecord domain ttl record)
     = renderDomain domain
    <> maybe mempty (mappend (char7 ' ') . word32Dec) ttl
    <> string7 " IN "
    <> renderRecord record

-- | Render a zone file of the specified 'Zone' and return it as a 'Builder'.
renderZone :: Zone -> Builder
renderZone z = string7 "$TTL " <> word32Dec (_zoneTTL z) <> char7 '\n'
            <> renderSOA (_zoneSOA z)
            <> mconcat (fmap renderRR (_zoneRecords z))
