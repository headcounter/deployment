-- | Data type and utility functions for manipulating (fully qualified) domain
--   names.
{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving, DeriveAnyClass, OverloadedStrings #-}
module Nexus.DNS.Types.Domain
    ( FQDN
    , RRName(Origin)
    , Domain(..)

    , emailToFQDN
    ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString, char7)
import Data.Char (toLower)
import Data.Data (Data)
import Data.List (isSuffixOf)
import Data.Maybe (isJust)
import Data.Semigroup (Semigroup(..))
import Data.Serialize (Serialize)
import Data.String (IsString(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import qualified Data.SafeCopy as SC
import qualified Data.ByteString.Char8 as BC

-- | Class representing a domain, which is either a fully qualified domain name
--   or various other representations typically used in a zone file.
class (SC.SafeCopy a, Serialize a, IsString a, Monoid a) => Domain a where
    -- | Parse a 'ByteString' into the internal representation of 'Domain'
    --
    -- Returns 'Left' with an error string if it's not a valid domain name.
    parseDomain :: ByteString -> Either String a

    -- | Convert a 'Domain' to a 'Builder' for use in a zone file.
    renderDomain :: a -> Builder

    -- | Convert a 'Domain' to a 'ByteString'.
    toByteString :: a -> ByteString

    -- | Reverse the labels of a 'Domain'.
    reverseDomain :: a -> a

    -- | Retrieve the subdomain portions of a particular root domain.
    --
    -- For example (with @-XOverloadedStrings@):
    --
    -- >>> "foo.bar.example.org" `subDomainOf` "example.org"
    -- Just "foo.bar"
    subDomainOf :: a -> FQDN -> Maybe a

    -- | Similar to 'subDomainOf' but just returns a boolean instead of a
    --   'Maybe'.
    isSubDomainOf :: a -> FQDN -> Bool
    isSubDomainOf x = isJust . subDomainOf x

    -- | Convert a 'Domain' to a 'FQDN' with the root 'FQDN' in the second
    --   argument.
    toFQDN :: a -> FQDN -> FQDN

    -- | Convert a 'Domain' to a 'RRName'.
    toRRName :: a -> RRName

-- | A fully qualified domain name.
newtype FQDN = FQDN [ByteString]
    deriving (Eq, Show, Ord, Data, Typeable, Generic)

deriving instance Serialize FQDN

instance IsString FQDN where
    fromString = either error id . parseDomain . BC.pack

instance Monoid FQDN where
    mempty = FQDN []

    mappend (FQDN a) (FQDN b) = FQDN $ b `mappend` a

instance Semigroup FQDN where
    (<>) = mappend
    stimes n (FQDN x) = FQDN $ stimes n x

instance Domain FQDN where
    parseDomain = parseOnly (domain <* endOfInput)

    renderDomain x = byteString $ BC.snoc (toByteString x) '.'

    toByteString (FQDN x) = BC.intercalate (BC.singleton '.') x

    reverseDomain (FQDN x) = FQDN $ reverse x

    subDomainOf (FQDN x) (FQDN root)
        | not $ root `isSuffixOf` x = Nothing
        | otherwise = Just . FQDN $ take (length x - length root) x

    toFQDN = const
    toRRName (FQDN x) = Absolute $ fmap encodeRRPart x

data RRPart = Wildcard | Label ByteString
    deriving (Eq, Show, Ord, Data, Typeable, Generic)

deriving instance Serialize RRPart

-- | A name for a resource record, which may contain underscores and wildcards.
data RRName = Origin | Relative [RRPart] | Absolute [RRPart]
    deriving (Eq, Show, Ord, Data, Typeable, Generic)

deriving instance Serialize RRName

instance IsString RRName where
    fromString = either error id . parseDomain . BC.pack

instance Monoid RRName where
    mempty = Origin

    mappend Origin x                  = x
    mappend x Origin                  = x
    mappend (Absolute a) (Absolute b) = Absolute $ b <> a
    mappend (Relative a) (Absolute b) = Absolute $ a <> b
    mappend (Absolute a) (Relative b) = Absolute $ b <> a
    mappend (Relative a) (Relative b) = Relative $ b <> a

instance Domain RRName where
    parseDomain = parseOnly (rrname <* endOfInput)

    renderDomain Origin = char7 '@'
    renderDomain (Relative []) = char7 '@'
    renderDomain (Relative (p:ps)) =
        renderRRPart p <> mconcat ((char7 '.' <>) . renderRRPart <$> ps)
    renderDomain (Absolute []) = char7 '.'
    renderDomain (Absolute x) = renderDomain (Relative x) <> char7 '.'

    toByteString Origin       = BC.singleton '@'
    toByteString (Relative x) =
        BC.intercalate (BC.singleton '.') (decodeRRPart <$> x)
    toByteString (Absolute x) = BC.snoc (toByteString (Relative x)) '.'

    reverseDomain Origin       = Origin
    reverseDomain (Relative x) = Relative $ reverse x
    reverseDomain (Absolute x) = Absolute $ reverse x

    subDomainOf x _ = Just x

    toFQDN Origin       root        = root
    toFQDN (Relative x) (FQDN root) = FQDN $ fmap decodeRRPart x <> root
    toFQDN (Absolute x) _           = FQDN $ fmap decodeRRPart x

    toRRName = id

renderRRPart :: RRPart -> Builder
renderRRPart Wildcard = char7 '*'
renderRRPart (Label l) = byteString l

decodeRRPart :: RRPart -> ByteString
decodeRRPart Wildcard = BC.singleton '*'
decodeRRPart (Label l) = l

encodeRRPart :: ByteString -> RRPart
encodeRRPart "*" = Wildcard
encodeRRPart l   = Label l

letterLC :: Parser Char
letterLC = toLower <$> letter_ascii <?> "case-insensitive"

labelFirst :: Parser ByteString
labelFirst = BC.pack <$> many (letterLC <|> digit) <?> "label-first"

labelNonHyphen :: Parser ByteString
labelNonHyphen = BC.pack <$> many1 (letterLC <|> digit)

labelRest :: Parser ByteString
labelRest = BC.concat <$> many (BC.cons <$> char '-' <*> labelNonHyphen)

labelLDH :: Parser ByteString
labelLDH = BC.append <$> labelFirst <*> labelRest <?> "letter-digit-hyhen"

constrainLen :: ByteString -> Parser ByteString
constrainLen s | BC.length s > 63 = fail "label must be 63 characters or less"
               | otherwise        = return s

label :: Parser ByteString
label = do
    lstr <- BC.cons <$> letterLC <*> labelLDH <?> "label"
    constrainLen lstr

domain :: Parser FQDN
domain = FQDN <$> label `sepBy1` char '.'

rrpartFull :: Parser ByteString
rrpartFull = do
    first <- letterLC <|> char '_'
    lstr <- BC.cons <$> pure first <*> labelLDH <?> "label"
    constrainLen lstr

rrpart :: Parser RRPart
rrpart = (Wildcard <$ string "*") <|> (Label <$> rrpartFull)

rrname :: Parser RRName
rrname = Origin <$ char '@'
     <|> Absolute <$> rrpart `sepBy` char '.' <* char '.'
     <|> Relative <$> rrpart `sepBy1` char '.'

email :: Parser FQDN
email = fmap FQDN . (:) <$> label <* char '@' <*> label `sepBy1` char '.'

-- | Parse an email address and turn it into a 'FQDN'.
--
-- This basically works like `parseDomain` but expects an @\@@ after the first
-- label.
emailToFQDN :: ByteString -> Either String FQDN
emailToFQDN = parseOnly (email <* endOfInput)

SC.deriveSafeCopy 0 'SC.base ''FQDN
SC.deriveSafeCopy 0 'SC.base ''RRPart
SC.deriveSafeCopy 0 'SC.base ''RRName
