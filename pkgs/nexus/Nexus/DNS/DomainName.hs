-- | Data type and utility functions for manipulating (fully qualified) domain
--   names.
{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}
module Nexus.DNS.DomainName
    ( DomainName

    , fromByteString
    , toByteString
    , toZoneString

    , reverseDomain
    , subDomainOf
    , isSubDomainOf
    ) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.List (isSuffixOf)
import Data.Maybe (isJust)
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import qualified Data.SafeCopy as SC
import qualified Data.ByteString.Char8 as BC

-- | A fully qualified domain name.
newtype DomainName = DN [ByteString]
    deriving (Eq, Ord, Typeable, Generic)

instance Show DomainName where
    show = show . toByteString

instance IsString DomainName where
    fromString = either error id . fromByteString . BC.pack

instance Monoid DomainName where
    mempty = DN []
    mappend (DN a) (DN b) = DN $ b `mappend` a

instance Semigroup DomainName where
    (<>) = mappend
    stimes n (DN x) = DN $ stimes n x

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

label :: Parser DomainName
label = do
    lstr <- BC.cons <$> letterLC <*> labelLDH <?> "label"
    if BC.length lstr > 63
       then fail "label must be 63 characters or less"
       else return $ DN [lstr]

dotDomain :: Parser DomainName
dotDomain = char '.' >> domain <?> "dot-domain"

domain :: Parser DomainName
domain = flip mappend <$> label <*> option mempty dotDomain

-- | Convert a 'ByteString' into the internal representation of 'DomainName'
--
-- Returns 'Left' with an error string if it's not a valid domain name.
fromByteString :: ByteString -> Either String DomainName
fromByteString = parseOnly (domain <* endOfInput)

-- | Convert a 'DomainName' to a 'ByteString' in a form typically used in zone
--   files with a dot at the end (FQDN).
toZoneString :: DomainName -> ByteString
toZoneString x = BC.snoc (toByteString x) '.'

-- | Convert a 'DomainName' to a 'ByteString'.
toByteString :: DomainName -> ByteString
toByteString (DN x) = BC.intercalate (BC.singleton '.') x

-- | Reverse the labels of a 'DomainName'.
reverseDomain :: DomainName -> DomainName
reverseDomain (DN x) = DN $ reverse x

-- | Retrieve the subdomain portions of a particular root domain.
--
-- For example (with @-XOverloadedStrings@):
--
-- >>> "foo.bar.example.org" `subDomainOf` "example.org"
-- Just "foo.bar"
subDomainOf :: DomainName -- ^ The domain name to query
            -> DomainName -- ^ The root domain
            -> Maybe DomainName
subDomainOf (DN x) (DN root)
    | not $ root `isSuffixOf` x = Nothing
    | otherwise                 = Just . DN $ take (length x - length root) x

-- | Similar to 'subDomainOf' but just returns a boolean instead of a 'Maybe'.
isSubDomainOf :: DomainName -- ^ The domain name to query
              -> DomainName -- ^ The root domain
              -> Bool
isSubDomainOf x = isJust . subDomainOf x

SC.deriveSafeCopy 0 'SC.base ''DomainName
