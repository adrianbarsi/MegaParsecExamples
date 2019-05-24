-- scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import Control.Applicative hiding (some)
import Control.Monad
import Data.Text
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Uri = Uri
  { uriScheme :: Scheme
  , uriAuthority :: Maybe Authority
  } deriving (Eq, Show)

data Scheme
    = SchemeData
    | SchemeFile
    | SchemeFtp
    | SchemeHttp
    | SchemeHttps
    | SchemeIrc
    | SchemeMailto
    deriving (Eq, Show)

type User = Text
type Password = Text
type Host = Text
type Port = Int

data Authority = Authority
    { authUser :: Maybe (User, Password)
    , authHost :: Host
    , authPort :: Maybe Port
    } deriving (Eq, Show)

{-
pScheme :: Parser Text
pScheme = isValid >> eof >> return "Valid"
    where
        isValid = string "data"
            <|> string "file"
            <|> string "ftp"
            <|> string "http"
            <|> string "https"
            <|> string "irc"
            <|> string "mailto"

-}

{-
-- Better way of defining pScheme
-- choice is a fold of <|> for a given list of parsers
-- choise is a synonym for asum (Alternative Sum)
pScheme :: Parser Text
pScheme = choice
        [ string "data"
        , string "file"
        , string "ftp"
        , string "https"
        , string "http"
        , string "irc"
        , string "mailto"
        ]
-}

-- If a given parser works, discard its return value and pack a Scheme data constructor into Parser and return it
-- string has implicit back tracking
pScheme :: Parser Scheme
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttp   <$ string "http"
  , SchemeHttps  <$ string "https"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]


-- Importance of try
{-
alternatives :: Parser (Char, Char)
alternatives = foo <|> bar
  where
    foo = (,) <$> char 'a' <*> char 'b'
    bar = (,) <$> char 'a' <*> char 'c'
Looks reasonable, let us try it:

λ> parseTest alternatives "ab"
('a','b')

λ> parseTest alternatives "ac"
1:2:
  |
1 | ac
  |  ^
unexpected 'c'
expecting 'b'

Because foo consumes some part of input 'ac', it means that bar will never be tested. Instead it will throw an exception.
The solution is to use try
-}

pUri :: Parser Uri
pUri = do
    uriScheme <- pScheme
    void (char ':')
    uriAuthority <- optional . try $ do            -- (1)
        void (string "//")
        authUser <- optional . try $ do              -- (2)
            user <- T.pack <$> some alphaNumChar       -- (3)
            void (char ':')
            password <- T.pack <$> some alphaNumChar
            void (char '@')
            return (user, password)
        authHost <- T.pack <$> some (alphaNumChar <|> char '.')
        authPort <- optional . try $ do                      -- (4)
            char ':'
            port <- L.decimal
            eof
            return port
        
        return Authority {..}                        -- (5)
    return Uri {..}                                -- (6)

{-
In (1) and (2) we need to wrap the argument of optional with try because it is a composite parser, not a primitive.

(3) some is just like many, but demands that its argument parser matches at least once: some p = (:) <$> p <*> many p.

(4) Do not use try unless necessary! Here if char ':' succeeds (which is by itself built on top of token, so it does not need a try), we know for sure that port must follow after it, so we just demand a decimal number with L.decimal. After matching :, we are sort of committed and do not need a way to go back.

In (5) and (6) we assemble Authority and Uri values using the RecordWildCards language extension.
-}