--------------------------------------------------------------------------------
-- |
-- Module      : Network.URL
-- Copyright   : (c) Galois, Inc. 2007, 2008
-- License     : BSD3
--
-- Maintainer  : Iavor S. Diatchki
-- Stability   : Provisional
-- Portability : Portable
--
-- Provides a convenient way for working with HTTP URLs.
-- Based on RFC 1738.
-- See also: RFC 3986

module Network.URL
  ( URL(..), URLType(..), Host(..), Protocol(..)
  , secure, secure_prot
  , exportURL, importURL, exportHost
  , add_param
  , decString, encString
  , ok_host, ok_url, ok_param, ok_path
  ) where

import Data.Word(Word8)
import Data.Char(isAlpha,isAscii,isDigit)
import Data.List(intersperse)
import Numeric(readHex,showHex)
import Control.Monad

import Codec.Binary.UTF8.String as UTF8


-- | Contains information about the connection to the host.
data Host     = Host { protocol :: Protocol
                     , host     :: String
                     , port     :: Maybe Integer
                     } deriving (Eq,Ord,Show)

-- | The type of known protocols.
data Protocol = HTTP Bool | FTP Bool | RawProt String deriving (Eq,Ord,Show)

-- | Is this a \"secure\" protocol.  This works only for known protocols,
-- for 'RawProt' values we return 'False'.
secure_prot :: Protocol -> Bool
secure_prot (HTTP s)     = s
secure_prot (FTP s)      = s
secure_prot (RawProt _)  = False

-- | Does this host use a \"secure\" protocol (e.g., https).
secure :: Host -> Bool
secure x = secure_prot (protocol x)

-- | Different types of URL.
data URLType  = Absolute Host       -- ^ Has a host
              | HostRelative        -- ^ Does not have a host
              | PathRelative        -- ^ Relative to another URL
                deriving (Eq, Ord, Show)

-- | A type for working with URL.
-- The parameters are in @application\/x-www-form-urlencoded@ format:
-- <http://www.w3.org/TR/html4/interact/forms.html#h-17.13.4.1>
data URL = URL
            { url_type    :: URLType
            , url_path    :: String
            , url_params  :: [(String,String)]
            } deriving (Eq,Ord,Show)

-- | Add a (key,value) parameter to a URL.
add_param :: URL -> (String,String) -> URL
add_param url x = url { url_params = x : url_params url }


-- | Convert a list of \"bytes\" to a URL.
importURL :: String -> Maybe URL
importURL cs1 =
  do (ho,cs5) <- front cs1
     (pa,cs6) <- the_path cs5
     as       <- the_args cs6
     return URL { url_type = ho, url_path = pa, url_params = as }

  where
  front ('/':cs)  = return (HostRelative,cs)
  front cs =
    case the_prot cs of
      Just (pr,cs1) ->
        do let (ho,cs2) = the_host cs1
           (po,cs3) <- the_port cs2
           cs4 <- case cs3 of
                    [] -> return []
                    '/':cs -> return cs
                    _ -> Nothing
           return (Absolute Host { protocol = pr
                                 , host = ho
                                 , port = po
                                 }, cs4)
      _ -> return (PathRelative,cs)

  the_prot :: String -> Maybe (Protocol, String)
  the_prot urlStr = case break (':' ==) urlStr of
     (as@(_:_), ':' : '/' : '/' : bs) -> Just (prot, bs)
       where prot = case as of
                      "https" -> HTTP True
                      "http"  -> HTTP False
                      "ftps"  -> FTP  True
                      "ftp"   -> FTP  False
                      _       -> RawProt as
     _                                -> Nothing

  the_host cs = span ok_host cs

  the_port (':':cs)     = case span isDigit cs of
                            ([],_)   -> Nothing
                            (xs,ds) -> Just (Just (read xs),ds)
  the_port cs5          = return (Nothing, cs5)

  the_path cs = do let (as,bs) = break end_path cs
                   s <- decString False as
                   return (s,bs)
    where end_path c = c == '#' || c == '?'

  the_args ('?' : cs)   = parse_params cs
  the_args _            = return []


parse_params :: String -> Maybe [(String,String)]
parse_params [] = return []
parse_params cs = mapM a_param (breaks ('&'==) cs)
  where
  a_param cs = do let (as,bs) = break ('=' ==) cs
                  k <- decString True as
                  v <- case bs of
                         "" -> return ""
                         _:xs -> decString True xs
                  return (k,v)


-- | Convert the host part of a URL to a list of \"bytes\".
exportHost :: Host -> String
exportHost abs = the_prot ++ "://" ++ host abs ++ the_port
  where the_prot  = exportProt (protocol abs)
        the_port  = maybe "" (\x -> ":" ++ show x) (port abs)

-- | Convert the host part of a URL to a list of \"bytes\".
-- WARNING: We output \"raw\" protocols as they are.
exportProt :: Protocol -> String
exportProt prot = case prot of
  HTTP True   -> "https"
  HTTP False  -> "http"
  FTP  True   -> "ftps"
  FTP  False  -> "ftp"
  RawProt s   -> s


-- | Convert a URL to a list of \"bytes\".
-- We represent non-ASCII characters using UTF8.
exportURL :: URL -> String
exportURL url = abs ++ the_path ++ the_params
  where
  abs         = case url_type url of
                  Absolute host -> exportHost host ++ "/"
                  HostRelative  -> "/"
                  PathRelative  -> ""

  the_path    = encString False ok_path (url_path url)
  the_params  = case url_params url of
                  [] -> ""
                  xs -> "?" ++ concat (intersperse "&" $ map a_param xs)

  a_param (k,mv)  = encString True ok_param k ++
                    case mv of
                      "" -> ""
                      v  -> '=' : encString True ok_param v






-- | Convert a string to bytes by escaping the characters that
-- do not satisfy the input predicate.  The first argument specifies
-- if we should replace spaces with +.
encString :: Bool -> (Char -> Bool) -> String -> String
encString pl p xs = foldr enc1 [] xs
  where enc1 ' ' xs | pl = '+' : xs
        enc1 x xs = if p x then x : xs else encChar x ++ xs

-- | %-encode a character. Uses UTF8 to represent characters as bytes.
encChar :: Char -> String
encChar c = concatMap encByte (UTF8.encode [c])

-- | %-encode a byte.
encByte :: Word8 -> String
encByte b = '%' : case showHex b "" of
                    d@[_] -> '0' : d
                    d     -> d

-- | Decode a list of \"bytes\" to a string.
-- Performs % and UTF8 decoding.
decString :: Bool -> String -> Maybe String
decString b xs = fmap UTF8.decode (decStrBytes b xs)

-- Convert a list of \"bytes\" to actual bytes.
-- Performs %-decoding.  The boolean specifies if we should turn pluses into
-- spaces.
decStrBytes :: Bool -> String -> Maybe [Word8]
decStrBytes _ []          = Just []
decStrBytes p ('%' : cs)  = do (n,cs1) <- decByte cs
                               fmap (n:) (decStrBytes p cs1)
decStrBytes p (c : cs)    = let b = if p && c == '+'
                                       then 32    -- space
                                       else fromIntegral (fromEnum c)
                            in (b :) `fmap` decStrBytes p cs
                            -- truncates "large bytes".


-- | Parse a percent-encoded byte.
decByte :: String -> Maybe (Word8,String)
decByte (x : y : cs)  = case readHex [x,y] of
                          [(n,"")] -> Just (n,cs)
                          _ -> Nothing
decByte _             = Nothing



-- Classification of characters.
-- Note that these only return True for ASCII characters; this is important.
--------------------------------------------------------------------------------
ok_host :: Char -> Bool
ok_host c   = isDigit c || isAlphaASCII c || c == '.' || c == '-'

ok_param :: Char -> Bool
ok_param c  = ok_host c || c `elem` "~;:@$_!*'(),"

-- | Characters that can appear non % encoded in the path part of the URL
ok_path :: Char -> Bool
ok_path c   = ok_param c || c `elem` "/=&"

-- XXX: others? check RFC
-- | Characters that may appear in the textual representation of a URL
ok_url :: Char -> Bool
ok_url c = isDigit c || isAlphaASCII c || c `elem` ".-;:@$_!*'(),/=&?~%"

-- Misc
--------------------------------------------------------------------------------
isAlphaASCII :: Char -> Bool
isAlphaASCII x = isAscii x && isAlpha x

breaks :: (a -> Bool) -> [a] -> [[a]]
breaks p xs = case break p xs of
                (as,[])   -> [as]
                (as,_:bs) -> as : breaks p bs


