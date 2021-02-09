module Aftok.Navigation where

import Prelude
import Data.Either (Either)
import Data.Lens (view)
import Data.Maybe (Maybe)
import URI.URI 
  ( HierPath
  , Host
  , Path
  , Port
  , URIOptions
  , UserInfo
  , parser
  , _query
  )
import URI.Extra.QueryPairs (QueryPairs, Key, Value)
import URI.Extra.QueryPairs as QueryPairs
import URI.Fragment (Fragment)
import URI.HostPortPair as HostPortPair
import URI.HostPortPair (HostPortPair)
import Text.Parsing.Parser (runParser, ParseError)

parseURIQuery :: String -> Either ParseError (Maybe (QueryPairs Key Value))
parseURIQuery uriStr =
  view _query <$> runParser uriStr (parser options)

options ∷ Record (URIOptions UserInfo (HostPortPair Host Port) Path HierPath (QueryPairs Key Value) Fragment)
options =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseQuery: QueryPairs.parse pure pure
  , printQuery: QueryPairs.print identity identity
  , parseFragment: pure
  , printFragment: identity
  }
