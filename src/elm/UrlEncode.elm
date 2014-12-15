module UrlEncode where

import Dict
import List
import String
import Maybe (withDefault)

-- http://en.wikipedia.org/w/index.php?title=Percent-encoding&oldid=634660400#Types_of_URI_characters
unreservedCharacters : List Char
unreservedCharacters =
  [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-', '_', '.', '~' ]

charToString : Char -> String
charToString c = String.fromList [c]

unreservedCharactersDict : Dict.Dict Char String
unreservedCharactersDict =
  List.map2 (,) unreservedCharacters (List.map charToString unreservedCharacters)
  |> Dict.fromList

-- http://en.wikipedia.org/w/index.php?title=Percent-encoding&oldid=634660400#Percent-encoding_reserved_characters
reservedCharactersDict : Dict.Dict Char String
reservedCharactersDict = [
    ('!', "%21")
  , ('#', "%23")
  , ('$', "%24")
  , ('&', "%26")
  , ('\'', "%27")
  , ('(', "%28")
  , (')', "%29")
  , ('*', "%2A")
  , ('+', "%2B")
  , (',', "%2C")
  , ('/', "%2F")
  , (':', "%3A")
  , (';', "%3B")
  , ('=', "%3D")
  , ('?', "%3F")
  , ('@', "%40")
  , ('[', "%5B")
  , (']', "%5D")
  ] |> Dict.fromList

-- http://en.wikipedia.org/w/index.php?title=Percent-encoding&oldid=634660400#Character_data
commonCharactersDict : Dict.Dict Char String
commonCharactersDict = [
    ('\n', "%0A")
  , ('\r', "%0D")
  , (' ', "%20")
  , ('"', "%22")
  , ('%', "%25")
  , ('-', "%2D")
  , ('.', "%2E")
  , ('<', "%3C")
  , ('>', "%3E")
  , ('\\', "%5C")
  , ('^', "%5E")
  , ('_', "%5F")
  , ('`', "%60")
  , ('{', "%7B")
  , ('|', "%7C")
  , ('}', "%7D")
  , ('~', "%7E")
  ] |> Dict.fromList

charactersDict : Dict.Dict Char String
charactersDict =
  commonCharactersDict
  |> Dict.union reservedCharactersDict
  |> Dict.union unreservedCharactersDict

encodeChar : Char -> String
encodeChar c = withDefault "-" (Dict.get c charactersDict)

encode : String -> String
encode =
  String.toList
  >> List.map encodeChar
  >> List.map String.toList
  >> List.concat
  >> String.fromList

genLink : String -> List (String, String) -> String
genLink baseUrl parameters =
  let
    paramsUrl = List.map (\(name, value) -> name ++ "=" ++ encode value)
                  parameters |> String.join "&"
  in
    baseUrl ++ (if String.isEmpty paramsUrl then "" else "?" ++ paramsUrl)