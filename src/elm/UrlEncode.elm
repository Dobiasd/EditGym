module UrlEncode where

-- http://en.wikipedia.org/w/index.php?title=Percent-encoding&oldid=634660400#Types_of_URI_characters
unreservedCharacters =
  [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-', '_', '.', '~' ]

-- http://en.wikipedia.org/w/index.php?title=Percent-encoding&oldid=634660400#Percent-encoding_reserved_characters
reservedCharacters = [
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
  ]

-- http://en.wikipedia.org/w/index.php?title=Percent-encoding&oldid=634660400#Character_data
commonCharacters = [
    ('\n', "%0A")
  , ('\r', "%0D")
  , ('"', "%20")
  , ('', "%22")
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
  ]