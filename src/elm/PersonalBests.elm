module PersonalBests where

import Result
import Debug
import Dict
import Maybe
import List
import String
import Json.Decode as Decode
import Json.Decode((:=))
import Json.Encode as Encode

type alias PBValues = { keys : Int, time : Int }
type alias PB = { name : String, keys : Int, time : Int }
type alias PBs = List PB

bestDecoder : Decode.Decoder PB
bestDecoder =
  Decode.object3 PB
    ("name" := Decode.string)
    ("keys" := Decode.int)
    ("time" := Decode.int)

bestsDecoder : Decode.Decoder PBs
bestsDecoder =
  Decode.list bestDecoder

readBests : String -> PBs
readBests str =
  if String.isEmpty str
     then let dummy = Debug.log "Json.Decode:" "No saved personal bests."
          in  []
     else case Decode.decodeString bestsDecoder str of
            Result.Ok pbs -> pbs
            Result.Err error -> let dummy = Debug.log "Json.Decode:" error
                                in  []

showBest : PB -> Encode.Value
showBest {name, keys, time} =
  Encode.object [ ("name", Encode.string name)
                , ("keys", Encode.int keys)
                , ("time", Encode.int time)
                ]

showBests : PBs -> String
showBests pbs =
  List.map showBest pbs |> Encode.list |> Encode.encode 4

get : PBs -> String -> Maybe PB
get bests name =
  let dict = toDict bests
  in  case Dict.get name dict of
        Maybe.Just res -> Maybe.Just { res | name = name }
        Nothing -> Nothing

updateBest : PB -> Int -> Int -> PB
updateBest ({time, keys} as best) newKeys newTime =
  { best | time <- min time newTime
         , keys <- min keys newKeys }

toPair : PB -> (String, PBValues)
toPair best = (best.name, { best - name })

fromPair : (String, PBValues) -> PB
fromPair (name, vals) = { vals | name = name }

toDict : PBs -> Dict.Dict String PBValues
toDict = List.map toPair >> Dict.fromList

insert : PBs -> PB -> PBs
insert bests newBest =
  let dict = toDict bests
      name = newBest.name
      newEntry = case Dict.get name dict of
                        Maybe.Just ob ->
                          updateBest { ob | name = name }
                                     newBest.keys
                                     newBest.time
                        Nothing -> newBest
      newDict = uncurry Dict.insert (toPair newEntry) dict
  in  newDict |> Dict.toList |> List.map fromPair