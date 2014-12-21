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

type alias PBValues = { keys : Int
                      , keysdate : String
                      , time : Int
                      , timedate : String }
type alias PB = { name : String
                , keys : Int
                , keysdate : String
                , time : Int
                , timedate : String }
type alias PBs = List PB

bestDecoder : Decode.Decoder PB
bestDecoder =
  Decode.object5 PB
    ("name" := Decode.string)
    ("keys" := Decode.int)
    ("keysdate" := Decode.string)
    ("time" := Decode.int)
    ("timedate" := Decode.string)

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
showBest {name, keys, time, keysdate, timedate} =
  Encode.object [ ("name", Encode.string name)
                , ("keys", Encode.int keys)
                , ("keysdate", Encode.string keysdate)
                , ("time", Encode.int time)
                , ("timedate", Encode.string timedate)
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

updateTime : PB -> PB -> PB
updateTime pb newBest =
  if newBest.time < pb.time then { pb | time <- newBest.time
                                      , timedate <- newBest.timedate }
                            else pb

updateKeys : PB -> PB -> PB
updateKeys pb newBest =
  if newBest.keys < pb.keys then { pb | keys <- newBest.keys
                                      , keysdate <- newBest.keysdate }
                            else pb

updateBest : PB -> PB -> PB
updateBest best newBest =
  best
  |> updateKeys newBest
  |> updateKeys newBest

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
                          updateBest { ob | name = name } newBest
                        Nothing -> newBest
      newDict = uncurry Dict.insert (toPair newEntry) dict
  in  newDict |> Dict.toList |> List.map fromPair