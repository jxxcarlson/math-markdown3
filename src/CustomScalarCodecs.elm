
module CustomScalarCodecs exposing (Id, Jsonb(..), Timestamptz(..), Uuid, codecs)

import Json.Decode as Decode
import Json.Encode as Encode
import Api.Scalar exposing(defaultCodecs)
import Time exposing(Posix(..))
import Graphql.Internal.Builder.Object as Object
import Prng.Uuid exposing(Uuid(..))
import Random.Pcg.Extended exposing (Seed, initialSeed, step)

type Id =
    Id String

type Jsonb = Jsonb (List String)

type Timestamptz = Timestamptz String

type alias PosixTime =
    Time.Posix

type alias Uuid = Prng.Uuid.Uuid




codecs : Api.Scalar.Codecs Id Jsonb Timestamptz Uuid
codecs =
    let
      (defaultUuid, newSeed ) =
                  step Prng.Uuid.generator (initialSeed 0 [1,2,3,4])
    in
    Api.Scalar.defineCodecs
        { codecId =
             { encoder = \(Id raw) -> Encode.string raw
               , decoder = Object.scalarDecoder |> Decode.map Id
             }
        , codecJsonb =
             {   encoder = \(Jsonb raw) -> raw |> String.join "," |> Encode.string
               , decoder = Decode.list Decode.string |> Decode.map Jsonb
             }
        , codecTimestamptz =
             { encoder = \(Timestamptz raw) -> Encode.string raw
             , decoder = Object.scalarDecoder |> Decode.map Timestamptz
             }
         , codecUuid =
             { encoder = \uuid -> Encode.string (Prng.Uuid.toString uuid)
--               encoder = \rawUuid ->
--                 case rawUuid of
--                   Just uuid -> Encode.string (Prng.Uuid.toString uuid)
--                   Nothing -> Encode.string "14dc0e4d-8c90-4e97-a384-291467d62200"
             , decoder = Object.scalarDecoder |> Decode.map (Prng.Uuid.fromString >> Maybe.withDefault defaultUuid)
                  --|> Maybe.withDefault (Uuid.fromString "14dc0e4d-8c90-4e97-a384-291467d62200")
             }
        }



