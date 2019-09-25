
module CustomScalarCodecs exposing (Id(..), Jsonb(..), Timestamptz(..), codecs)

import Json.Decode as Decode
import Json.Encode as Encode
import Api.Scalar
import Time exposing(Posix(..))
import Graphql.Internal.Builder.Object as Object



type Id
    = Id String


type Timestamptz = Timestamptz String

type Jsonb = Jsonb (List String)

type alias PosixTime =
    Time.Posix

codecs : Api.Scalar.Codecs Id Jsonb Timestamptz
codecs =
    Api.Scalar.defineCodecs
       { codecId =
               { encoder = \(Id raw) -> Encode.string raw
               , decoder = Object.scalarDecoder |> Decode.map Id
               }
           , codecJsonb =
               { encoder = \(Jsonb raw) -> raw |> String.join "," |> Encode.string
               , decoder = Decode.list Decode.string |> Decode.map Jsonb
               }
           , codecTimestamptz =
               { encoder = \(Timestamptz raw) -> Encode.string raw
                , decoder = Object.scalarDecoder |> Decode.map Timestamptz
               }
       }

--codecs : Swapi.Scalar.Codecs Id PosixTime
--codecs =
--    Swapi.Scalar.defineCodecs
--        { codecId =
--            { encoder = \(Id raw) -> raw |> String.fromInt |> Encode.string
--            , decoder =
--                Decode.string
--                    |> Decode.map String.toInt
--                    |> Decode.andThen
--                        (\maybeParsedId ->
--                            case maybeParsedId of
--                                Just parsedId ->
--                                    Decode.succeed parsedId
--
--                                Nothing ->
--                                    Decode.fail "Could not parse ID as an Int."
--                        )
--                    |> Decode.map Id
--            }
--        , codecPosixTime =
--            { encoder = Time.posixToMillis >> Encode.int
--            , decoder = Decode.int |> Decode.map Time.millisToPosix
--            }
--        }