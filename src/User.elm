module User exposing (AuthorizedUser, OutsideUser, User, barebonesUser, dummy, outsideUserDecoder, outsideUserEncoder, outsideUserWithToken)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Prng.Uuid as Uuid exposing (Uuid(..))
import Utility exposing (getId)


type alias User =
    { id : Uuid
    , username : String
    , email : String
    , firstName : String
    , lastName : String
    , admin : Bool
    }


type alias AuthorizedUser =
    { id : Int
    , username : String
    , email : String
    , token : String
    }


barebonesUser : Uuid -> String -> String -> User
barebonesUser uuid username email =
    { id = uuid
    , username = username
    , email = email
    , firstName = "not_yet"
    , lastName = "not_yet"
    , admin = False
    }


dummy username =
    { id = getId 6
    , username = username
    , email = username ++ "@gmail.com"
    , public = True
    , firstName = "James"
    , lastName = "Carlson"
    , admin = False
    }



-- CODECS FOR OUTSIDE --


uuidDecoder : Decoder Uuid
uuidDecoder =
    D.string
        |> D.map Uuid.fromString
        |> D.map (Maybe.withDefault Utility.id0)


type alias OutsideUser =
    { id : Uuid
    , username : String
    , email : String
    , firstName : String
    , lastName : String
    , admin : Bool
    , token : String
    }


outsideUserWithToken : String -> User -> OutsideUser
outsideUserWithToken token u =
    { id = u.id
    , username = u.username
    , email = u.email
    , firstName = u.firstName
    , lastName = u.lastName
    , admin = u.admin
    , token = token
    }


outsideUserDecoder : Decoder OutsideUser
outsideUserDecoder =
    D.map7 OutsideUser
        (D.field "id" uuidDecoder)
        (D.field "username" D.string)
        (D.field "email" D.string)
        (D.field "firstName" D.string)
        (D.field "lastName" D.string)
        (D.field "admin" D.bool)
        (D.field "token" D.string)


outsideUserEncoder : OutsideUser -> E.Value
outsideUserEncoder u =
    E.object
        [ ( "id", E.string (Uuid.toString u.id) )
        , ( "username", E.string u.username )
        , ( "email", E.string u.email )
        , ( "firstName", E.string u.firstName )
        , ( "lastName", E.string u.lastName )
        , ( "admin", E.bool u.admin )
        , ( "token", E.string u.token )
        ]
