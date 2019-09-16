module Author exposing (..)

import Time exposing(Posix)

type alias AuthorID = String

type alias Author = {
     id : AuthorID
    , email : String
    , public : Bool
    , firstName : String
    , lastName : String
    , timeEnrolled : Posix
    , timeUpdated : Posix
  }