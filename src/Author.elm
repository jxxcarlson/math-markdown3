module Author exposing (Author, dummy)

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
    , admin : Bool
  }

dummy = {
     id = "jxxcarlson"
   , email = "jxxcarlson@gmail.com"
   , public = True
   , firstName = "James"
   , lastName = "Carlson"
   , timeEnrolled = Time.millisToPosix (1568667528 * 1000)
   , timeUpdated = Time.millisToPosix (1568667528 * 1000)
   , admin = True
  }