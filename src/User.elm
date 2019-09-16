module User exposing (User, dummy)

import Time exposing(Posix)

type alias UserId = String

type alias User = {
     id : UserId
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