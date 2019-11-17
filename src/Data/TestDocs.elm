module Data.TestDocs exposing (da, db, dc, dd, de, dummy, id1, id2, id3, id4, id5, id6, initialOutline, master)

import Document exposing (..)
import Utility exposing (getId)


initialOutline =
    """
A
B
C
D
E
"""



-- DATA FOR TESTS --


dummy =
    { id = id1
    , title = "Text"
    , authorIdentifier = "jxxcarlson"
    , content = "empty"
    , public = False
    , tags = []
    , slug = "yada123"
    , docType = Markdown MDExtendedMath
    , childInfo = []
    , permissions = []
    }



-- IDS --
-- DOCUMENTS --


id1 =
    getId 1


id2 =
    getId 2


id3 =
    getId 3


id4 =
    getId 4


id5 =
    getId 5


id6 =
    getId 6


master =
    { dummy | id = id6, title = "Master", childInfo = [ ( id1, 0 ), ( id2, 0 ), ( id3, 0 ), ( id4, 0 ), ( id5, 0 ) ] }


da =
    { dummy | id = id1, title = "A" }


db =
    { dummy | id = id2, title = "B" }


dc =
    { dummy | id = id3, title = "C" }


dd =
    { dummy | id = id4, title = "DC" }


de =
    { dummy | id = id5, title = "E" }
