module Editor.Model exposing (InternalState, Snapshot, slider)

import Buffer exposing (Buffer)
import Editor.Config exposing (Config)
import Editor.History exposing (History)
import Position exposing (Position)
import RollingList exposing (RollingList)
import SingleSlider as Slider exposing (..)
import Window exposing (Window)


type alias Snapshot =
    { cursor : Position
    , selection : Maybe Position
    , buffer : Buffer
    }


type alias InternalState =
    { config : Config
    , scrolledLine : Int
    , window : Window
    , cursor : Position
    , selection : Maybe Position
    , selectedText : Maybe String
    , clipboard : String
    , currentLine : Maybe String
    , dragging : Bool
    , history : History Snapshot
    , searchTerm : String
    , replacementText : String
    , canReplace : Bool
    , searchResults : RollingList ( Position, Position )
    , showHelp : Bool
    , showInfoPanel : Bool
    , showGoToLinePanel : Bool
    , showSearchPanel : Bool
    , savedBuffer : Buffer
    , slider : Slider.Model
    }


slider : Slider.Model
slider =
    let
        initialSlider =
            Slider.defaultModel
    in
    { initialSlider
        | min = 0
        , max = 100
        , step = 0.01
        , value = 0
    }
