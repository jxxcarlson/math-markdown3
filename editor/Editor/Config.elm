module Editor.Config exposing (Config, WrapOption(..), WrapParams, default, setMaximumWrapWidth, setOptimumWrapWidth, setWrapOption)

{-| Use this module to configure the editor.
The `default` value is a basic configuration
which you can modify like this:

    config =
        { default | lines = 30 }

@docs Config, WrapOption, WrapParams, default, setMaximumWrapWidth, setOptimumWrapWidth, setWrapOption

-}


{-| -}
type alias Config =
    { lines : Int
    , wrapParams : WrapParams
    , showInfoPanel : Bool
    , wrapOption : WrapOption
    , height : Float
    , lineHeight : Float
    }


{-| -}
type alias WrapParams =
    { maximumWidth : Int
    , optimalWidth : Int
    , stringWidth : String -> Int
    }


{-| -}
type WrapOption
    = DoWrap
    | DontWrap


{-| -}
default : Config
default =
    { lines = 10
    , wrapParams = { maximumWidth = 50, optimalWidth = 45, stringWidth = String.length }
    , showInfoPanel = False
    , wrapOption = DoWrap
    , height = 400
    , lineHeight = 14
    }



-- TODO: Make maximumWidth and optimalWidth configurable at startup and at runtime


{-| -}
setWrapOption : WrapOption -> Config -> Config
setWrapOption wrapOption config =
    { config | wrapOption = wrapOption }


{-| -}
setMaximumWrapWidth : Int -> Config -> Config
setMaximumWrapWidth k config =
    let
        w =
            config.wrapParams

        newWrapParams =
            { w | maximumWidth = k }
    in
    { config | wrapParams = newWrapParams }


{-| -}
setOptimumWrapWidth : Int -> Config -> Config
setOptimumWrapWidth k config =
    let
        w =
            config.wrapParams

        newWrapParams =
            { w | optimalWidth = k }
    in
    { config | wrapParams = newWrapParams }
