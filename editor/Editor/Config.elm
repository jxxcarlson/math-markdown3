module Editor.Config exposing (Config, WrapOption(..), WrapParams, default, setMaximumWrapWidth, setOptimumWrapWidth, setWrapOption)


type alias Config =
    { lines : Int
    , wrapParams : WrapParams
    , showInfoPanel : Bool
    , wrapOption : WrapOption
    }


type alias WrapParams =
    { maximumWidth : Int
    , optimalWidth : Int
    , stringWidth : String -> Int
    }


default m o =
    { lines = 10
    , wrapParams = { maximumWidth = m, optimalWidth = o, stringWidth = String.length }
    , showInfoPanel = False
    , wrapOption = DoWrap
    }



-- TODO: Make maximumWidth and optimalWidth configurable at startup and at runtime


setWrapOption : WrapOption -> Config -> Config
setWrapOption wrapOption config =
    { config | wrapOption = wrapOption }


setMaximumWrapWidth : Int -> Config -> Config
setMaximumWrapWidth k config =
    let
        w =
            config.wrapParams

        newWrapParams =
            { w | maximumWidth = k }
    in
    { config | wrapParams = newWrapParams }


setOptimumWrapWidth : Int -> Config -> Config
setOptimumWrapWidth k config =
    let
        w =
            config.wrapParams

        newWrapParams =
            { w | optimalWidth = k }
    in
    { config | wrapParams = newWrapParams }


type WrapOption
    = DoWrap
    | DontWrap
