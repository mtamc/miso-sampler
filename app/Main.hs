{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import BoundChildCounter qualified
import Control.Category ((<<<))
import Miso (App, Effect, View, component, defaultEvents, ms, startApp, text, (+>))
import Miso.Html.Element qualified as H
import Miso.Html.Event qualified as E
import Miso.Lens (Lens, lens, (.=), (^.))
import Miso.Lens.TH (makeLenses)

data Model = Model
    { _boundChildCounterLens :: BoundChildCounter.Model
    }
    deriving (Show, Eq)
$(makeLenses ''Model)

initModel :: Model
initModel =
    Model
        { _boundChildCounterLens = BoundChildCounter.initModel
        }

main :: IO ()
#ifdef INTERACTIVE
main = reload defaultEvents app
#else
main = startApp defaultEvents app
#endif

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif

app :: App Model Action
app = component initModel updateModel viewModel

data Action
    = ResetChildCounter
    deriving (Show, Eq)

updateModel :: Action -> Effect parent Model Action
updateModel = \case
    ResetChildCounter ->
        boundChildCounterLens .= BoundChildCounter.initModel

viewModel :: Model -> View Model Action
viewModel mdl =
    H.div_
        []
        [ H.span_
            []
            [ text
                ( "I am a <span> in the parent, and I am aware of the bound child counter's model. It is "
                    <> Miso.ms (mdl ^. (boundChildCounterLens <<< BoundChildCounter.counterLens))
                )
            ]
        , H.br_ []
        , H.button_ [E.onClick ResetChildCounter] [text "I am a <button> in the parent, click me to reset the child counter model!"]
        , "boundChildCounter" +> BoundChildCounter.component_ boundChildCounterLens
        ]
