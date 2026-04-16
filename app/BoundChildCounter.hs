{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module BoundChildCounter (component_, Model (..), counterLens, initModel) where

import Miso (Component (bindings), Effect, View, component, ms, text, (<-->))
import Miso.Html.Element qualified as H
import Miso.Html.Event qualified as E
import Miso.Lens (Lens, lens, this, (+=), (-=), (^.))
import Miso.Lens.TH (makeLenses)

data Model = Model
    { _counterLens :: Int
    }
    deriving (Show, Eq)

$(makeLenses ''Model)

initModel :: Model
initModel =
    Model
        { _counterLens = 0
        }

data Action
    = AddOne
    | SubtractOne
    deriving (Show, Eq)

component_ :: Lens parent Model -> Component parent Model Action
component_ lensParentToModel =
    (component initModel updateModel viewModel)
        { bindings = [lensParentToModel <--> this]
        }

updateModel :: Action -> Effect parent Model Action
updateModel = \case
    AddOne ->
        counterLens += 1
    SubtractOne ->
        counterLens -= 1

viewModel :: Model -> View Model Action
viewModel x =
    H.div_
        []
        [ H.h1_ [] ["Child counter"]
        , H.div_ [] [text (ms (x ^. counterLens))]
        , H.div_
            []
            [ H.button_ [E.onClick AddOne] [text "+"]
            , H.button_ [E.onClick SubtractOne] [text "-"]
            ]
        ]
