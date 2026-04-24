{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Proxy (Proxy (Proxy))
import GHC.Records (HasField)
import Miso
import Miso.CSS qualified as CSS
import Miso.Html.Element qualified as H
import Miso.Html.Event qualified as E
import Miso.Html.Property qualified as P
import Miso.Lens
import Miso.Lens.TH

data AppModel = AppModel
    { _counter :: Int
    }
    deriving (Eq, Show)
$(makeLenses ''AppModel)

data Action
    = AddOne
    | SubtractOne
    deriving (Show, Eq)

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif

main :: IO ()
#ifdef INTERACTIVE
main = reload defaultEvents app
#else
main = startApp defaultEvents app
#endif

app :: App AppModel Action
app =
    (component (AppModel 0) updateModel viewModel)
        { styles = []
        }

updateModel :: Action -> Effect parent AppModel Action
updateModel = \case
    AddOne ->
        counter += 1
    SubtractOne ->
        counter -= 1

viewModel :: AppModel -> View parent AppModel Action
viewModel x =
    H.div_
        []
        [ H.h1_ [] ["🍜 Miso sampler"]
        , H.div_ [] [text (ms (x ^. counter))]
        , H.div_
            []
            [ H.button_ [E.onClick AddOne] [text "+"]
            , H.button_ [E.onClick SubtractOne] [text "-"]
            ]
        , "foo" +> fooComponent
        ]

-- #####################

-- Dummy child component

fooComponent :: (HasField "_counter" parent Int) => Component parent () ()
fooComponent =
    (component () (const $ pure ()) viewFoo)
        { useProps = True
        }

viewFoo :: (HasField "_counter" parent Int) => () -> View parent () ()
viewFoo _model = props @"_counter" $ \counterValue ->
    H.div_
        [ CSS.style_
            [ CSS.color (if counterValue > 0 then CSS.green else CSS.red)
            ]
        ]
        ["Foo!"]
