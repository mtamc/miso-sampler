{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PenpalChildCounter (component_, Model (..), counterLens, initModel, InboundMessage (..), OutboundMessage (..)) where

import GHC.Generics (Generic)
import Miso (Component (mailbox), Effect, MisoString, View, checkMail, component, consoleLog, get, io_, mailParent, ms, text)
import Miso.Html.Element qualified as H
import Miso.Html.Event qualified as E
import Miso.JSON qualified
import Miso.Lens (Lens, lens, (+=), (-=), (.=), (^.))
import Miso.Lens.TH (makeLenses)
import Miso.TH (makeSumTypeJSONInstances)

data Model = Model
    { _counterLens :: Int
    }
    deriving (Show, Eq, Generic, Miso.JSON.FromJSON, Miso.JSON.ToJSON)

$(makeLenses ''Model)

initModel :: Model
initModel =
    Model
        { _counterLens = 0
        }

data Action
    = AddOne
    | SubtractOne
    | HandleInboundMessage InboundMessage
    | HandleUnknownInboundMessage MisoString
    | SendModelToParent
    deriving (Show, Eq)

data InboundMessage
    = ResetTheCounter
    deriving (Eq, Show, Generic)
-- FOOTGUN ALERT: Miso will allow you to automatically derive Miso.JSON.FromJSON and Miso.JSON.ToJSON instances for sum types with only one nullary data constructor like the above, but will serialize them to `Object (fromList []), meaning `data Foo = Foo` will be incorrectly parsed the same as `data Bar = Bar`
-- Therefore, you must either write the instances by hand, or use my TH helper to write them.
$(makeSumTypeJSONInstances ''InboundMessage)

data OutboundMessage
    = HereIsMyModel Model
    deriving (Eq, Show, Generic)
-- See `InboundMessage` note.
$(makeSumTypeJSONInstances ''OutboundMessage)

component_ :: Component parent Model Action
component_ =
    (component initModel updateModel viewModel)
        { mailbox = checkMail HandleInboundMessage HandleUnknownInboundMessage
        }

updateModel :: Action -> Effect parent Model Action
updateModel = \case
    AddOne ->
        counterLens += 1
    SubtractOne ->
        counterLens -= 1
    HandleInboundMessage inboundMsg ->
        case inboundMsg of
            ResetTheCounter -> do
                counterLens .= 0
    HandleUnknownInboundMessage receivedError ->
        io_ . consoleLog $
            "Received non-parsing message with error: " <> receivedError
    SendModelToParent -> do
        mdl <- get
        mailParent (HereIsMyModel mdl)

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
        , H.button_
            [E.onClick SendModelToParent]
            [text "I am a <button> in the child, click me to send my Model to my parent!"]
        ]
