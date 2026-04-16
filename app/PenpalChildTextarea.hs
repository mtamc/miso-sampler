{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module PenpalChildTextarea (component_, Model (..), textLens, initModel, InboundMessage (..), OutboundMessage (..)) where

import GHC.Generics (Generic)
import Miso (Component (mailbox), Effect, MisoString, View, checkMail, component, consoleLog, createWith, get, inline, io_, mailParent, ms, text, (=:))
import Miso.Html.Element qualified as H
import Miso.Html.Event qualified as E
import Miso.Html.Property qualified as P
import Miso.JSON qualified
import Miso.Lens (Lens, lens, (.=))
import Miso.Lens.TH (makeLenses)
import Miso.TH (makeSumTypeJSONInstances)

data Model = Model
    { _textLens :: MisoString
    }
    deriving (Show, Eq, Generic, Miso.JSON.FromJSON, Miso.JSON.ToJSON)

$(makeLenses ''Model)

initModel :: Model
initModel =
    Model
        { _textLens = ""
        }

data Action
    = HandleTextareaInput MisoString
    | HandleInboundMessage Miso.JSON.Value InboundMessage
    | HandleUnknownInboundMessage MisoString
    | SendModelToParent
    deriving (Show, Eq)

data InboundMessage
    = ResetTheTextarea
    deriving (Eq, Show, Generic)
-- FOOTGUN ALERT: Miso will allow you to automatically derive Miso.JSON.FromJSON and Miso.JSON.ToJSON instances for sum types with only one nullary data constructor like the above, but will serialize them to `Object (fromList []), meaning `data Foo = Foo` will be incorrectly parsed the same as `data Bar = Bar`
-- Therefore, you must either write the instances by hand, or use my TH helper to write them.
$(makeSumTypeJSONInstances ''InboundMessage)

data OutboundMessage
    = HereIsMyTextareaModel Model
    -- See `InboundMessage` note.
    deriving (Eq, Show, Generic)
$(makeSumTypeJSONInstances ''OutboundMessage)

component_ :: Component parent Model Action
component_ =
    (component initModel updateModel viewModel)
        { mailbox = \val ->
            checkMail (HandleInboundMessage val) HandleUnknownInboundMessage val
        }

updateModel :: Action -> Effect parent Model Action
updateModel = \case
    HandleTextareaInput val ->
        textLens .= val
    HandleInboundMessage value inboundMsg ->
        case inboundMsg of
            ResetTheTextarea -> do
                io_ $ consoleLog $ ms (show value)
                textLens .= ""
                io_ $ do
                    exposedNames <- createWith ["textareaId" =: ("my-textarea" :: MisoString)]
                    inline "document.getElementById(textareaId).value = ''" exposedNames
    HandleUnknownInboundMessage receivedError ->
        io_ . consoleLog $
            "Received non-parsing message with error: " <> receivedError
    SendModelToParent -> do
        mdl <- get
        mailParent (HereIsMyTextareaModel mdl)

viewModel :: Model -> View Model Action
viewModel _mdl =
    H.div_
        []
        [ H.h1_ [] ["Child textarea"]
        , H.div_
            []
            [ H.textarea_
                [ P.id_ "my-textarea"
                , E.onInput HandleTextareaInput
                ]
            ]
        , H.button_
            [E.onClick SendModelToParent]
            [text "I am a <button> in the textarea child, click me to send my Model to my parent!"]
        ]
