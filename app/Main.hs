{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative (asum)
import GHC.Generics (Generic)
import Miso (App, Component (mailbox), Effect, MisoString, View, component, consoleLog, defaultEvents, io_, mailChildren, ms, reload, startApp, text, (+>))
import Miso.CSS qualified as CSS
import Miso.Html.Element qualified as H
import Miso.Html.Event qualified as E
import Miso.JSON (Value)
import Miso.JSON qualified
import Miso.Lens (Lens, lens, (.=), (^.))
import Miso.Lens.TH (makeLenses)
import PenpalChildCounter qualified
import PenpalChildTextarea qualified

data Model = Model
    { _childCounterWhenLastAsked :: Maybe PenpalChildCounter.Model
    , _childTextareaWhenLastAsked :: Maybe PenpalChildTextarea.Model
    }
    deriving (Show, Eq)
$(makeLenses ''Model)

initModel :: Model
initModel =
    Model
        { _childCounterWhenLastAsked = Nothing
        , _childTextareaWhenLastAsked = Nothing
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
app =
    (component initModel updateModel viewModel)
        { mailbox = receiveMail
        }

-- `Miso.checkMail isn't enough here because we want to try decoding the message
-- into all of our children's outbound message types`
receiveMail :: Value -> Maybe Action
receiveMail value =
    let parseAttempt =
            asum
                [ PenpalChildCounterMessage <$> Miso.JSON.fromJSON value
                , PenpalChildTextareaMessage <$> Miso.JSON.fromJSON value
                ]
     in Just $ case parseAttempt of
            Miso.JSON.Success inboundMessage ->
                HandleInboundMessage inboundMessage
            Miso.JSON.Error err ->
                HandleUnknownInboundMessage (ms err)

data Action
    = ResetChildCounter
    | ResetChildTextarea
    | HandleInboundMessage InboundMessage
    | HandleUnknownInboundMessage MisoString
    deriving (Show, Eq)

data InboundMessage
    = PenpalChildCounterMessage PenpalChildCounter.OutboundMessage
    | PenpalChildTextareaMessage PenpalChildTextarea.OutboundMessage
    deriving (Eq, Show, Generic)

updateModel :: Action -> Effect parent Model Action
updateModel = \case
    ResetChildCounter ->
        mailChildren PenpalChildCounter.ResetTheCounter
    ResetChildTextarea ->
        mailChildren PenpalChildTextarea.ResetTheTextarea
    HandleInboundMessage inboundMsg ->
        case inboundMsg of
            PenpalChildCounterMessage penpalChildCounterMessage ->
                case penpalChildCounterMessage of
                    PenpalChildCounter.HereIsMyModel childCounter ->
                        childCounterWhenLastAsked .= Just childCounter
            PenpalChildTextareaMessage penpalChildTextareaMessage ->
                case penpalChildTextareaMessage of
                    PenpalChildTextarea.HereIsMyTextareaModel childTextarea ->
                        childTextareaWhenLastAsked .= Just childTextarea
    HandleUnknownInboundMessage receivedError ->
        Miso.io_ . Miso.consoleLog $
            "Received non-parsing message with error: "
                <> receivedError

viewModel :: Model -> View Model Action
viewModel mdl =
    H.div_
        []
        [ H.span_ [] [text "I am a <span> in the parent, I can access the child model when the child sends me its OutboundMessage!"]
        , H.br_ []
        , H.span_
            []
            [ case mdl ^. childCounterWhenLastAsked of
                Nothing -> text "The child has not sent me its model yet."
                Just childCounter ->
                    text $ "The last time the child sent me its model, it was: " <> ms (show childCounter)
            ]
        , H.br_ []
        , H.br_ []
        , "I can also reset the child model using the child's InboundMessage!"
        , H.br_ []
        , H.button_ [E.onClick ResetChildCounter] [text "I am a <button> in the parent, click me to reset the child counter model!"]
        , H.div_
            [CSS.style_ [CSS.border "2px solid black", CSS.padding "10px", CSS.margin "10px"]]
            ["penpalChildCounter" +> PenpalChildCounter.component_]
        , H.hr_ [CSS.style_ [CSS.margin "20px 0"]]
        , H.span_ [] [text "It's me, the parent again! I'm demoing the exact same thing with a different child here in order to demonstrate how to receive messages from different children."]
        , H.br_ []
        , H.span_
            []
            [ case mdl ^. childTextareaWhenLastAsked of
                Nothing -> text "The child textarea has not sent me its model yet."
                Just childCounter ->
                    text $ "The last time the child textarea sent me its model, it was: " <> ms (show childCounter)
            ]
        , H.br_ []
        , H.button_ [E.onClick ResetChildTextarea] [text "I am a <button> in the parent, click me to reset the child textarea model!"]
        , H.div_
            [CSS.style_ [CSS.border "2px solid black", CSS.padding "10px", CSS.margin "10px"]]
            ["penpalChildTextarea" +> PenpalChildTextarea.component_]
        ]
