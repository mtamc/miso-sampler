{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

{- HLINT ignore "Use newtype instead of data" -}

module Main where

import Data.Maybe (fromMaybe)
import Miso hiding (Child, Parent)
import qualified Miso.CSS as CSS
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
import Miso.Lens
import qualified Miso.String

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

app = grandparent

(%) = flip compose

----------------------------------------------------------------------------------------------------

grandparent :: App Gp GpAction
grandparent = component (Gp True) updateModel viewModel

data Gp = Gp {_validate :: Bool} deriving (Show, Eq)
validateL = lens _validate (\x y -> x{_validate = y})

data GpAction = ToggleValidate Checked deriving (Show, Eq)

updateModel :: GpAction -> Effect parentAction parent Gp GpAction
updateModel (ToggleValidate (Checked bool)) = validateL .= bool

viewModel :: Maybe ROOT -> Gp -> View Gp GpAction
viewModel _ mdl =
    H.div_
        [ CSS.style_ [CSS.border "4px solid blue", CSS.padding "10px", CSS.color CSS.blue]
        ]
        [ H.h1_ [] ["Grandparent"]
        , -- , H.div_ [] [text ("(DEBUG: My model is: " <> ms (show mdl) <> ")")]
          H.div_
            []
            [ H.input_
                [ P.type_ "checkbox"
                , P.checked_ (mdl ^. validateL)
                , E.onChecked ToggleValidate
                ]
            , "Enable validation"
            ]
        , H.div_
            [CSS.style_ [CSS.fontWeight "bold", CSS.marginTop "30px"]]
            [text "(DEBUG: The prop I want to pass is: validate = ", text (ms (show (mdl ^. validateL))), ")"]
        , Miso.mountProps_ "parent" (mdl ^. validateL) parentComponent
        ]

----------------------------------------------------------------------------------------------------

parentComponent :: Component grandparent grandparentAction Bool Parent ParentAction
parentComponent = component initParent updateParent viewParent

initParent :: Parent
initParent = Parent "" initChild

data Parent = Parent
    { _text :: MisoString
    , _child :: Child
    }
    deriving (Show, Eq)
textL :: Lens Parent MisoString
textL = lens _text (\x y -> x{_text = y})
childL :: Lens Parent Child
childL = lens _child (\x y -> x{_child = y})

data ParentAction = OnInput MisoString deriving (Show, Eq)

updateParent :: ParentAction -> Effect parentAction parent Parent ParentAction
updateParent (OnInput str) = textL .= str

viewParent :: Maybe Bool -> Parent -> View Parent ParentAction
viewParent mprops mdl =
    let validate = mprops & fromMaybe False
        textLen = Miso.String.length (mdl ^. textL)
        counterVal = mdl ^. childL % counterL
        typedText = mdl ^. textL
        validation =
            if
                | not validate -> []
                | counterVal > textLen -> ["Your estimate is too high!"]
                | counterVal < textLen -> ["Your estimate is too low!"]
                | otherwise -> []
     in H.div_
            [ CSS.style_
                [ CSS.border "4px solid green"
                , CSS.padding "10px"
                , CSS.margin "20px"
                , CSS.color CSS.green
                ]
            ]
            [ H.div_
                [CSS.style_ [CSS.fontWeight "bold"]]
                [text ("(DEBUG: The prop I received from my parent is: validate = " <> ms (show mprops) <> ")")]
            , H.h1_ [] ["Parent"]
            , -- , H.div_ [] [text ("(DEBUG: My model is: " <> ms (show mdl) <> ")")]
              H.div_
                []
                [ "Your Text "
                , H.input_
                    [ P.type_ "text"
                    , P.value_ typedText
                    , E.onInput OnInput
                    ]
                ]
            , H.div_ [] ["You have typed: ", text typedText]
            , H.div_
                [CSS.style_ [CSS.fontWeight "bold", CSS.marginTop "40px"]]
                ["(DEBUG: The prop I want to pass is: validation = ", text (ms (show mprops)), ")"]
            , Miso.mountProps_ "child" validation (childComponent childL)
            ]

----------------------------------------------------------------------------------------------------

childComponent :: Lens parent Child -> Component parent parentAction [MisoString] Child ChildAction
childComponent lensParentToChild =
    (component (Child 0) updateChild viewChild)
        { bindings = [lensParentToChild <--> this]
        }

initChild :: Child
initChild = Child 0

data Child = Child
    { _counter :: Int
    }
    deriving (Show, Eq)
counterL = lens _counter (\x y -> x{_counter = y})

data ChildAction = Incr | Decr deriving (Show, Eq)

updateChild :: ChildAction -> Effect parentAction parent Child ChildAction
updateChild = \case
    Incr -> counterL += 1
    Decr -> counterL -= 1

viewChild :: Maybe [MisoString] -> Child -> View parent ChildAction
viewChild mprops mdl =
    let validation = mprops & fromMaybe []
     in H.div_
            [ CSS.style_
                [ CSS.border "4px solid teal"
                , CSS.padding "10px"
                , CSS.margin "20px"
                , CSS.color CSS.teal
                ]
            ]
            [ H.div_
                [CSS.style_ [CSS.fontWeight "bold"]]
                [text ("(DEBUG: The prop I received from my parent is: validation = " <> ms (show mprops) <> ")")]
            , H.h1_ [] ["Child"]
            , H.div_ [] ["Estimate how manu characters are in the text input above:"]
            , H.div_
                []
                [ text (ms (show (mdl ^. counterL)))
                , " | "
                , H.button_ [E.onClick Incr] ["+1"]
                , H.button_ [E.onClick Decr] ["-1"]
                ]
            , H.div_
                [ CSS.style_ [CSS.color CSS.red, CSS.marginTop "10px"]
                ]
                [ "Validation errors: "
                , case validation of
                    [] -> "None"
                    _ -> H.ul_ [] (map (H.li_ [] . pure . text) validation)
                ]
            ]
