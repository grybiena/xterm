module Example.Widget.Button where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.Event.Event (Event, stopPropagation)
import Web.UIEvent.MouseEvent (toEvent)

type Slots :: forall k. Row k
type Slots = ()

data Action = Click Event

data Output = ButtonClicked

component :: forall q s m. MonadAff m => H.Component q s Output m
component = do
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     }
    }

render :: forall m. MonadAff m => Unit -> H.ComponentHTML Action Slots m
render _ = HH.button [ HE.onClick (Click <<< toEvent)] [HH.text "Click me!"] 

handleAction :: forall s m .
                MonadAff m
             => Action
             -> H.HalogenM s Action Slots Output m Unit
handleAction = case _ of
  Click e -> do
    H.liftEffect $ stopPropagation e
    H.raise ButtonClicked


