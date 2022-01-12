module Form.Game.GuessForm where

import Prologue
import AppM (AppM)
import Data.Cardano (CardanoWasm)
import Data.Cardano.Address (Address)
import Data.Cardano.Address as Address
import Data.Const (Const)
import Data.Either (hush)
import Data.Int (fromString)
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Formless as F
import Halogen (ClassName(ClassName))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (toEvent)

data Action
  = HandleSubmit Event

type GuessArgs
  = { guessArgsPayeeBech32Addr :: Address
    , guessArgsStartTime :: Int
    , guessArgsSecret :: String
    }

data AddressError
  = RecipientAddrNotInBech32

data POSIXTimeError
  = POSIXTimeNegative
  | POSIXTimeNotAnInt

newtype GuessArgsForm (r :: Row Type -> Type) f
  = GuessArgsForm
  ( r
      ( guessArgsPayeeBech32Addr :: f AddressError String Address
      , guessArgsStartTime :: f POSIXTimeError String Int
      , guessArgsSecret :: f Unit String String
      )
  )

derive instance newtypeGuessArgsForm :: Newtype (GuessArgsForm r f) _

component :: CardanoWasm -> F.Component GuessArgsForm (Const Void) () Unit GuessArgs AppM
component cardanoWasm = F.component (const $ input cardanoWasm) spec

input :: forall m. Monad m => CardanoWasm -> F.Input' GuessArgsForm m
input cardanoWasm =
  { initialInputs:
      Just
        ( F.wrapInputFields
            { guessArgsPayeeBech32Addr: "addr_test1qpvagl6ns8gfe7f0w74nylkyht53hlfhsvhpgvsf45c0cr4j2ftykgcgwpq4hma4mtjupynzky5pg9p9qzalu8zu3d6qscr5vz"
            , guessArgsStartTime: "0"
            , guessArgsSecret: ""
            }
        )
  , validators:
      GuessArgsForm
        { guessArgsPayeeBech32Addr:
            F.hoistFnE_ \str -> do
              let
                addrE = (Address.fromBech32 str) cardanoWasm
              case hush addrE of
                Nothing -> Left RecipientAddrNotInBech32
                Just addr -> Right addr
        , guessArgsStartTime:
            F.hoistFnE_ \str -> case fromString str of
              Nothing -> Left POSIXTimeNotAnInt
              Just n
                | n < 0 -> Left POSIXTimeNegative
                | otherwise -> Right n
        , guessArgsSecret:
            F.hoistFnE_ \str -> Right str
        }
  }

spec ::
  forall input m.
  MonadEffect m =>
  MonadAff m =>
  F.Spec GuessArgsForm () (Const Void) Action () input GuessArgs m
spec = F.defaultSpec { render = render, handleEvent = handleEvent, handleAction = handleAction }
  where
  handleEvent = F.raiseResult

  handleAction = case _ of
    HandleSubmit event -> do
      H.liftEffect $ Event.preventDefault event
      F.handleAction handleAction handleEvent F.submit

  render { form } =
    HH.form_
      [ HH.fieldset [ HP.class_ <<< ClassName $ "ba b--transparent ph0 mh0" ]
          [ HH.div [ HP.class_ <<< ClassName $ "mt3" ]
              [ HH.label [ HP.class_ <<< ClassName $ "db fw4 lh-copy f6" ] [ HH.text "Payee's Bech32 address" ]
              , HH.input
                  [ HP.class_ <<< ClassName $ "pa2 input-reset ba bg-transparent w-100"
                  , HP.value $ F.getInput _guessArgsPayeeBech32Addr form
                  , HE.onValueInput $ F.setValidate _guessArgsPayeeBech32Addr
                  ]
              , HH.p [ HP.class_ <<< ClassName $ "light-red" ]
                  [ HH.text case F.getError _guessArgsPayeeBech32Addr form of
                      Nothing -> ""
                      Just RecipientAddrNotInBech32 -> "The provided Cardano address is not in Bech32 format."
                  ]
              ]
          , HH.div [ HP.class_ <<< ClassName $ "mt3" ]
              [ HH.label [ HP.class_ <<< ClassName $ "db fw4 lh-copy f6" ] [ HH.text "Starting POSIX time" ]
              , HH.input
                  [ HP.class_ <<< ClassName $ "pa2 input-reset ba bg-transparent"
                  , HP.type_ HP.InputNumber
                  , HP.value $ F.getInput _guessArgsStartTime form
                  , HE.onValueInput $ F.setValidate _guessArgsStartTime
                  ]
              , HH.p [ HP.class_ <<< ClassName $ "light-red" ]
                  [ HH.text case F.getError _guessArgsStartTime form of
                      Nothing -> ""
                      Just POSIXTimeNotAnInt -> "Given POSIX time is not an number."
                      Just POSIXTimeNegative -> "Given POSIX time is a negative number."
                  ]
              ]
          , HH.div [ HP.class_ <<< ClassName $ "mt3" ]
              [ HH.label [ HP.class_ <<< ClassName $ "db fw4 lh-copy f6" ] [ HH.text "Secret word" ]
              , HH.input
                  [ HP.class_ <<< ClassName $ "pa2 input-reset ba bg-transparent w-100"
                  , HP.value $ F.getInput _guessArgsSecret form
                  , HE.onValueInput $ F.setValidate _guessArgsSecret
                  ]
              ]
          ]
      , HH.div [ HP.class_ <<< ClassName $ "mt3" ]
          [ HH.button
              [ HP.class_ <<< ClassName $ "b ph3 pv2 input-reset ba b--black bg-transparent grow pointer f6"
              , HE.onClick \e -> F.injAction $ HandleSubmit $ toEvent e
              ]
              [ HH.text "Guess"
              ]
          ]
      ]

  _guessArgsPayeeBech32Addr = Proxy :: Proxy "guessArgsPayeeBech32Addr"

  _guessArgsStartTime = Proxy :: Proxy "guessArgsStartTime"

  _guessArgsSecret = Proxy :: Proxy "guessArgsSecret"
