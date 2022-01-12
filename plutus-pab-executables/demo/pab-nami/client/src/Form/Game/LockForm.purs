module Form.Game.LockForm where

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

type LockArgs
  = { lockArgsPayeeBech32Addr :: Address
    , lockArgsStartTime :: Int
    , lockArgsSecret :: String
    , lockArgsLovelace :: Int
    }

data AddressError
  = RecipientAddrNotInBech32

data POSIXTimeError
  = POSIXTimeNegative
  | POSIXTimeNotAnInt

data LovelaceAmountError
  = LovelaceAmountLowerThanMin
  | LovelaceAmountNotAnInt

newtype LockArgsForm (r :: Row Type -> Type) f
  = LockArgsForm
  ( r
      ( lockArgsPayeeBech32Addr :: f AddressError String Address
      , lockArgsStartTime :: f POSIXTimeError String Int
      , lockArgsSecret :: f Unit String String
      , lockArgsLovelace :: f LovelaceAmountError String Int
      )
  )

derive instance newtypeLockArgsForm :: Newtype (LockArgsForm r f) _

component :: CardanoWasm -> F.Component LockArgsForm (Const Void) () Unit LockArgs AppM
component cardanoWasm = F.component (const $ input cardanoWasm) spec

input :: forall m. Monad m => CardanoWasm -> F.Input' LockArgsForm m
input cardanoWasm =
  { initialInputs:
      Just
        ( F.wrapInputFields
            { lockArgsPayeeBech32Addr: "addr_test1qpvagl6ns8gfe7f0w74nylkyht53hlfhsvhpgvsf45c0cr4j2ftykgcgwpq4hma4mtjupynzky5pg9p9qzalu8zu3d6qscr5vz"
            , lockArgsStartTime: "0"
            , lockArgsSecret: "secret"
            , lockArgsLovelace: "2000000"
            }
        )
  , validators:
      LockArgsForm
        { lockArgsPayeeBech32Addr:
            F.hoistFnE_ \str -> do
              let
                addrE = (Address.fromBech32 str) cardanoWasm
              case hush addrE of
                Nothing -> Left RecipientAddrNotInBech32
                Just addr -> Right addr
        , lockArgsStartTime:
            F.hoistFnE_ \str -> case fromString str of
              Nothing -> Left POSIXTimeNotAnInt
              Just n
                | n < 0 -> Left POSIXTimeNegative
                | otherwise -> Right n
        , lockArgsSecret:
            F.hoistFnE_ \str -> Right str
        , lockArgsLovelace:
            F.hoistFnE_ \str -> case fromString str of
              Nothing -> Left LovelaceAmountNotAnInt
              Just n
                | n < 2000000 -> Left LovelaceAmountLowerThanMin
                | otherwise -> Right n
        }
  }

spec :: forall input m. MonadEffect m => MonadAff m => F.Spec LockArgsForm () (Const Void) Action () input LockArgs m
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
                  , HP.value $ F.getInput _lockArgsPayeeBech32Addr form
                  , HE.onValueInput $ F.setValidate _lockArgsPayeeBech32Addr
                  ]
              , HH.p [ HP.class_ <<< ClassName $ "light-red" ]
                  [ HH.text case F.getError _lockArgsPayeeBech32Addr form of
                      Nothing -> ""
                      Just RecipientAddrNotInBech32 -> "The provided Cardano address is not in Bech32 format."
                  ]
              ]
          , HH.div [ HP.class_ <<< ClassName $ "mt3" ]
              [ HH.label [ HP.class_ <<< ClassName $ "db fw4 lh-copy f6" ] [ HH.text "Starting POSIX time" ]
              , HH.input
                  [ HP.class_ <<< ClassName $ "pa2 input-reset ba bg-transparent"
                  , HP.type_ HP.InputNumber
                  , HP.value $ F.getInput _lockArgsStartTime form
                  , HE.onValueInput $ F.setValidate _lockArgsStartTime
                  ]
              , HH.p [ HP.class_ <<< ClassName $ "light-red" ]
                  [ HH.text case F.getError _lockArgsStartTime form of
                      Nothing -> ""
                      Just POSIXTimeNotAnInt -> "Given POSIX time is not an number."
                      Just POSIXTimeNegative -> "Given POSIX time is a negative number."
                  ]
              ]
          , HH.div [ HP.class_ <<< ClassName $ "mt3" ]
              [ HH.label [ HP.class_ <<< ClassName $ "db fw4 lh-copy f6" ] [ HH.text "Secret word" ]
              , HH.input
                  [ HP.class_ <<< ClassName $ "pa2 input-reset ba bg-transparent w-100"
                  , HP.value $ F.getInput _lockArgsSecret form
                  , HE.onValueInput $ F.setValidate _lockArgsSecret
                  ]
              -- , HH.p [ HP.class_ <<< ClassName $ "light-red" ]
              --     [ HH.text case F.getError _lockArgsSecret form of
              --         Nothing -> ""
              --         Just RecipientAddrNotInBech32 -> "The provided secret is not valid."
              --     ]
              ]
          , HH.div [ HP.class_ <<< ClassName $ "mt3" ]
              [ HH.label [ HP.class_ <<< ClassName $ "db fw4 lh-copy f6" ] [ HH.text "Lovelace" ]
              , HH.input
                  [ HP.class_ <<< ClassName $ "pa2 input-reset ba bg-transparent"
                  , HP.type_ HP.InputNumber
                  , HP.value $ F.getInput _lockArgsLovelace form
                  , HE.onValueInput $ F.setValidate _lockArgsLovelace
                  ]
              , HH.p [ HP.class_ <<< ClassName $ "light-red" ]
                  [ HH.text case F.getError _lockArgsLovelace form of
                      Nothing -> ""
                      Just LovelaceAmountNotAnInt -> "The lovelace amount is not an number."
                      Just LovelaceAmountLowerThanMin -> "The lovelace amount must be a minimum of 2 000 000 to satisfy a constraint in the Cardano blockchain."
                  ]
              ]
          ]
      , HH.div [ HP.class_ <<< ClassName $ "mt3" ]
          [ HH.button
              [ HP.class_ <<< ClassName $ "b ph3 pv2 input-reset ba b--black bg-transparent grow pointer f6"
              , HE.onClick \e -> F.injAction $ HandleSubmit $ toEvent e
              ]
              [ HH.text "Lock funds"
              ]
          ]
      ]

  _lockArgsPayeeBech32Addr = Proxy :: Proxy "lockArgsPayeeBech32Addr"

  _lockArgsStartTime = Proxy :: Proxy "lockArgsStartTime"

  _lockArgsSecret = Proxy :: Proxy "lockArgsSecret"

  _lockArgsLovelace = Proxy :: Proxy "lockArgsLovelace"
