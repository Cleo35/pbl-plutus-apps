module Main where

import Prologue
import AppM (AppM, Env, runAppM)
import Cardano.Wallet.Nami as Nami
import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Control.Monad.Reader (class MonadAsk)
import Data.Cardano (CardanoWasm, loadCardanoWasm)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Type.Proxy (Proxy(..))
import Formless as F
import Halogen (ClassName(ClassName), defaultEval)
import Halogen.Css (classNames)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Text.Pretty (class Pretty, pretty, text)
import PayToWallet as PayToWallet
import Game as Game

data Contract
  = PayToWallet
  | Game

derive instance eqContract :: Eq Contract

instance prettyContract :: Pretty Contract where
  pretty PayToWallet = text "PayToWallet"
  pretty Game = text "Game"

type State
  = { cardanoWasm :: CardanoWasm
    , errors :: Set AppError
    , isLoadingWallet :: Boolean
    , selectedContract :: Contract
    }

data Action
  = ConnectWallet
  | SelectContract Contract

data AppError
  = WalletConnectionError

derive instance eqAppError :: Eq AppError

derive instance ordAppError :: Ord AppError

instance prettyAppError :: Pretty AppError where
  pretty WalletConnectionError = text "There was an error while trying to connect to the browser wallet"

main :: Effect Unit
main = do
  runHalogenAff do
    cardanoWasm <- loadCardanoWasm
    body <- awaitBody
    _ <- runUI (H.hoist (runAppM cardanoWasm) (mainComponent cardanoWasm)) unit body
    pure unit

mainComponent :: forall query input output. CardanoWasm -> H.Component query input output AppM
mainComponent cardanoWasm = do
  H.mkComponent
    { initialState: const $ initialState cardanoWasm
    , render
    , eval:
        H.mkEval
          defaultEval
            { handleAction = handleAction
            , initialize = Just ConnectWallet
            }
    }

initialState :: CardanoWasm -> State
initialState cardanoWasm = do
  { cardanoWasm
  , errors: Set.empty
  , isLoadingWallet: Nami.isEnabled
  , selectedContract: Game
  }

type ChildSlots
  = ( payToWallet :: forall query. F.Slot' query Void Int
    , game :: forall query. F.Slot' query Void Int
    )

_payToWallet = Proxy :: Proxy "payToWallet"

_game = Proxy :: Proxy "game"

render :: State -> HH.HTML (H.ComponentSlot ChildSlots AppM Action) Action
render s =
  HH.div
    [ classNames [ "frames", "pa4", "black-80" ] ]
    $ [ HH.h1_ [ HH.text "PAB-Nami demo" ]
      , HH.p_
          [ HH.text "This simple demo allows you to test some contracts using the PAB and the Nami wallet." ]
      , navPane s
      , HH.h2_ [ HH.text $ show $ pretty s.selectedContract ]
      , getSlotComponent s.selectedContract
      , modal s.isLoadingWallet (modalView s "Connecting to Nami...")
      ]
    <> [ HH.div_
          [ HH.ul_
              $ map (\e -> HH.li [ HP.class_ <<< ClassName $ "light-red" ] [ HH.text $ show $ pretty e ])
              $ Set.toUnfoldable s.errors
          ]
      ]
  where
  getSlotComponent PayToWallet = HH.slot_ _payToWallet 0 PayToWallet.component s.cardanoWasm

  getSlotComponent Game = HH.slot_ _game 1 Game.component s.cardanoWasm

navPane :: forall p. State -> HH.HTML p Action
navPane s =
  HH.nav
    [ classNames [ "flex" ] ]
    [ HH.span [ classNames [ "navbar-text" ] ] [ HH.text "Contracts" ]
    , HH.ul
        [ classNames [ "flex" ] ]
        [ HH.li_
            [ HH.a
                [ if s.selectedContract == PayToWallet then
                    classNames [ "nav-link", "active" ]
                  else
                    classNames [ "nav-link" ]
                , HE.onClick \_ -> SelectContract PayToWallet
                ]
                [ HH.text "PayToWallet" ]
            ]
        , HH.li_
            [ HH.a
                [ if s.selectedContract == Game then
                    classNames [ "nav-link", "active" ]
                  else
                    classNames [ "nav-link" ]
                , HE.onClick \_ -> SelectContract Game
                ]
                [ HH.text "Game" ]
            ]
        ]
    ]

modal ::
  Boolean ->
  HH.HTML (H.ComponentSlot ChildSlots AppM Action) Action ->
  HH.HTML (H.ComponentSlot ChildSlots AppM Action) Action
modal isVisible view =
  let
    displayClass = if isVisible then "flex" else "dn"

    containerClasses =
      "bottom-0 fixed items-center justify-center left-0 right-0 top-0"
        <> " "
        <> displayClass
  in
    HH.div [ HP.class_ <<< ClassName $ containerClasses ]
      [ HH.div
          [ HP.class_ <<< ClassName $ "bg-white flex items-center justify-center mw7 relative w-90 z-4" ]
          [ view ]
      , HH.div
          [ HP.class_ <<< ClassName $ "bg-black bottom-0 fixed left-0 o-80 right-0 top-0 z-1" ]
          []
      ]

modalView :: State -> String -> HH.HTML (H.ComponentSlot ChildSlots AppM Action) Action
modalView _ text =
  HH.div [ HP.class_ <<< ClassName $ "flex flex-column items-center justify-center pa6" ]
    [ HH.h1_ [ HH.text text ]
    , HH.div [ HP.class_ <<< ClassName $ "loader" ] []
    ]

handleAction ::
  forall msg m.
  MonadAsk Env m =>
  MonadAff m =>
  MonadThrow Error m =>
  MonadError Error m =>
  Action ->
  H.HalogenM State Action ChildSlots msg m Unit
-- | Connect to the Nami wallet
handleAction ConnectWallet = do
  enableRes <- H.lift $ try $ void $ Nami.enable
  H.modify_ \s -> s { isLoadingWallet = false }
  case enableRes of
    Left _ -> H.modify_ \s -> s { errors = Set.singleton WalletConnectionError }
    Right _ -> pure unit

handleAction (SelectContract contract) = do
  H.modify_ \s -> s { selectedContract = contract }
