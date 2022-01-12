module Game where

import Prologue
import Halogen.HTML.Events as HE
import API.Contract (fetchContractPartialTx)
import API.Wallet (balanceSignAndSubmitTx)
import API.Cardano (getAddressKeyHashes)
import Affjax (post, printError) as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import AppM (AppM, Env)
import Cardano.Wallet.Nami (WalletId)
import Halogen.Css (classNames)
import Cardano.Wallet.Nami as Nami
import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Effect.Class.Console (log)
import Control.Monad.Reader (class MonadAsk)
import Data.Argonaut.Core as A
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Cardano (CardanoWasm)
import Data.Either (either)
import Data.Int (toNumber)
import Data.Set (Set)
import Data.Set as Set
import Effect.Aff (Milliseconds(Milliseconds), delay, error, throwError)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Type.Proxy (Proxy(..))
import Foreign.Object as Object
import Formless as F
import Form.Game.LockForm (LockArgs, LockArgsForm)
import Form.Game.LockForm as LockForm
import Form.Game.GuessForm (GuessArgs, GuessArgsForm)
import Form.Game.GuessForm as GuessForm
import Halogen (ClassName(ClassName), defaultEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Text.Pretty (class Pretty, pretty, text)

type State
  = { cardanoWasm :: CardanoWasm
    , errors :: Set AppError
    , isSubmittingTx :: Boolean
    , lastSubmittedTxId :: Maybe String
    , selectedEndpoint :: GameEndpoint
    }

data GameEndpoint
  = Lock
  | Guess

derive instance eqGameEndpoint :: Eq GameEndpoint

data Action
  = LockFunds LockArgs
  | UnlockFunds GuessArgs
  | SelectEndpoint GameEndpoint

data AppError
  = WalletConnectionError
  | LockFundsError
  | UnlockFundsError

derive instance eqAppError :: Eq AppError

derive instance ordAppError :: Ord AppError

instance prettyAppError :: Pretty AppError where
  pretty WalletConnectionError = text "There was an error while trying to connect to the browser wallet"
  pretty LockFundsError = text "There was an error while trying to lock the funds."
  pretty UnlockFundsError = text "There was an error while trying to unlock the funds."

component :: forall query output. H.Component query CardanoWasm output AppM
component = do
  H.mkComponent
    { initialState: \cardanoWasm -> initialState cardanoWasm
    , render
    , eval:
        H.mkEval
          defaultEval
            { handleAction = handleAction
            , initialize = Nothing
            }
    }

initialState :: CardanoWasm -> State
initialState cardanoWasm = do
  { cardanoWasm
  , errors: Set.empty
  , isSubmittingTx: false
  , lastSubmittedTxId: Nothing
  , selectedEndpoint: Lock
  }

type ChildSlots
  = ( lockForm :: F.Slot' LockArgsForm LockArgs Unit
    , guessForm :: F.Slot' GuessArgsForm GuessArgs Unit
    )

render :: State -> HH.HTML (H.ComponentSlot ChildSlots AppM Action) Action
render s =
  HH.div [ HP.class_ <<< ClassName $ "black-80 game" ]
    $ [ HH.nav
          [ classNames [ "flex" ] ]
          [ HH.span [ classNames [ "navbar-text" ] ] [ HH.text "Endpoints" ]
          , HH.ul
              [ classNames [ "flex" ] ]
              [ HH.li_
                  [ HH.a
                      [ if s.selectedEndpoint == Lock then
                          classNames [ "nav-link", "active" ]
                        else
                          classNames [ "nav-link" ]
                      , HE.onClick \_ -> SelectEndpoint Lock
                      ]
                      [ HH.text "Lock" ]
                  ]
              , HH.li_
                  [ HH.a
                      [ if s.selectedEndpoint == Guess then
                          classNames [ "nav-link", "active" ]
                        else
                          classNames [ "nav-link" ]
                      , HE.onClick \_ -> SelectEndpoint Guess
                      ]
                      [ HH.text "Unlock" ]
                  ]
              ]
          ]
      , getSlotComponent s.selectedEndpoint
      , modal s.isSubmittingTx (modalView s "Currently submitting a transaction to the Cardano blockchain testnet...")
      ]
    <> submittedLockTransactionHtml
    <> [ HH.div_
          [ HH.ul_
              $ map (\e -> HH.li [ HP.class_ <<< ClassName $ "light-red" ] [ HH.text $ show $ pretty e ])
              $ Set.toUnfoldable s.errors
          ]
      ]
  where
  getSlotComponent Lock = HH.slot (Proxy :: Proxy "lockForm") unit (LockForm.component s.cardanoWasm) unit LockFunds

  getSlotComponent Guess = HH.slot (Proxy :: Proxy "guessForm") unit (GuessForm.component s.cardanoWasm) unit UnlockFunds

  submittedLockTransactionHtml = case s.lastSubmittedTxId of
    Nothing -> []
    Just txId ->
      [ HH.div_
          [ HH.p_
              [ HH.text "Successfully submitted "
              , HH.a
                  [ HP.href $ "https://testnet.cardanoscan.io/transaction/" <> txId ]
                  [ HH.text "the transaction " ]
              , HH.text "to the Cardano testnet (need to wait a bit for the transaction to appear)."
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
-- | Makes a payment given the recipient's bech32 addresses and the amount in Lovelace
handleAction (LockFunds lockArgs) = do
  H.modify_ \s -> s { isSubmittingTx = true, lastSubmittedTxId = Nothing }
  walletIdE <- H.liftAff $ try Nami.getWalletId
  case walletIdE of
    Left err -> do
      log $ show err
      H.modify_ \s ->
        s
          { errors = Set.singleton WalletConnectionError
          }
    Right walletId -> do
      lockFundsRes <- H.lift $ try $ lockFunds walletId lockArgs
      case lockFundsRes of
        Left err -> do
          log $ show err
          H.modify_ \s ->
            s
              { isSubmittingTx = false
              , lastSubmittedTxId = Nothing
              , errors = Set.singleton LockFundsError
              }
        Right txId -> do
          H.modify_ \s ->
            s
              { isSubmittingTx = false
              , lastSubmittedTxId = Just txId
              , errors = Set.empty :: Set AppError
              }

handleAction (UnlockFunds guessArgs) = do
  H.modify_ \s -> s { isSubmittingTx = true, lastSubmittedTxId = Nothing }
  walletIdE <- H.liftAff $ try Nami.getWalletId
  case walletIdE of
    Left err -> do
      log $ show err
      H.modify_ \s ->
        s
          { errors = Set.singleton WalletConnectionError
          }
    Right walletId -> do
      guessFundsRes <- H.lift $ try $ unlockFunds walletId guessArgs
      case guessFundsRes of
        Left err -> do
          log $ show err
          H.modify_ \s ->
            s
              { isSubmittingTx = false
              , lastSubmittedTxId = Nothing
              , errors = Set.singleton UnlockFundsError
              }
        Right txId -> do
          H.modify_ \s ->
            s
              { isSubmittingTx = false
              , lastSubmittedTxId = Just txId
              , errors = Set.empty :: Set AppError
              }

handleAction (SelectEndpoint e) = H.modify_ \s -> s { selectedEndpoint = e }

-- | Given a 'WalletId', an 'Address' and an amount in lovelace, use the PAB and
-- Nami wallet to balance, sign and submit a transaction.
lockFunds ::
  forall m.
  MonadAff m =>
  MonadThrow Error m =>
  MonadAsk CardanoWasm m =>
  WalletId ->
  LockArgs ->
  m String
lockFunds walletId lockArgs = do
  cid <- activateGameContract walletId
  callLockEndpoint cid lockArgs
  -- Need to wait a bit to ensure the endpoint correctly handled
  -- the request and modifies the constract instance's status
  H.liftAff $ delay $ Milliseconds 2000.0
  partialCborTx <- fetchContractPartialTx cid
  balanceSignAndSubmitTx partialCborTx

-- | Given a 'WalletId', an 'Address' and an amount in lovelace, use the PAB and
-- Nami wallet to balance, sign and submit a transaction.
unlockFunds ::
  forall m.
  MonadAff m =>
  MonadThrow Error m =>
  MonadAsk CardanoWasm m =>
  WalletId ->
  GuessArgs ->
  m String
unlockFunds walletId lockArgs = do
  -- TODO: Should only activate contract if doesn't already exist
  cid <- activateGameContract walletId
  callGuessEndpoint cid lockArgs
  -- Need to wait a bit to ensure the endpoint correctly handled
  -- the request and modifies the constract instance's status
  H.liftAff $ delay $ Milliseconds 2000.0
  partialCborTx <- fetchContractPartialTx cid
  balanceSignAndSubmitTx partialCborTx

-- | Activate the 'Game' contract in the PAB.
-- TODO: Movie to 'API.Contract' when we can use the psgenerator again.
activateGameContract ::
  forall m.
  MonadAff m =>
  MonadThrow Error m =>
  Nami.WalletId ->
  m String
activateGameContract walletId = do
  let
    body =
      A.fromObject
        ( Object.fromFoldable
            [ Tuple "caID" (A.fromString "Game")
            , Tuple "caWallet"
                ( A.fromObject
                    ( Object.fromFoldable
                        [ Tuple "getWalletId" (A.fromString walletId.unWalletId) ]
                    )
                )
            ]
        )
  resE <-
    H.liftAff
      $ AX.post ResponseFormat.json
          "/api/contract/activate"
          (Just (RequestBody.json body))
  res <- either (throwError <<< error <<< AX.printError) pure resE
  either (throwError <<< error <<< show)
    (\c -> pure $ c.unContractInstanceId)
    $ (decodeJson (res.body) :: Either JsonDecodeError { unContractInstanceId :: String })

-- | Call the lock endpoint of our contract.
callLockEndpoint ::
  forall m.
  MonadAff m =>
  MonadThrow Error m =>
  MonadAsk CardanoWasm m =>
  String ->
  LockArgs ->
  m Unit
callLockEndpoint cid lockArgs = do
  Tuple paymentKeyHash _ <- getAddressKeyHashes lockArgs.lockArgsPayeeBech32Addr
  -- TODO: Use the generated PS types from plutus-ledger to construct the JSON instead
  let
    endpointCallBody =
      A.fromObject
        ( Object.fromFoldable
            [ Tuple "lockArgsGameParam"
                ( A.fromObject
                    ( Object.fromFoldable
                        [ Tuple "gameParamPayeePkh"
                            ( A.fromObject
                                ( Object.fromFoldable
                                    [ Tuple "unPaymentPubKeyHash"
                                        ( A.fromObject
                                            ( Object.fromFoldable
                                                [ Tuple "getPubKeyHash" (A.fromString paymentKeyHash) ]
                                            )
                                        )
                                    ]
                                )
                            )
                        , Tuple "gameParamStartTime"
                            (A.fromNumber $ toNumber lockArgs.lockArgsStartTime)
                        ]
                    )
                )
            , Tuple "lockArgsSecret" (A.fromString lockArgs.lockArgsSecret)
            , Tuple "lockArgsValue"
                ( A.fromObject
                    ( Object.fromFoldable
                        [ Tuple "getValue"
                            ( A.fromArray
                                [ A.fromArray
                                    [ A.fromObject $ Object.fromFoldable [ Tuple "unCurrencySymbol" (A.fromString "") ]
                                    , A.fromArray
                                        [ A.fromArray
                                            [ A.fromObject $ Object.fromFoldable [ Tuple "unTokenName" (A.fromString "") ]
                                            , A.fromNumber $ toNumber lockArgs.lockArgsLovelace
                                            ]
                                        ]
                                    ]
                                ]
                            )
                        ]
                    )
                )
            ]
        )
  resE <-
    H.liftAff do
      AX.post ResponseFormat.json
        ("/api/contract/instance/" <> cid <> "/endpoint/lock")
        (Just (RequestBody.json endpointCallBody))
  either (throwError <<< error <<< AX.printError) (const $ pure unit) resE

-- | Call the lock endpoint of our contract.
callGuessEndpoint ::
  forall m.
  MonadAff m =>
  MonadThrow Error m =>
  MonadAsk CardanoWasm m =>
  String ->
  GuessArgs ->
  m Unit
callGuessEndpoint cid guessArgs = do
  Tuple paymentKeyHash _ <- getAddressKeyHashes guessArgs.guessArgsPayeeBech32Addr
  -- TODO: Use the generated PS types from plutus-ledger to construct the JSON instead
  let
    endpointCallBody =
      A.fromObject
        ( Object.fromFoldable
            [ Tuple "guessArgsGameParam"
                ( A.fromObject
                    ( Object.fromFoldable
                        [ Tuple "gameParamPayeePkh"
                            ( A.fromObject
                                ( Object.fromFoldable
                                    [ Tuple "unPaymentPubKeyHash"
                                        ( A.fromObject
                                            ( Object.fromFoldable
                                                [ Tuple "getPubKeyHash" (A.fromString paymentKeyHash) ]
                                            )
                                        )
                                    ]
                                )
                            )
                        , Tuple "gameParamStartTime"
                            (A.fromNumber $ toNumber guessArgs.guessArgsStartTime)
                        ]
                    )
                )
            , Tuple "guessArgsSecret" (A.fromString guessArgs.guessArgsSecret)
            ]
        )
  resE <-
    H.liftAff do
      AX.post ResponseFormat.json
        ("/api/contract/instance/" <> cid <> "/endpoint/guess")
        (Just (RequestBody.json endpointCallBody))
  either (throwError <<< error <<< AX.printError) (const $ pure unit) resE
