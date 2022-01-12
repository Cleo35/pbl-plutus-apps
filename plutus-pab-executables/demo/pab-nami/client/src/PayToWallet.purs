module PayToWallet where

import Prologue
import API.Contract (fetchContractPartialTx)
import API.Wallet (balanceSignAndSubmitTx)
import Affjax (post, printError) as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import AppM (AppM, Env)
import Cardano.Wallet.Nami (WalletId)
import Cardano.Wallet.Nami as Nami
import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Control.Monad.Reader (class MonadAsk)
import Data.Argonaut.Core as A
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Cardano (CardanoWasm)
import Data.Cardano.Address (Address)
import API.Cardano (getAddressKeyHashes)
import Data.Const (Const)
import Data.Either (either)
import Data.Int (toNumber)
import Data.Set (Set)
import Data.Set as Set
import Effect.Aff (Milliseconds(Milliseconds), delay, error, throwError)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Foreign.Object as Object
import Formless as F
import Halogen (ClassName(ClassName), defaultEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Form.PayToWallet.PaymentForm (Payment, PaymentForm, ChildSlots)
import Form.PayToWallet.PaymentForm as PaymentForm
import Text.Pretty (class Pretty, pretty, text)

type State
  = { cardanoWasm :: CardanoWasm
    , errors :: Set AppError
    , isLoadingWallet :: Boolean
    , isSubmittingPayment :: Boolean
    , lastSubmittedTxId :: Maybe String
    }

data Action
  = ConnectWallet
  | MakePayment Payment

data AppError
  = WalletConnectionError
  | MakePaymentAppError

derive instance eqAppError :: Eq AppError

derive instance ordAppError :: Ord AppError

instance prettyAppError :: Pretty AppError where
  pretty WalletConnectionError = text "There was an error while trying to connect to the browser wallet"
  pretty MakePaymentAppError = text "There was an error while submitting the payment."

component :: forall query output. H.Component query CardanoWasm output AppM
component = do
  H.mkComponent
    { initialState: \cardanoWasm -> initialState cardanoWasm
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
  , isSubmittingPayment: false
  , lastSubmittedTxId: Nothing
  }

type ChildSlot
  = ( formless :: F.Slot PaymentForm (Const Void) ChildSlots Payment Unit )

render :: State -> HH.HTML (H.ComponentSlot ChildSlots AppM Action) Action
render s =
  HH.div [ HP.class_ <<< ClassName $ "black-80" ]
    $ [ HH.slot F._formless unit (PaymentForm.component s.cardanoWasm) unit MakePayment
      , modal s.isSubmittingPayment (modalView s "Currently submitting the payment to the Cardano blockchain testnet...")
      ]
    <> submittedPaymentHtml
    <> [ HH.div_
          [ HH.ul_
              $ map (\e -> HH.li [ HP.class_ <<< ClassName $ "light-red" ] [ HH.text $ show $ pretty e ])
              $ Set.toUnfoldable s.errors
          ]
      ]
  where
  submittedPaymentHtml = case s.lastSubmittedTxId of
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
-- | Connect to the Nami wallet
handleAction ConnectWallet = do
  enableRes <- H.lift $ try $ void $ Nami.enable
  H.modify_ \s -> s { isLoadingWallet = false }
  case enableRes of
    Left _ -> H.modify_ \s -> s { errors = Set.singleton WalletConnectionError }
    Right _ -> pure unit

-- | Makes a payment given the recipient's bech32 addresses and the amount in Lovelace
handleAction (MakePayment payment) = do
  H.modify_ \s -> s { isSubmittingPayment = true, lastSubmittedTxId = Nothing }
  walletIdE <- H.liftAff $ try Nami.getWalletId
  case walletIdE of
    Left err -> do
      log $ show err
      H.modify_ \s ->
        s
          { isSubmittingPayment = false
          , lastSubmittedTxId = Nothing
          , errors = Set.singleton WalletConnectionError
          }
    Right walletId -> do
      makePaymentRes <-
        H.lift
          $ try
          $ makePayment
              walletId
              payment.recipientBech32Addr
              payment.lovelaceAmount
      case makePaymentRes of
        Left err -> do
          log $ show err
          H.modify_ \s ->
            s
              { isSubmittingPayment = false
              , lastSubmittedTxId = Nothing
              , errors = Set.singleton MakePaymentAppError
              }
        Right txId -> do
          H.modify_ \s ->
            s
              { isSubmittingPayment = false
              , lastSubmittedTxId = Just txId
              , errors = Set.empty :: Set AppError
              }

-- | Given a 'WalletId', an 'Address' and an amount in lovelace, use the PAB and
-- Nami wallet to balance, sign and submit a transaction.
makePayment ::
  forall m.
  MonadAff m =>
  MonadThrow Error m =>
  MonadAsk CardanoWasm m =>
  WalletId ->
  Address ->
  Int ->
  m String
makePayment walletId recipientAddr lovelaceAmount = do
  pubKeyHashesHex <- getAddressKeyHashes recipientAddr
  cid <- activateContract walletId
  callPayToWalletEndpoint cid pubKeyHashesHex lovelaceAmount
  -- Need to wait a bit to ensure the endpoint correctly handled
  -- the request and modifies the constract instance's status
  H.liftAff $ delay $ Milliseconds 2000.0
  partialCborTx <- fetchContractPartialTx cid
  balanceSignAndSubmitTx partialCborTx

-- | Activate the 'PayToWallet' contract in the PAB.
activateContract ::
  forall m.
  MonadAff m =>
  MonadThrow Error m =>
  Nami.WalletId ->
  m String
activateContract walletId = do
  let
    body =
      A.fromObject
        ( Object.fromFoldable
            [ Tuple "caID" (A.fromString "PayToWallet")
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

-- | Call the endpoint of our contract to generate a transaction for a payment.
callPayToWalletEndpoint ::
  forall m.
  MonadAff m =>
  MonadThrow Error m =>
  String ->
  Tuple String String ->
  Int ->
  m Unit
callPayToWalletEndpoint cid (Tuple paymentKeyHashHex stakeKeyHashHex) lovelaceAmount = do
  -- TODO: Use the generated PS types from plutus-ledger to construct the JSON instead
  let
    endpointCallBody =
      A.fromObject
        ( Object.fromFoldable
            [ Tuple "amount"
                ( A.fromObject
                    ( Object.fromFoldable
                        [ Tuple "getValue"
                            ( A.fromArray
                                [ A.fromArray
                                    [ A.fromObject $ Object.fromFoldable [ Tuple "unCurrencySymbol" (A.fromString "") ]
                                    , A.fromArray
                                        [ A.fromArray
                                            [ A.fromObject $ Object.fromFoldable [ Tuple "unTokenName" (A.fromString "") ]
                                            , A.fromNumber $ toNumber lovelaceAmount
                                            ]
                                        ]
                                    ]
                                ]
                            )
                        ]
                    )
                )
            , Tuple "pkh"
                ( A.fromObject
                    ( Object.fromFoldable
                        [ Tuple "unPaymentPubKeyHash"
                            ( A.fromObject
                                ( Object.fromFoldable
                                    [ Tuple "getPubKeyHash" (A.fromString paymentKeyHashHex) ]
                                )
                            )
                        ]
                    )
                )
            , Tuple "skh"
                ( A.fromObject
                    ( Object.fromFoldable
                        [ Tuple "unStakePubKeyHash"
                            ( A.fromObject
                                ( Object.fromFoldable
                                    [ Tuple "getPubKeyHash" (A.fromString stakeKeyHashHex) ]
                                )
                            )
                        ]
                    )
                )
            ]
        )
  resE <-
    H.liftAff do
      AX.post ResponseFormat.json
        ("/api/contract/instance/" <> cid <> "/endpoint/PayToWallet")
        (Just (RequestBody.json endpointCallBody))
  either (throwError <<< error <<< AX.printError) (const $ pure unit) resE
