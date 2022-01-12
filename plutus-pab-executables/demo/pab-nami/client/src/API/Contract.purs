module API.Contract where

import Prologue
import Affjax.ResponseFormat as ResponseFormat
import Effect.Aff (error, throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Affjax (get, printError) as AX
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut.Core as A
import Data.Array (head)
import Data.Either (either)
import Data.Maybe (maybe)
import Effect.Exception (Error)
import Foreign.Object as FO
import Debug (traceM)

-- | Fetch the partial transaction from the PAB.
fetchContractPartialTx ::
  forall m.
  MonadAff m =>
  MonadThrow Error m =>
  String ->
  m String
fetchContractPartialTx cid = do
  resE <-
    liftAff
      $ AX.get ResponseFormat.json ("/api/contract/instance/" <> cid <> "/status")
  res <- either (throwError <<< error <<< AX.printError) pure resE
  traceM res
  let
    partialTxCborM =
      (\y -> A.toObject y >>= \x -> FO.lookup "transaction" x >>= A.toString)
        =<< head
        =<< A.toArray
        =<< FO.lookup "cicYieldedExportTxs"
        =<< A.toObject res.body
  traceM partialTxCborM
  maybe (throwError $ error "Could not parse fields cicYieldedExportTxs.[transaction]")
    pure
    partialTxCborM
