{-# LANGUAGE RecordWildCards #-}

module Exporter (
  save,
  save_,
  type App,
  runExporter,
) where

import System.FilePath ((</>))

import Control.Monad.State (StateT, execStateT, forM_, modify')
import Plutarch
import Plutarch (Config (Config, tracingMode), TracingMode (DoTracing))
import Plutarch.Api.V1
import Plutarch.Script
import Ply (
  ScriptRole (..),
  TypedScriptEnvelope (
    TypedScriptEnvelope,
    tsDescription,
    tsParamTypes,
    tsRole,
    tsScript,
    tsVersion
  ),
 )
import Ply.Plutarch (mkEnvelope)
import Ply.Plutarch.TypedWriter (TypedWriter, writeTypedScript)

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Example.NftM (alwaysSucceeds, nftMp)
import Ply.Core.Serialize (writeEnvelope)
import System.Directory (createDirectoryIfMissing)

import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text.Encoding as T
import Debug.Trace (trace, traceM)
import GHC.IO (throwIO)
import Plutarch.Api.V1 (scriptHash)
import Plutarch.Lift (pconstant)
import PlutusLedgerApi.V1 (ScriptHash (ScriptHash, getScriptHash))
import PlutusTx.Prelude (fromBuiltin)
import Ply.Core.Deserialize (readEnvelope)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)

type UPLCProgram = Program DeBruijn DefaultUni DefaultFun ()

type App = ReaderT FilePath (StateT (Map Text TypedScriptEnvelope) IO)

hashScript :: UPLCProgram -> ScriptHash
hashScript = scriptHash . Script

hashTypedEnvelope :: TypedScriptEnvelope -> ScriptHash
hashTypedEnvelope TypedScriptEnvelope {..} = hashScript tsScript

save_ :: TypedWriter pt => Text -> ClosedTerm pt -> App ()
save_ nm pt = case mkEnvelope (Config {tracingMode = DoTracing}) nm pt of
  Left err -> liftIO $ throwIO (userError $ T.unpack err)
  Right env -> modify' $ M.insert nm env

save :: TypedWriter pt => Text -> ClosedTerm pt -> App ScriptHash
save nm pt = case mkEnvelope (Config {tracingMode = DoTracing}) nm pt of
  Left err -> liftIO $ throwIO (userError $ T.unpack err)
  Right env -> do
    modify' $ M.insert nm env
    pure $ hashTypedEnvelope env

showHash :: ScriptHash -> String
showHash =
  T.unpack
    . T.decodeUtf8
    . Base16.encode
    . fromBuiltin
    . getScriptHash

runExporter :: FilePath -> App () -> IO ()
runExporter path exporter = do
  putStrLn $ "Creating directory ./" <> path
  createDirectoryIfMissing True path
  putStrLn $ "Created directory"
  toWrite <- flip execStateT M.empty (flip runReaderT path exporter)
  putStrLn $ "Preparing to write" <> show (length toWrite) <> " scripts"
  index <- foldM go M.empty (M.toList toWrite)
  putStrLn $ "Writing Index.json"
  writeIndex index
  putStrLn "Done"
  where
    go :: Map Text FilePath -> (Text, TypedScriptEnvelope) -> IO (M.Map Text FilePath)
    go acc (nm, env) = do
      let hash = hashTypedEnvelope env
          hashStr = showHash hash
      putStrLn $ "Writing " <> T.unpack nm <> " to " <> path </> hashStr <> ".plutus"
      writeEnvelope
        (path </> hashStr <> ".plutus")
        env
      pure $ M.insert nm hashStr acc

    writeIndex :: Map Text FilePath -> IO ()
    writeIndex = LBS.writeFile filepath . encodePretty
      where
        filepath = path </> "Index.json"

main :: IO ()
main = runExporter "compiled-scripts" $ do
  hash <- save "always succeeds" alwaysSucceeds
  save_ "nft (hash applied)" (nftMp # pconstant hash)
  save_ "nft (no hash applied)" nftMp

runImporter ::
  FilePath -> -- Path to the *directory* that contains the serialized scripts + Index.json
  IO (Map Text TypedScriptEnvelope)
runImporter path =
  eitherDecodeFileStrict (path </> "Index.json") >>= \case
    Left err -> throwIO (userError err)
    Right index -> foldM go M.empty (M.toList index)
  where
    go :: Map Text TypedScriptEnvelope -> (Text, FilePath) -> IO (Map Text TypedScriptEnvelope)
    go acc (nm, file) = do
      env <- readEnvelope (path </> file <> ".plutus")
      pure $ M.insert nm env acc
