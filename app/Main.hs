module Main where
import System.Process
import Options.Applicative
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Exception
import Data.Maybe
import Data.Traversable
import Text.Read
import Data.List
import Data.List.Split

data Command = Input | Change
  deriving (Eq, Show)

data Options = Options
  { address      :: String
  , testnet      :: Maybe Int
  , outputAssets :: [String]
  , commandType  :: Command
  }
  deriving (Eq, Show)

pCommand :: Parser Options
pCommand
   =  Options
  <$> strOption
      ( long "address"
      <> short 'a'
      <> metavar "ADDRESS"
      )
  <*> optional
        ( option auto
          ( long "testnet"
          <> metavar "TESTNET_MAGIC_NUMBER"
          )
        )
  <*> many
        ( strOption
          (  long "output"
          <> short 'o'
          <> metavar "OUTPUT_VALUE"
          )
        )
  <*> subparser
      (   ( command "input"
              (Input <$ info (pure ()) (progDesc "Prints out the UTxOs as input args for the cardano-cli"))
          )
      <> ( command "change"
              (Change <$ info (pure ()) (progDesc "create a value suitable "))
          )
      )

parseNonNativeTokens :: [String] -> Maybe Value
parseNonNativeTokens = go mempty where
  go acc xs = case xs of
    "+":"TxOutDatumNone":[] -> Just acc
    "+":"TxOutDatumHash":_ -> Just acc
    "+":countStr:asset:rest -> do
      count <- readMaybe countStr
      (policyId, tokenName) <- case splitOn "." asset of
        [policyId, tokenName] -> Just (policyId, tokenName)
        _ -> Nothing

      let newAcc = M.insertWith (<>) policyId (M.singleton tokenName count) acc

      go newAcc rest

parseValue :: String -> Maybe Value
parseValue = parseValue' . words

parseValue' :: [String] -> Maybe Value
parseValue' xs = case xs of
  lovelacesStr:"lovelace":nonNativeTokens -> do
    lovelaces <- readMaybe lovelacesStr
    initialValue <- parseNonNativeTokens nonNativeTokens
    pure $ M.insert "" (M.singleton "" lovelaces) initialValue
  _ -> Nothing

type Value = Map String (Map String Int)

data UTxO = UTxO
  { utxoTx     :: String
  , utxoIndex  :: String
  , utxoAssets :: Value
  } deriving (Eq, Show)

parseUTxOLine :: String -> Maybe UTxO
parseUTxOLine line = case words line of
  utxoTx:utxoIndex:rest -> do
    utxoAssets <- parseValue' rest
    pure UTxO {..}
  _ -> Nothing

runCardanoCli :: String -> Maybe Int -> IO [UTxO]
runCardanoCli address mTestnet
  = mapM (\line -> maybe (throwIO . userError $ "Failed to parse UTxO for line: " <> line) pure $ parseUTxOLine line) . drop 2 . lines
  =<< readProcess
      "cardano-cli"
      ( [ "query"
        , "utxo"
        , "--address"
        , address
        ] <>
        maybe ["--mainnet"] (\x -> ["--testnet-magic", show x]) mTestnet
      )
      ""

mergeValues :: [Value] -> Value
mergeValues = foldr mergeValue mempty

mergeValue :: Value -> Value -> Value
mergeValue x y = M.unionWith (M.unionWith (+)) x y

diffTokenMap :: Map String Int -> Map String Int -> Maybe (Map String Int)
diffTokenMap x y =
  let
    diffCoin a b =
         let a' = a - b
         in if a' < 1
              then Nothing
              else Just a'

    new = M.differenceWith diffCoin x y

    in if new == mempty
          then Nothing
          else Just new


diffValues :: Value -> Value -> Value
diffValues = M.differenceWith (diffTokenMap)

pprPolicyTokens :: String -> Map String Int -> [String]
pprPolicyTokens policyId tokenMap = if policyId == ""
  then map (\count -> show count <> " lovelace") $ M.elems tokenMap
  else map (\(tokenName, count) -> show count <> " " <> policyId <> "." <> tokenName )
    $ M.toList tokenMap

pprValue :: Value -> String
pprValue
  = intercalate " + "
  . concatMap (uncurry pprPolicyTokens) . M.toList

pprUTxOInput :: UTxO -> String
pprUTxOInput UTxO {..}
  = "--tx-in "
  <> utxoTx
  <> "#"
  <> utxoIndex

main :: IO ()
main = do
  Options {..} <- execParser $ info (pCommand <**> helper) mempty

  utxos <- runCardanoCli address testnet

  case commandType of
    Change -> do
      outputValues <- forM outputAssets $ \outputStr ->
        maybe (throwIO $ userError $ "Failed to parse output value: " <> outputStr) pure
          $ parseValue outputStr

      putStrLn $ pprValue
        $ (mergeValues $ map utxoAssets utxos) `diffValues` mergeValues outputValues


    Input -> do
      let inputsStr = unwords $ map pprUTxOInput utxos
      putStrLn inputsStr