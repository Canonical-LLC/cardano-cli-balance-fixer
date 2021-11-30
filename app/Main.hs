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
import Data.Function
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as Aeson

data Command
  = Input
    { address :: String
    , testnet :: Maybe Int
    }
  | Change
    { address :: String
    , testnet :: Maybe Int
    , outputAssets :: [String]
    }
  | ParseAsUtxo
  | CollateralUtxo
    { address :: String
    , testnet :: Maybe Int
    }
  | UtxoAssets
    { utxo :: String
    , testnet :: Maybe Int
    }
  | Balance
    { address :: String
    , testnet :: Maybe Int
    }
  | DiffValues
    { inputFile0 :: FilePath
    , inputFile1 :: FilePath
    }
  deriving (Eq, Show)

pAddressAndTestnet :: Parser (String, Maybe Int)
pAddressAndTestnet
  =   (,)
  <$> strOption
        ( long "address"
        <> short 'a'
        <> metavar "ADDRESS"
        )
  <*> optional
        ( option auto
          ( long "testnet-magic"
          <> metavar "TESTNET_MAGIC_NUMBER"
          )
        )

pOutputs :: Parser [String]
pOutputs = many
  ( strOption
    (  long "output"
    <> short 'o'
    <> metavar "OUTPUT_VALUE"
    )
  )

pUtxo :: Parser (String, Maybe Int)
pUtxo
  = (,)
  <$> strOption
      ( long "utxo"
      <> short 'u'
      <> metavar "TRANSACTION_ID#OUTPUT_ID"
      )
  <*> optional
        ( option auto
          ( long "testnet-magic"
          <> metavar "TESTNET_MAGIC_NUMBER"
          )
        )

twoValuePaths :: Parser (FilePath, FilePath)
twoValuePaths
  = (,)
  <$> strOption
      ( long "value-1"
      <> short 'v'
      <> metavar "VALUE_FILEPATH_1"
      )
  <*> strOption
      ( long "value-2"
      <> short 'u'
      <> metavar "VALUE_FILEPATH_2"
      )


pCommand :: Parser Command
pCommand
   = subparser
      (   ( command "input"
              (uncurry Input <$> info pAddressAndTestnet (progDesc "Prints out the UTxOs as input args for the cardano-cli"))
          )
      <> ( command "change"
              (info
                ((\(address, testnet) outputAssets -> Change {..}) <$> pAddressAndTestnet <*> pOutputs)
                (progDesc "create a value suitable "))
         )
      <> ( command "parse-as-utxo"
              (ParseAsUtxo <$ info
                (pure ())
                (progDesc "Parse a line from `cardano-cli query utxo` and print as tx#output-id")
              )
         )
      <> ( command "collateral"
            (uncurry CollateralUtxo <$> info
              pAddressAndTestnet
              (progDesc "Find a large UTxO for collateral")
            )
          )
      <> ( command "utxo-assets"
            (uncurry UtxoAssets <$> info
              pUtxo
              (progDesc "Print out the assets on a UTxO")
            )
          )
      <> ( command "balance"
            (uncurry Balance <$> info
              pAddressAndTestnet
              (progDesc "Find a large UTxO for collateral")
            )
          )
      <> ( command "diff-values"
            (uncurry DiffValues <$> info
              twoValuePaths
              (progDesc "Find a large UTxO for collateral")
            )
          )
      )


parseNonNativeTokens :: [String] -> Maybe Value
parseNonNativeTokens = go mempty where
  go acc xs = case xs of
    [] -> Just acc
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


runCardanoCliUtxo :: String -> Maybe Int -> IO [UTxO]
runCardanoCliUtxo utxo mTestnet
  = mapM (\line -> maybe (throwIO . userError $ "Failed to parse UTxO for line: " <> line) pure $ parseUTxOLine line) . drop 2 . lines
  =<< readProcess
      "cardano-cli"
      ( [ "query"
        , "utxo"
        , "--tx-in"
        , utxo
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

diffTokenMapWithNegatives :: Map String Int -> Map String Int -> Maybe (Map String Int)
diffTokenMapWithNegatives x y =
  let
    diffCoin a b =
      let a' = a - b
      in if a' == 0
        then Nothing
        else Just a'

    new = M.differenceWith diffCoin x y

    in if new == mempty
          then Nothing
          else Just new

diffValuesWithNegatives :: Value -> Value -> Value
diffValuesWithNegatives = M.differenceWith (diffTokenMapWithNegatives)

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
  execParser (info (pCommand <**> helper) mempty) >>= \case
    Change {..} -> do
      utxos <- runCardanoCli address testnet
      outputValues <- forM outputAssets $ \outputStr ->
        maybe (throwIO $ userError $ "Failed to parse output value: " <> outputStr) pure
          $ parseValue outputStr

      putStrLn $ pprValue
        $ M.delete "" (mergeValues $ map utxoAssets utxos) `diffValues` mergeValues outputValues

    Input {..} -> do
      utxos <- runCardanoCli address testnet
      let inputsStr = unwords $ map pprUTxOInput utxos
      putStrLn inputsStr

    ParseAsUtxo -> do
      utxoLine <- getLine

      UTxO {..} <- maybe (throwIO . userError $ "Failed to parse UTxO for line: " <> utxoLine) pure
        $ parseUTxOLine utxoLine

      putStr $ utxoTx
            <> "#"
            <> utxoIndex

    CollateralUtxo {..} -> do
      utxos <- runCardanoCli address testnet
      let lovelaces :: UTxO -> Int
          lovelaces = fromMaybe 0 . M.lookup "" . fromMaybe mempty . M.lookup "" . utxoAssets
      let UTxO {..} = maximumBy (compare `on` lovelaces) utxos
      putStr $ utxoTx
            <> "#"
            <> utxoIndex

    UtxoAssets {..} -> do
      runCardanoCliUtxo utxo testnet >>= \case
        [UTxO {..}] -> putStr $ pprValue utxoAssets
        _ -> throwIO $ userError "found more than one utxo!"

    Balance {..} -> do
      utxos <- runCardanoCli address testnet
      let mergedValue = mergeValues $ map utxoAssets utxos

      BSLC.putStrLn $ Aeson.encode mergedValue

    DiffValues {..} -> do
      value0 <- either (throwIO . userError) pure
              . Aeson.eitherDecode
              =<< BSL.readFile inputFile0

      value1 <- either (throwIO . userError) pure
              . Aeson.eitherDecode
              =<< BSL.readFile inputFile1

      BSLC.putStrLn $ Aeson.encode $ diffValuesWithNegatives value1 value0
