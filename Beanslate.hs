import Text.Megaparsec hiding (label, (<?>))
import qualified Text.Megaparsec as P
import Text.Megaparsec.Internal (ParsecT(..))
import Text.Megaparsec.Char
import qualified Data.Set as Set
import Data.Void (Void)
import Data.List (isPrefixOf)
import Data.Maybe (isJust, mapMaybe)
import Text.Pretty.Simple (pPrint)
import Control.Monad (void)
import Text.Printf (printf)
import Data.Either (fromRight)

-- The following several lines are from this Stack Overflow answer by K. A.
-- Buhr (https://stackoverflow.com/a/78209173/3422337).  It changes the
-- behavior of Megaparsec's label function (infix version: <?>) so that a full
-- traceback of parsers is shown when a parsing error happens.
data ErrorWithLabel = ErrorWithLabel String (ParseError String ErrorWithLabel)
  deriving (Eq, Ord)

-- orphan instance needed for technical reasons
deriving instance Ord (ParseError String ErrorWithLabel)

instance ShowErrorComponent ErrorWithLabel where
  showErrorComponent (ErrorWithLabel l e) =
    "while parsing " <> l <> ",\n" <> parseErrorTextPretty e

type Parser = Parsec ErrorWithLabel String

label :: String -> Parser p -> Parser p
label l p = ParsecT $ \s cok cerr eeok eerr ->
  let addLabel e = FancyError (errorOffset e) .
        Set.singleton . ErrorCustom $ ErrorWithLabel l e
  in unParser (P.label l p) s cok (cerr . addLabel) eeok eerr

infix 0 <?>
(<?>) :: Parser p -> String -> Parser p
(<?>) = flip label

-- End of code from K. A. Buhr's answer.

data Date = Date
    { year :: Int
    , month :: Int
    , day :: Int
    } deriving (Eq, Show)

data CurrenciedAmount = CurrenciedAmount
    { caAmount :: String
    , caCurrency :: String
    } deriving (Eq, Show)

type AccountName = String

type TransactionKeyword = String

data Transaction = Transaction
    { txnDate :: Date
    , txnNarration :: String
    , txnAccountLines :: [TransactionAccountLine]
    } deriving (Eq, Show)

data TransactionAccountLine = TransactionAccountLine
    { talAccountName :: AccountName
    , talAmount :: String  -- TODO: this should be CurrenciedAmount
    , talSign :: Char
    } deriving (Eq, Show)

-- A slightly more refined version of RawAccountPart, but less refined
-- than a TransactionAccountLine. A sap MUST have a sign, but can have
-- a missing amount.
data SignedAccountPart = SignedAccountPart
    { sapAccountName :: AccountName
    , sapCurrenciedAmount :: Maybe CurrenciedAmount
    , sapSign :: Char
    } deriving (Eq, Show)

-- This represents a TransactionAccountLine with possibly missing parts.
-- Various functions (in particular, withBothSigns) will be used to fill in the
-- missing parts and create a complete TransactionAccountLine.
data RawAccountPart = RawAccountPart
    { rapAccountName :: AccountName
    , rapTransactionKeyword :: Maybe TransactionKeyword
    , rapCurrenciedAmount :: Maybe CurrenciedAmount
    , rapKeywordSign :: Maybe Char
    , rapArrowSign :: Maybe Char
    } deriving (Eq, Show)

main :: IO ()
main = do
  contents <- getContents
  let txns = parse transaction "<stdin>" contents
  case txns of
    Left e -> putError e
    Right x -> case x of
                Left e' -> putStrLn e'
                Right t -> putStr $ toBeancount t

-- Modified from https://github.com/JakeWheat/intro_to_parsing/blob/a84aca1c172f201e5457cfa2f190cf98cd120d06/FunctionsAndTypesForParsing.lhs#L49-L51
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) "(source unknown)"
                        where
                         leftOver = manyTill anySingle eof

putError :: ParseErrorBundle String ErrorWithLabel -> IO ()
putError e = putStr $ errorBundlePretty e

parseOrPrintError :: (Show a, Show b) => Parser a -> (a -> b) -> String -> IO ()
parseOrPrintError p f input = case parseWithLeftOver p input of
                                Right x -> do
                                            putStrLn "("
                                            pPrint $ f $ fst x
                                            putStr ","
                                            putStr $ snd x
                                            putStrLn ")"
                                Left e -> putError e
                                -- Left e -> putStrLn $ show e

-- Use like parseOrPrintErrorFromFile transaction "example.txt"
parseOrPrintErrorFromFile :: (Show a, Show b) => Parser a -> (a -> b) -> FilePath -> IO ()
parseOrPrintErrorFromFile p f filename = do
                                     input <- readFile filename
                                     parseOrPrintError p f input

debugParseTransactionFromFile :: FilePath -> IO ()
debugParseTransactionFromFile = parseOrPrintErrorFromFile transaction (either (const "") toBeancount)

keywordToSign :: String -> String -> Either String Char
keywordToSign accountType keyword
  | accountType == "Assets" =
     case keyword of
        "increase" -> Right '+'
        "decrease" -> Right '-'
        "opening balance" -> Right '+'
        "paid" -> Right '-'  -- e.g. Assets:PayPal
        "spend" -> Right '-'  -- e.g. Assets:PayPal
        "used" -> Right '-'  -- e.g. Assets:Cash
        "owed to me" -> Right '+'  -- e.g. Assets:Bob
        "owed to them" -> Right '-'  -- e.g. Assets:Bob
        "received" -> Right '+'  -- e.g. Assets:PayPal
        "receive" -> Right '+'  -- e.g. Assets:PayPal
        "repayment to me" -> Right '-'  -- e.g. Assets:Bob
        "repayment to them" -> Right '+'  -- e.g. Assets:Bob
        "withdrawal" -> Right '-'  -- e.g. Assets:Bank
        "deposit" -> Right '+'  -- e.g. Assets:Bank
        _ -> Left $ "The transaction keyword " ++ keyword ++ " is not supported for the account type " ++ accountType
  | accountType == "Liabilities" =
     case keyword of
        "increase" -> Right '-'
        "decrease" -> Right '+'
        "owed to me" -> Right '+'  -- e.g. Liabilities:Bob
        "owed to them" -> Right '-'  -- e.g. Liabilities:Bob
        "charge" -> Right '-'  -- e.g. Liabilities:CreditCard
        "payment" -> Right '+'  -- e.g. Liabilities:CreditCard
        "repayment to me" -> Right '-'  -- e.g. Liabilities:Bob
        "repayment to them" -> Right '+'  -- e.g. Liabilities:Bob
        _ -> Left $ "The transaction keyword " ++ keyword ++ " is not supported for the account type " ++ accountType
  | accountType == "Equity" =
     case keyword of
        "increase" -> Right '-'
        "decrease" -> Right '+'
        "opening balance" -> Right '-'
        _ -> Left $ "The transaction keyword " ++ keyword ++ " is not supported for the account type " ++ accountType
  | accountType == "Income" =
     case keyword of
        "increase" -> Right '-'
        "decrease" -> Right '+'
        "owed to me" -> Right '-'
        "earned" -> Right '-'  -- e.g. Income:Salary
        "income" -> Right '-'  -- e.g. Income:Salary
        "charge" -> Right '+'
        _ -> Left $ "The transaction keyword " ++ keyword ++ " is not supported for the account type " ++ accountType
  | accountType == "Expenses" =
     case keyword of
        "increase" -> Right '+'
        "decrease" -> Right '-'
        "spent" -> Right '+'  -- e.g. Expenses:Groceries
        "expense" -> Right '+'  -- e.g. Expenses:Groceries
        "rebate" -> Right '-'
        _ -> Left $ "The transaction keyword " ++ keyword ++ " is not supported for the account type " ++ accountType
  | otherwise = Left $ "Unknown account type: " ++ accountType

-- TODO: output for this function should be Maybe String or Either String String
-- so that errors can be propagated. It's not a big deal since the parser accountName
-- only allows valid account types, but catching it here will make this function more
-- flexible, in case we want to use it for anything else.
figureOutAccountType :: String -> String
figureOutAccountType name
  | "Assets" `isPrefixOf` name = "Assets"
  | "Liabilities" `isPrefixOf` name = "Liabilities"
  | "Equity" `isPrefixOf` name = "Equity"
  | "Income" `isPrefixOf` name = "Income"
  | "Expenses" `isPrefixOf` name = "Expenses"

-- If the RawAccountPart has no keyword, then that's okay because we can maybe
-- infer it from an arrow. But if the RawAccountPart has an incorrect keyword that
-- doesn't apply to this account type, or an account type that's not valid, then
-- we want to propagate that error message forward.
keywordSign :: RawAccountPart -> Either String (Maybe Char)
keywordSign (RawAccountPart _ Nothing _ _ _) = Right Nothing
keywordSign (RawAccountPart name (Just keyword) _ _ _) =
    do
      sign <- keywordToSign (figureOutAccountType name) keyword
      Right $ Just sign

withKeywordSign' :: RawAccountPart -> Either String RawAccountPart
withKeywordSign' rap@(RawAccountPart name tkw ca _ ars) =
    do
        sign <- keywordSign rap
        return $ RawAccountPart name tkw ca sign ars

withKeywordSign :: [RawAccountPart] -> Either String [RawAccountPart]
withKeywordSign = traverse withKeywordSign'

forceArrowSign :: Char -> RawAccountPart -> RawAccountPart
forceArrowSign c (RawAccountPart name tkw ca kws _) = RawAccountPart name tkw ca kws (Just c)

withArrowSign :: [RawAccountPart] -> String -> [RawAccountPart] -> ([RawAccountPart], [RawAccountPart])
withArrowSign acclines1 "->" acclines2 = (map (forceArrowSign '-') acclines1, map (forceArrowSign '+') acclines2)
withArrowSign acclines1 "<-" acclines2 = (map (forceArrowSign '+') acclines1, map (forceArrowSign '-') acclines2)
withArrowSign acclines1 _ acclines2 = (acclines1, acclines2)

withBothSigns :: [RawAccountPart] -> String -> [RawAccountPart] -> Either String [RawAccountPart]
withBothSigns acclines1 arr acclines2 = withKeywordSign (xs1 ++ xs2)
                                        where
                                            (xs1, xs2) = withArrowSign acclines1 arr acclines2

-- This does a local check of each RawAccountPart -- if it has no sign or
-- mismatching signs, then we raise an error.
-- It's ok for the input to have a sign but no amount (since the amount can
-- possibly be filled in using the other amounts in the transaction -- see
-- validateSignedAccountParts), but it's not okay for the input to have an amount
-- without a sign (because we should have inserted any missing signs already,
-- so that means not enough sign information was provided in the transaction).
rapToSap :: RawAccountPart -> Either String SignedAccountPart
rapToSap (RawAccountPart name _ ca (Just kwSign) (Just arrSign))
  | kwSign == arrSign = Right $ SignedAccountPart name ca kwSign
  | otherwise = Left ("In account part " ++ name ++ " the keyword sign is " ++
                      [kwSign] ++ " but the arrow sign is " ++ [arrSign])
rapToSap (RawAccountPart name _ ca (Just kwSign) _) = Right $ SignedAccountPart name ca kwSign
rapToSap (RawAccountPart name _ ca _ (Just arrSign)) = Right $ SignedAccountPart name ca arrSign
rapToSap (RawAccountPart name _ _ _ _) = Left ("The account part " ++ name ++
                                               " is missing a sign (no keyword sign or arrow sign has been provided)!")

signedAmount :: SignedAccountPart -> Maybe Float
signedAmount (SignedAccountPart _ (Just ca) '+') = Just (read $ caAmount ca :: Float)
signedAmount (SignedAccountPart _ (Just ca) '-') = Just (read $ '-' : caAmount ca :: Float)
signedAmount _ = Nothing

hasAmount :: SignedAccountPart -> Bool
hasAmount = isJust . sapCurrenciedAmount

sapToTal :: SignedAccountPart -> Either String TransactionAccountLine
sapToTal (SignedAccountPart name (Just ca) sign) = Right $ TransactionAccountLine name (caAmount ca) sign
sapToTal (SignedAccountPart name Nothing _) = Left ("In account part " ++ name ++ " no currencied amount has been given!")

-- Make sure at most one currencied amount is missing, and if it is missing,
-- fill it in by calculating what it must be using the other amounts.
-- * Having all amounts and signs and adding up to zero -> ok
-- * Having all amounts and signs, but not adding up to zero -> not ok
-- * Having a single missing amount (but everything that DOES have
--   an amount has a sign) -> ok
-- * Having more than one missing amount -> not ok
-- * Having one missing amount, but the missing amount has an explicit
--   sign and that sign is different from the calculated sign -> not ok
validateSignedAccountParts :: [SignedAccountPart] -> Either String [TransactionAccountLine]
validateSignedAccountParts saps =
    let justs = filter hasAmount saps
        nothings = filter (not . hasAmount) saps
        sumOfJusts = sum $ mapMaybe signedAmount saps :: Float
    in case length nothings of
        0 -> if abs sumOfJusts < 0.0001
                then traverse sapToTal justs
                else Left "All amounts have been provided but don't sum to ~zero!"
        1 -> let insertIndex = length $ takeWhile hasAmount saps
                 missingValue = abs sumOfJusts
                 missingSap = head nothings
                 missingSign = if (-sumOfJusts) >= 0 then '+' else '-'
                 missingTal = if sapSign missingSap == missingSign
                                then sapToTal (SignedAccountPart
                                                (sapAccountName missingSap)
                                                (Just $ CurrenciedAmount
                                                    (show missingValue) "USD")
                                                (sapSign missingSap))
                                else Left ("An explicit sign (" ++ [sapSign missingSap] ++ ") was given for "
                                           ++ sapAccountName missingSap ++
                                           " but this sign does not match the " ++
                                           "(implicit, calculated) sign (" ++ [missingSign] ++ ") from the " ++
                                           "other amounts!" ++ show missingValue)
                 (before, after) = splitAt insertIndex justs
             in (\x y z -> x ++ y ++ z) <$> traverse sapToTal before <*> sequence [missingTal] <*> traverse sapToTal after
        _ -> Left "More than one missing amount found!"

showTransactionAccountLine :: Int -> TransactionAccountLine -> String
showTransactionAccountLine padding (TransactionAccountLine name amount sign) = "  " ++ name ++ replicate (padding - length name + 2) ' ' ++ [sign] ++ amount ++ " USD"

showDate :: Date -> String
showDate (Date y m d) = show y ++ "-" ++ printf "%02d" m ++ "-" ++ printf "%02d" d

toBeancount :: Transaction -> String
toBeancount (Transaction d nar tals) = showDate d ++ " * \"" ++ nar ++ "\"\n" ++ unlines (map (showTransactionAccountLine longestAccountLength) tals)
                                        where
                                            longestAccountLength = maximum (map (length . talAccountName) tals)

date :: Parser Date
date = label "date" $ do
         y <- count 4 digitChar
         _ <- char '-'
         m <- count 2 digitChar
         _ <- char '-'
         d <- count 2 digitChar
         return $ Date (read y) (read m) (read d)

narration :: Parser String
narration = label "narration" $ char '"' *>  many (satisfy (/= '"')) <* char '"'

flagOrDirective :: Parser String
flagOrDirective = label "flag/directive" $ string "*" <|> string "!" <|> string "txn"

accountName :: Parser String
accountName = label "account name" $ do
                accountType <- string "Assets" <|> string "Liabilities" <|> string "Income" <|> string "Expenses" <|> string "Equity"
                rest <- many (letterChar <|> char ':')
                return $ accountType ++ rest

unsignedValue :: Parser String
unsignedValue = label "unsigned value" $ do
                    integerPart <- some digitChar
                    decimalPart <- optional . try $ do
                                            dot <- char '.'
                                            ds <- some digitChar
                                            return $ dot:ds
                    return $ case decimalPart of
                                Nothing -> integerPart
                                Just v -> integerPart ++ v

-- Parse a Beancount currency, which is just a string of uppercase characters,
-- like "USD" or "EUR". (TODO: Beancount is slightly more flexible than this in
-- that it allows certain special characters in the currency symbol, so
-- eventually the code below should be changed to support exactly the currency
-- strings that Beancount supports.)
currency :: Parser String
currency = label "currency" $ some upperChar

-- Parse an unsigned monetary amount like "12.50 USD".
unsignedAmount :: Parser (Either String CurrenciedAmount)
unsignedAmount = label "unsigned amount" $ do
                    n <- unsignedValue
                    c <- optional . try $ some (char ' ') *> currency
                    return $ case c of
                                Nothing -> Left $ "Amount of " ++ n ++" was found, but no currency (e.g. USD) was found!"
                                Just x -> Right $ CurrenciedAmount n x

accountPart :: Parser (Either String RawAccountPart)
accountPart = label "account part" $ do
                ac <- accountName
                keyword <- optional . try $ do
                                              _ <- some spaceChar
                                              _ <- char '('
                                              kw <- some (satisfy (/= ')'))
                                              _ <- char ')'
                                              return kw
                am <- optional . try $ some spaceChar *> unsignedAmount
                return $ case am of
                            Nothing -> Right $ RawAccountPart ac keyword Nothing Nothing Nothing
                            Just v -> do
                                        am' <- v
                                        Right $ RawAccountPart ac keyword (Just am') Nothing Nothing

arrow :: Parser String
arrow = label "arrow" $ string "->" <|> string "<-"

transaction :: Parser (Either String Transaction)
transaction = label "transaction" $ do
                d <- date
                _ <- some spaceChar
                nar <- narration
                -- Without the try, the line below will start trying to match
                -- another accountPart as soon as it sees the next space. Then
                -- if it doesn't immediately match another accountPart, it will
                -- produce a parse error.
                acclines1 <- some (try $ some spaceChar *> accountPart)
                arrowAndBeyond <- optional . try $ do
                                        _ <- some spaceChar
                                        ar <- arrow
                                        acclines2 <- some (try $ some spaceChar *> accountPart)
                                        return (ar, acclines2)
                _ <- void (some spaceChar) <|> eof
                let rawTransaction = case arrowAndBeyond of
                                        Nothing -> (d, nar, acclines1, "(no arrow)", [])
                                        Just (ar, acclines2) -> (d, nar, acclines1, ar, acclines2)
                let (d, nar, acclines1, arr, acclines2) = rawTransaction
                return $ do
                            al1 <- sequence acclines1
                            al2 <- sequence acclines2
                            raps <- withBothSigns al1 arr al2
                            saps <- case length raps of
                                        0 -> Left "No account parts were found!"
                                        1 -> Left ("Only one account part was found! In a double-entry "
                                                   ++ "bookkeeping system, at least two accounts must "
                                                   ++ "be involved in any transaction.")
                                        _ -> traverse rapToSap raps
                            tals <- validateSignedAccountParts saps
                            Right $ Transaction d nar tals
