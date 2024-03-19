import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad
import Data.Void (Void)
import Data.List (isPrefixOf)
import Data.Maybe (isJust, fromJust)
import Data.Either (fromRight)
import System.IO

type Parser = Parsec Void String

-- Modified from https://github.com/JakeWheat/intro_to_parsing/blob/a84aca1c172f201e5457cfa2f190cf98cd120d06/FunctionsAndTypesForParsing.lhs#L49-L51
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) "(source unknown)"
                        where
                         leftOver = manyTill anySingle eof

putError :: ParseErrorBundle String Void -> IO ()
putError e = putStr $ errorBundlePretty e

parseOrPrintError p input = case parseWithLeftOver p input of
                                Right x -> print x
                                Left e -> putError e
                                -- Left e -> putStrLn $ show e

-- Use like parseOrPrintErrorFromFile transaction "example.txt"
parseOrPrintErrorFromFile p filename = do
                                         input <- readFile filename
                                         parseOrPrintError p input

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

data TransactionAccountLine = TransactionAccountLine
    { talAccountName :: AccountName
    , talAmount :: String  -- TODO: this should be CurrenciedAmount
    , talSign :: Char
    } deriving (Eq, Show)

data Transaction = Transaction
    { txnDate :: Date
    , txnNarration :: String
    , txnAccountLines :: [TransactionAccountLine]
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

keywordToSign :: String -> String -> Maybe Char
keywordToSign accountType keyword
  | accountType == "Assets" = case keyword of
                                "increase" -> Just '+'
                                "decrease" -> Just '-'
                                "opening balance" -> Just '+'
                                "paid" -> Just '-'  -- e.g. Assets:PayPal
                                "spend" -> Just '-'  -- e.g. Assets:PayPal
                                "used" -> Just '-'  -- e.g. Assets:Cash
                                "owed to me" -> Just '+'  -- e.g. Assets:Bob
                                "owed to them" -> Just '-'  -- e.g. Assets:Bob
                                "received" -> Just '+'  -- e.g. Assets:PayPal
                                "receive" -> Just '+'  -- e.g. Assets:PayPal
                                "repayment to me" -> Just '-'  -- e.g. Assets:Bob
                                "repayment to them" -> Just '+'  -- e.g. Assets:Bob
                                _ -> Nothing
  | accountType == "Liabilities" = case keyword of
                                    "increase" -> Just '-'
                                    "decrease" -> Just '+'
                                    "owed to me" -> Just '+'  -- e.g. Liabilities:Bob
                                    "owed to them" -> Just '-'  -- e.g. Liabilities:Bob
                                    "charge" -> Just '-'  -- e.g. Liabilities:CreditCard
                                    "payment" -> Just '+'  -- e.g. Liabilities:CreditCard
                                    "repayment to me" -> Just '-'  -- e.g. Liabilities:Bob
                                    "repayment to them" -> Just '+'  -- e.g. Liabilities:Bob
                                    _ -> Nothing
  | accountType == "Equity" = case keyword of
                                "increase" -> Just '-'
                                "decrease" -> Just '+'
                                "opening balance" -> Just '-'
                                _ -> Nothing
  | accountType == "Income" = case keyword of
                                "increase" -> Just '-'
                                "decrease" -> Just '+'
                                "owed to me" -> Just '-'
                                "earned" -> Just '-'  -- e.g. Income:Salary
                                "income" -> Just '-'  -- e.g. Income:Salary
                                _ -> Nothing
  | accountType == "Expenses" = case keyword of
                                    "increase" -> Just '+'
                                    "decrease" -> Just '-'
                                    "spent" -> Just '+'  -- e.g. Expenses:Groceries
                                    "expense" -> Just '+'  -- e.g. Expenses:Groceries
                                    "rebate" -> Just '-'
                                    _ -> Nothing
  | otherwise = Nothing

figureOutAccountType name
  | "Assets" `isPrefixOf` name = "Assets"
  | "Liabilities" `isPrefixOf` name = "Liabilities"
  | "Equity" `isPrefixOf` name = "Equity"
  | "Income" `isPrefixOf` name = "Income"
  | "Expenses" `isPrefixOf` name = "Expenses"

date :: Parser Date
date = do
         y <- count 4 digitChar
         _ <- char '-'
         m <- count 2 digitChar
         _ <- char '-'
         d <- count 2 digitChar
         return $ Date (read y) (read m) (read d)

narration :: Parser String
narration = do
                _ <- char '"'
                x <- many (satisfy (/= '"'))
                _ <- char '"'
                return $ x

flagOrDirective :: Parser String
flagOrDirective = string "*" <|> string "!" <|> string "txn"

accountName :: Parser String
accountName = do
                accountType <- string "Assets" <|> string "Liabilities" <|> string "Income" <|> string "Expenses" <|> string "Equity"
                rest <- many (letterChar <|> char ':')
                return $ accountType ++ rest

unsignedValue :: Parser String
unsignedValue = do
                    integerPart <- some digitChar
                    decimalPart <- optional . try $ do
                                            dot <- char '.'
                                            ds <- some digitChar
                                            return $ [dot] ++ ds
                    return $ case decimalPart of
                                Nothing -> integerPart
                                Just v -> integerPart ++ v

currency :: Parser String
currency = some upperChar

unsignedAmount :: Parser CurrenciedAmount
unsignedAmount = do
                    n <- unsignedValue
                    _ <- some spaceChar
                    c <- currency
                    return $ CurrenciedAmount n c

accountPart :: Parser RawAccountPart
accountPart = do
                ac <- accountName
                keyword <- optional . try $ do
                                              _ <- some spaceChar
                                              _ <- char '('
                                              kw <- some (satisfy (/= ')'))
                                              _ <- char ')'
                                              return kw
                am <- optional . try $ do
                                         _ <- some spaceChar
                                         unsignedAmount
                return $ RawAccountPart ac keyword am Nothing Nothing

arrow :: Parser String
arrow = string "->" <|> string "<-"

-- If the RawAccountPart has no keyword, then that's okay because we can maybe
-- infer it from an arrow. But if RawAccountPart has an incorrect keyword that
-- doesn't apply to this account type, or an account type that's not valid, then
-- we want to propagate that error message forward.
keywordSign :: RawAccountPart -> Either String (Maybe Char)
keywordSign (RawAccountPart _ Nothing _ _ _) = Right Nothing
keywordSign (RawAccountPart name (Just keyword) _ _ _) =
    let sign = keywordToSign (figureOutAccountType name) keyword
     in case sign of
            Nothing -> Left $ "The keyword " ++ keyword ++ " does not apply to this account type!"
            Just x -> Right (Just x)

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

-- Make sure the keyword-inferred sign is the same as the arrow-inferred sign
validateRawAccountPartSign :: RawAccountPart -> Either String RawAccountPart
validateRawAccountPartSign (RawAccountPart name tkw ca kwSign arrSign)
  | kwSign == Just '+' && arrSign == Just '-' = Left ("In account part " ++ name ++ " the keyword sign is + but arrow sign is -")
  | kwSign == Just '-' && arrSign == Just '+' = Left ("In account part " ++ name ++ " the keyword sign is - but arrow sign is +")
  | kwSign == Just '+' || arrSign == Just '+' = Right (RawAccountPart name tkw ca (Just '+') (Just '+'))
  | kwSign == Just '-' || arrSign == Just '-' = Right (RawAccountPart name tkw ca (Just '-') (Just '-'))
  | otherwise = Left ("In account part " ++ name ++ " cannot figure out sign!")

-- Assume that validateRawAccountPartSign has already been run, so canonically
-- use the keyword sign as the sign.
signedAmount :: RawAccountPart -> Maybe Float
signedAmount (RawAccountPart _ _ (Just ca) (Just '+') _) = Just $ (read $ caAmount ca :: Float)
signedAmount (RawAccountPart _ _ (Just ca) (Just '-') _) = Just $ (read $ '-':(caAmount ca) :: Float)
signedAmount (RawAccountPart _ _ Nothing _ _) = Nothing

hasAmount :: RawAccountPart -> Bool
hasAmount = isJust . signedAmount

-- Assuming a filled in RawAccountPart, convert it to TransactionAccountLine
rapToTal :: RawAccountPart -> TransactionAccountLine
rapToTal (RawAccountPart name tkw (Just ca) (Just sign) _) = TransactionAccountLine name (caAmount ca) sign

-- Make sure at most one currencied amount is missing, and if it is missing,
-- fill it in by calculating what it must be using the other amounts
validateRawAccountParts :: [RawAccountPart] -> Either String [TransactionAccountLine]
validateRawAccountParts raps =
    let justs = filter hasAmount raps
        nothings = filter (not . hasAmount) raps
        sumOfJusts = sum (map (fromJust . signedAmount) justs) :: Float
        insertIndex = length $ takeWhile hasAmount raps
    in case length nothings of
        0 -> if abs (sumOfJusts) < 0.0001
                then Right (map rapToTal justs)
                else Left "All values are Just but don't sum to ~zero!"
        1 -> let missingValue = -sumOfJusts
                 missingRap = head nothings
                 missingTal = TransactionAccountLine (rapAccountName missingRap) (show missingValue) (fromJust $ rapKeywordSign missingRap)
                 (before, after) = splitAt insertIndex justs
             in Right $ (map rapToTal before) ++ [missingTal] ++ (map rapToTal after)
        _ -> Left "More than one Nothing found!"

transaction :: Parser (Either String Transaction)
transaction = do
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
                let rawTransaction = case arrowAndBeyond of
                                        Nothing -> (d, nar, acclines1, "(no arrow)", [])
                                        Just (ar, acclines2) -> (d, nar, acclines1, ar, acclines2)
                let (d, nar, acclines1, arr, acclines2) = rawTransaction
                return $ do
                            raps <- withBothSigns acclines1 arr acclines2
                            raps' <- sequence $ map validateRawAccountPartSign raps
                            v <- validateRawAccountParts raps'
                            Right $ Transaction d nar v
