import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad
import Data.Void (Void)
import Data.List (isPrefixOf)
import Data.Maybe (isJust, fromJust)
import Data.Either (fromRight)

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

keywordToSign accountType keyword
  | accountType == "Assets" = case keyword of
                                "increase" -> '+'
                                "decrease" -> '-'
                                "opening balance" -> '+'
                                "paid" -> '-'  -- e.g. Assets:PayPal
                                "spend" -> '-'  -- e.g. Assets:PayPal
                                "used" -> '-'  -- e.g. Assets:Cash
                                "owed to me" -> '+'  -- e.g. Assets:Bob
                                "owed to them" -> '-'  -- e.g. Assets:Bob
                                "received" -> '+'  -- e.g. Assets:PayPal
                                "receive" -> '+'  -- e.g. Assets:PayPal
                                "repayment to me" -> '-'  -- e.g. Assets:Bob
                                "repayment to them" -> '+'  -- e.g. Assets:Bob
  | accountType == "Liabilities" = case keyword of
                                    "increase" -> '-'
                                    "decrease" -> '+'
                                    "owed to me" -> '+'  -- e.g. Liabilities:Bob
                                    "owed to them" -> '-'  -- e.g. Liabilities:Bob
                                    "charge" -> '-'  -- e.g. Liabilities:CreditCard
                                    "payment" -> '+'  -- e.g. Liabilities:CreditCard
                                    "repayment to me" -> '-'  -- e.g. Liabilities:Bob
                                    "repayment to them" -> '+'  -- e.g. Liabilities:Bob
  | accountType == "Equity" = case keyword of
                                "increase" -> '-'
                                "decrease" -> '+'
                                "opening balance" -> '-'
  | accountType == "Income" = case keyword of
                                "increase" -> '-'
                                "decrease" -> '+'
                                "owed to me" -> '-'
                                "earned" -> '-'  -- e.g. Income:Salary
                                "income" -> '-'  -- e.g. Income:Salary
  | accountType == "Expenses" = case keyword of
                                    "increase" -> '+'
                                    "decrease" -> '-'
                                    "spent" -> '+'  -- e.g. Expenses:Groceries
                                    "expense" -> '+'  -- e.g. Expenses:Groceries
                                    "rebate" -> '-'

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

keywordSign :: RawAccountPart -> Maybe Char
keywordSign (RawAccountPart _ Nothing _ _ _) = Nothing
keywordSign (RawAccountPart name (Just keyword) _ _ _) = Just $ keywordToSign (figureOutAccountType name) keyword

withKeywordSign' :: RawAccountPart -> RawAccountPart
withKeywordSign' rap@(RawAccountPart name tkw ca _ ars) = RawAccountPart name tkw ca (keywordSign rap) ars

withKeywordSign :: [RawAccountPart] -> [RawAccountPart]
withKeywordSign = map withKeywordSign'

forceArrowSign :: Char -> RawAccountPart -> RawAccountPart
forceArrowSign c (RawAccountPart name tkw ca kws _) = RawAccountPart name tkw ca kws (Just c)

withArrowSign :: [RawAccountPart] -> String -> [RawAccountPart] -> ([RawAccountPart], [RawAccountPart])
withArrowSign acclines1 "->" acclines2 = (map (forceArrowSign '-') acclines1, map (forceArrowSign '+') acclines2)
withArrowSign acclines1 "<-" acclines2 = (map (forceArrowSign '+') acclines1, map (forceArrowSign '-') acclines2)
withArrowSign acclines1 _ acclines2 = (acclines1, acclines2)

withBothSigns :: [RawAccountPart] -> String -> [RawAccountPart] -> [RawAccountPart]
withBothSigns acclines1 arr acclines2 = withKeywordSign (xs1 ++ xs2)
                                        where
                                            (xs1, xs2) = withArrowSign acclines1 arr acclines2

-- Make sure the keyword-inferred sign is the same as the arrow-inferred sign
validateRawAccountPartSign :: RawAccountPart -> Either String RawAccountPart
validateRawAccountPartSign (RawAccountPart name tkw ca (Just '+') (Just '-')) = Left ("In account part " ++ name ++ " the keyword sign is + but arrow sign is -")
validateRawAccountPartSign (RawAccountPart name tkw ca (Just '-') (Just '+')) = Left ("In account part " ++ name ++ " the keyword sign is - but arrow sign is +")
validateRawAccountPartSign (RawAccountPart name tkw ca (Just '+') _) = Right (RawAccountPart name tkw ca (Just '+') (Just '+'))
validateRawAccountPartSign (RawAccountPart name tkw ca (Just '-') _) = Right (RawAccountPart name tkw ca (Just '-') (Just '-'))
validateRawAccountPartSign (RawAccountPart name tkw ca _ (Just '+')) = Right (RawAccountPart name tkw ca (Just '+') (Just '+'))
validateRawAccountPartSign (RawAccountPart name tkw ca _ (Just '-')) = Right (RawAccountPart name tkw ca (Just '-') (Just '-'))
validateRawAccountPartSign (RawAccountPart name tkw ca _ _) = Left ("In account part " ++ name ++ " cannot figure out sign!")

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
                _ <- some spaceChar
                acclines1 <- some (accountPart <* some spaceChar)
                arrowAndBeyond <- optional . try $ do
                                        ar <- arrow
                                        _ <- some spaceChar
                                        acclines2 <- some (accountPart <* some spaceChar)
                                        return (ar, acclines2)
                let rawTransaction = case arrowAndBeyond of
                                        Nothing -> (d, nar, acclines1, "(no arrow)", [])
                                        Just (ar, acclines2) -> (d, nar, acclines1, ar, acclines2)
                let (d, nar, acclines1, arr, acclines2) = rawTransaction
                let raps = withBothSigns acclines1 arr acclines2
                let raps' = sequence $ map validateRawAccountPartSign raps
                return $ case raps' of
                            Left e -> Left e
                            Right x -> case validateRawAccountParts x of
                                            Left e -> Left e
                                            Right v -> Right (Transaction d nar v)
