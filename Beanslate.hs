import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad
import Data.Void (Void)
import Data.List (isPrefixOf)

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
    , talAmount :: Int
    , talSign :: Char
    } deriving (Eq, Show)

data Transaction = Transaction
    { txnDate :: Date
    , txnNarration :: String
    , txnAccountLines :: [TransactionAccountLine]
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

accountPart :: Parser (AccountName, Maybe TransactionKeyword, Maybe CurrenciedAmount)
accountPart = do
                ac <- accountName
                _ <- some spaceChar
                keyword <- optional . try $ do
                                              _ <- char '('
                                              kw <- some (satisfy (/= ')'))
                                              _ <- char ')'
                                              return kw
                _ <- some spaceChar
                am <- optional unsignedAmount
                return (ac, keyword, am)

arrow :: Parser String
arrow = string "->" <|> string "<-"

keywordSign :: (AccountName, Maybe TransactionKeyword, Maybe CurrenciedAmount) -> Maybe Char
keywordSign (name, Nothing, _) = Nothing
keywordSign (name, Just keyword, _) = Just $ keywordToSign (figureOutAccountType name) keyword

arrowSign :: String -> (AccountName, Maybe TransactionKeyword, Maybe CurrenciedAmount) -> Maybe Char
arrowSign "from" _ = Just '-'
arrowSign "to" _ = Just '+'
arrowSign _ _ = Nothing

-- TODO:
-- * make sure the arrow-inferred sign is the same as the keyword-inferred sign
-- * make sure at most one currencied amount is missing, and if it is missing, fill it in by calculating what it must be using the other amounts
-- * package all of that into a nice Transaction data type
-- validateTransaction :: ( Date
--                       , String
--                       , [(AccountName, Maybe TransactionKeyword, Maybe CurrenciedAmount)]
--                       , String
--                       , [(AccountName, Maybe TransactionKeyword, Maybe CurrenciedAmount)]
--                       ) -> Maybe Transaction
-- validateTransaction (d, nar, acclines1, ar, acclines2)
--   | ar == "(no arrow)" = do
--                             ac <- acclines1
--                             let kw = keywordSign ac
--                             case kw of
--                                 Just s -> TransactionAccountLine

-- transaction :: Parser Transaction
transaction :: Parser ( Date
                      , String
                      , [(AccountName, Maybe TransactionKeyword, Maybe CurrenciedAmount)]
                      , String
                      , [(AccountName, Maybe TransactionKeyword, Maybe CurrenciedAmount)]
                      )
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
                return rawTransaction
