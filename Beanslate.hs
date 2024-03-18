import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad
import Data.Void (Void)

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

data TransactionAccountLine = TransactionAccountLine
    { talAccountName :: String
    , talAmount :: Int
    , talSign :: Char
    }

data Transaction = Transaction
    { txnDate :: Date
    , txnNarration :: String
    , txnAccountLines :: [TransactionAccountLine]
    }

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

unsignedAmount :: Parser (String, String)
unsignedAmount = do
                    n <- unsignedValue
                    _ <- some spaceChar
                    c <- currency
                    return (n, c)

--                    (account name, transaction keyword, amount)
accountPart :: Parser (String, Maybe String, Maybe (String, String))
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

-- transaction :: Parser Transaction
transaction :: Parser (Date, String, [(String, Maybe String, Maybe (String, String))], String, [(String, Maybe String, Maybe (String, String))])
transaction = do
                d <- date
                _ <- some spaceChar
                nar <- narration
                _ <- some spaceChar
                acclines1 <- some (accountPart <* some spaceChar)
                arrowAndBeyond <- optional . try $ do
                                        _ <- many spaceChar
                                        ar <- arrow
                                        _ <- some spaceChar
                                        acclines2 <- some (accountPart <* some spaceChar)
                                        return (ar, acclines2)
                return $ case arrowAndBeyond of
                            Nothing -> (d, nar, acclines1, "(no arrow)", [])
                            Just (ar, acclines2) -> (d, nar, acclines1, ar, acclines2)
