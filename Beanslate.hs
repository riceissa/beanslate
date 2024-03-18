import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad
import Data.Void (Void)

type Parser = Parsec Void String

-- Modified from https://github.com/JakeWheat/intro_to_parsing/blob/a84aca1c172f201e5457cfa2f190cf98cd120d06/FunctionsAndTypesForParsing.lhs#L49-L51
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) "(source unknown)"
                        where
                         leftOver = manyTill anySingle eof

putError (Left e) = putStr $ errorBundlePretty (e :: ParseErrorBundle String Void)

parseOrPrintError p input = case parseWithLeftOver p input of
                                Right x -> print x
                                Left e -> putError (Left e)

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
transaction :: Parser (Date, String, [(String, Maybe String, Maybe (String, String))])
transaction = do
                d <- date
                _ <- some spaceChar
                nar <- narration
                _ <- some spaceChar
                acclines <- sepBy1 (do
                                    a <- accountPart
                                    _ <- many spaceChar
                                    return a)
                                    (do
                                    arrow
                                    optional (some spaceChar))
                return (d, nar, acclines)
