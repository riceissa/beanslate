import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad
import Data.Void (Void)
import Data.List (isPrefixOf)
import Data.Maybe (isJust, fromJust)
import Data.Either (fromRight)
import System.IO
import Text.Pretty.Simple (pPrint)

type Parser = Parsec Void String

-- Modified from https://github.com/JakeWheat/intro_to_parsing/blob/a84aca1c172f201e5457cfa2f190cf98cd120d06/FunctionsAndTypesForParsing.lhs#L49-L51
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) "(source unknown)"
                        where
                         leftOver = manyTill anySingle eof

putError :: ParseErrorBundle String Void -> IO ()
putError e = putStr $ errorBundlePretty e

parseOrPrintError :: Show a => Parser a -> String -> IO ()
parseOrPrintError p input = case parseWithLeftOver p input of
                                Right x -> pPrint x
                                Left e -> putError e
                                -- Left e -> putStrLn $ show e

-- Use like parseOrPrintErrorFromFile transaction "example.txt"
parseOrPrintErrorFromFile :: Show a => Parser a -> FilePath -> IO ()
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
                return x

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
currency = some upperChar

-- Parse an unsigned monetary amount like "12.50 USD".
unsignedAmount :: Parser (Either String CurrenciedAmount)
unsignedAmount = do
                    n <- unsignedValue
                    c <- optional . try $ some spaceChar *> currency
                    return $ case c of
                                Nothing -> Left "Amount was found, but no currency (e.g. USD) was found!"
                                Just x -> Right $ CurrenciedAmount n x

accountPart :: Parser (Either String RawAccountPart)
accountPart = do
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
arrow = string "->" <|> string "<-"

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
validateSignedAccountParts :: [SignedAccountPart] -> Either String [TransactionAccountLine]
validateSignedAccountParts saps =
    let justs = filter hasAmount saps
        nothings = filter (not . hasAmount) saps
        sumOfJusts = sum (map (fromJust . signedAmount) justs) :: Float
    in case length nothings of
        0 -> if abs sumOfJusts < 0.0001
                then traverse sapToTal justs
                else Left "All amounts have been provided but don't sum to ~zero!"
        1 -> let insertIndex = length $ takeWhile hasAmount saps
                 missingValue = abs sumOfJusts
                 missingSap = head nothings
                 missingTal = sapToTal (SignedAccountPart
                                            (sapAccountName missingSap)
                                            (Just $ CurrenciedAmount
                                                (show missingValue) "USD")
                                            (sapSign missingSap))
                 (before, after) = splitAt insertIndex justs
             in (\x y z -> x ++ y ++ z) <$> traverse sapToTal before <*> sequence [missingTal] <*> traverse sapToTal after
        _ -> Left "More than one missing amount found!"

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
                            al1 <- sequence acclines1
                            al2 <- sequence acclines2
                            raps <- withBothSigns al1 arr al2
                            saps <- traverse rapToSap raps
                            tals <- validateSignedAccountParts saps
                            Right $ Transaction d nar tals
