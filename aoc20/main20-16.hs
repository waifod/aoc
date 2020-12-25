import           Data.Either                   (rights)
import           Data.List                     (concat, delete, findIndex,
                                                isPrefixOf)
import           Data.Maybe                    (fromJust)
import           System.Environment
import           Text.ParserCombinators.Parsec

main = do
    args <- getArgs
    raw_input <- lines <$> readFile (args !! 0)
    let raw_field_rules:(_:raw_my_ticket:[]):(_:raw_nearby_tickets):[] = splitOn "" raw_input

    let field_rules = rights $ map (parse parseFieldRule "") raw_field_rules
    let my_ticket = map read $ splitOn ',' raw_my_ticket :: [Int]
    let nearby_tickets = map (map read . splitOn ',') raw_nearby_tickets :: [[Int]]

    print $ sum $ filter (obviouslyInvalidValue field_rules) (concat nearby_tickets) -- 22073

    let validTickets = filter (not . (obviouslyInvalidTicket field_rules)) nearby_tickets
    let fieldLocations = detectFieldLocations field_rules validTickets
    let departureFieldIndexes = map fst $ filter (\(i, FieldRule{fieldName=name}) -> "departure" `isPrefixOf` name) fieldLocations

    print $ product $ map (my_ticket !!) departureFieldIndexes -- 1346570764607

data FieldRule = FieldRule { fieldName::String, rangeA::(Int, Int), rangeB::(Int, Int) } deriving (Show, Eq)

detectFieldLocations :: [FieldRule] -> [[Int]] -> [(Int, FieldRule)]
detectFieldLocations [] tickets = []
detectFieldLocations fieldRules tickets = (nextFieldIndex, nextRule):(detectFieldLocations (delete nextRule fieldRules) tickets)
    where nextRule = head $ validRulesAtIndexes !! nextFieldIndex
          nextFieldIndex = fromJust $ findIndex ((==1) . length) validRulesAtIndexes
          validRulesAtIndexes = map (validRulesAtIndex fieldRules tickets) [0..totalFields-1]
          totalFields = length (head tickets)

validRulesAtIndex fieldRules tickets i = filter allValuesAtIndexValidForRule fieldRules
    where allValuesAtIndexValidForRule fieldRule = all (valueInsideBounds fieldRule) valuesAtIndex
          valuesAtIndex = map (!! i) tickets

obviouslyInvalidTicket :: [FieldRule] -> [Int] -> Bool
obviouslyInvalidTicket fieldRules ticketValues = any (obviouslyInvalidValue fieldRules) ticketValues

obviouslyInvalidValue :: [FieldRule] -> Int -> Bool
obviouslyInvalidValue fieldRules fieldValue = all (not . ((flip valueInsideBounds) fieldValue)) fieldRules

valueInsideBounds :: FieldRule -> Int -> Bool
valueInsideBounds FieldRule{rangeA=(rangeAMin, rangeAMax), rangeB=(rangeBMin, rangeBMax)} fieldValue =
    (rangeAMin <= fieldValue && fieldValue <= rangeAMax)
    || (rangeBMin <= fieldValue && fieldValue <= rangeBMax)

parseFieldRule :: Parser FieldRule
parseFieldRule = do
        fieldName <- many1 (noneOf ":")
        string ": "
        rangeAMin <- read <$> many1 digit
        char '-'
        rangeAMax <- read <$> many1 digit
        string " or "
        rangeBMin <- read <$> many1 digit
        char '-'
        rangeBMax <- read <$> many1 digit
        return (FieldRule {fieldName = fieldName, rangeA = (rangeAMin, rangeAMax) , rangeB = (rangeBMin, rangeBMax)})

splitOn delimiter xs = case break (== delimiter) xs of
    (y, _ : ys) -> y : splitOn delimiter ys
    (y, [])     -> [y]
