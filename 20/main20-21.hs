import           Data.Char                    (isLetter)
import           Data.Function                (on)
import           Data.List                    (find, intersperse, sortBy)
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import           System.Environment
import           Text.ParserCombinators.ReadP

main = do
    args <- getArgs
    rawInput <- lines <$> readFile (args !! 0)
    let foods = map (parse (food <* eof)) rawInput

    let allIngredients = S.fromList $ concat $ map fst foods
    let allergenPossibilities = ingredientAllergens foods
    let possibleAllergens = S.unions $ M.elems allergenPossibilities
    let safeIngredients = S.difference allIngredients possibleAllergens

    print $ length $ concat $ map (filter (`S.member` safeIngredients) . fst) foods -- 2556
    print $ concat $ intersperse "," $ map snd $ sortBy (compare `on` fst) $ matchAllergens allergenPossibilities -- vcckp,hjz,nhvprqb,jhtfzk,mgkhhc,qbgbmc,bzcrknb,zmh

type Food = ([Ingredient], [Allergen])
type Ingredient = String
type Allergen = String

matchAllergens :: M.Map Allergen (S.Set Ingredient) -> [(Allergen, Ingredient)]
matchAllergens allergenPossibilities
    | M.null allergenPossibilities = []
    | otherwise = (determinedAllergen, determinedIngredient) : (matchAllergens restOfPossibilities)
    where restOfPossibilities = M.map (S.delete determinedIngredient) $ M.delete determinedAllergen allergenPossibilities
          (determinedAllergen, determinedIngredient) = extractSingletonSet $ find ((==1) . S.size . snd) $ M.toList allergenPossibilities
          extractSingletonSet (Just (allergen, ingredients)) = (allergen, head $ S.toList ingredients)

ingredientAllergens :: [Food] -> M.Map Allergen (S.Set Ingredient)
ingredientAllergens foods = foldl1 (M.unionWith S.intersection) $ map possibleAllergens foods
    where possibleAllergens (ingredients, allergens) = M.fromList $ zip allergens (repeat (S.fromList ingredients))

parse :: ReadP a -> String -> a
parse parser = fst . head . readP_to_S parser

food = do
    ingredients <- ingredients
    optional (char ' ')
    allergens <- allergens
    return (ingredients, allergens)

ingredients = sepBy1 (munch1 isLetter) (char ' ')
allergens = parens ((string "contains ") *> sepBy1 (munch1 isLetter) (string ", "))
parens = between (string "(") (char ')')
