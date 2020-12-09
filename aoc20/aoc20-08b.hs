{-# LANGUAGE RecordWildCards #-}
import qualified Data.Array         as A
import           Data.Char
import qualified Data.IntSet        as S
import           Data.String
import           Prelude
import           System.Environment
import           System.IO

data Op = Acc Int | Jmp Int | NOp Int

data VMState = VMState { acc    :: Int
                       , cursor :: Int
                       , done   :: S.IntSet
                       , ops    :: A.Array Int Op
                       }

storeOps :: [String] -> [Op]
storeOps [] = []
storeOps (ins:n:strs) = case ins of
                            "acc" -> Acc m : storeOps strs
                            "jmp" -> Jmp m : storeOps strs
                            "nop" -> NOp m : storeOps strs
    where m  = read $ if head n == '+' then tail n else n

eval' :: VMState -> Maybe Int
eval' c@VMState{..} | S.member cursor done        = Nothing
                    | cursor > snd (A.bounds ops) = Just acc
                    | otherwise                   = exec
    where exec = case ops A.! cursor of
                     Acc k -> eval' c{ acc = acc + k
                                     , cursor = cursor + 1
                                     , done = S.insert cursor done
                                     }
                     Jmp k -> eval' c{ cursor = cursor + k
                                     , done = S.insert cursor done
                                     }
                     _     -> eval' c{ cursor = cursor + 1
                                     }

eval :: VMState -> Int
eval c@VMState{..} = case ops A.! cursor of
                         Acc k -> eval c{ acc = acc + k
                                        , cursor = cursor + 1
                                        , done = newDone
                                        }
                         Jmp k -> case eval' c{ cursor = cursor + 1, done = newDone } of
                                      Just n -> n
                                      _      -> eval c{ cursor = cursor + k
                                                      , done = newDone
                                                      }
                         NOp k -> case eval' c{ cursor = cursor + k, done = newDone } of
                                      Just n -> n
                                      _      -> eval c{ cursor = cursor + 1
                                                      , done = newDone
                                                      }
    where newDone = S.insert cursor done

stdArray :: [a] -> A.Array Int a
stdArray xs = A.listArray (0, size - 1) xs
    where size = length xs

solve :: String -> Int
solve = eval . VMState 0 0 S.empty . stdArray . storeOps . words

main :: IO ()
main = do args <- getArgs
          content <- readFile (args !! 0)
          print $ solve content
