{-# LANGUAGE OverloadedStrings #-}
import           Data.Char
import           Data.List
import qualified Data.Text          as T
import           System.Environment
import           Text.Read

goodpassports (x, y) = length (filter (==True) (map nocid x))

nocid x = not (or $ map (findstr "cid:") x)

findstr :: T.Text -> T.Text -> Bool
findstr word "" = False
findstr "" string = True
findstr word string = if (T.head word == T.head string) && findstr (T.tail word) (T.tail string) then True else findstr word (T.tail string)

passports x = partition (\x -> length x == 7 || length x == 8) $ map processpassport $ T.splitOn "\n\n" x
processpassport = concat . (map $ T.splitOn "\n") . (T.splitOn " ")

valid x = filter (\x -> x == True) (map passportcheck x)

main = do
   args <- getArgs
   content <- readFile (args !! 0)
   let (x, y) =  passports $ T.pack content
   let (k, p) = partition (\x -> length x == 8) x
   print p
   print $ length k + length (filter nocid p)
   print $ length (valid k) + length (valid (filter nocid p))

checkbyr x = if findstr "byr:" x then check p else False
   where
      check (Just k) = 1920 <= k && 2002 >= k
      check Nothing  = False
      p = readMaybe int :: Maybe Int
      int = drop 4 (T.unpack x)

checkiyr x = if findstr "iyr:" x then check p else False
   where
      check (Just k) = 2010 <= k && 2020 >= k
      check Nothing  = False
      p = readMaybe int :: Maybe Int
      int = drop 4 (T.unpack x)

checkeyr x = if findstr "eyr:" x then check p else False
   where
      check (Just k) = 2020 <= k && 2030 >= k
      check Nothing  = False
      p = readMaybe int :: Maybe Int
      int = drop 4 (T.unpack x)

checkhgt x = if findstr "hgt:" x then check height else False
   where
      height = drop 4 (T.unpack x)
      check h = if length h == 5 then cmcheck h else incheck h
      cmcheck h = (drop 3 h) == "cm" && cmnumcheck (readMaybe (take 3 h) :: Maybe Int)
      incheck h = (drop 2 h) == "in" && innumcheck (readMaybe (take 2 h) :: Maybe Int)
      cmnumcheck (Just a) = a >= 150 || a <= 193
      cmnumcheck Nothing  = False
      innumcheck (Just a) = a >= 59 || a <= 76
      innumcheck Nothing  = False

checkhcl x = if findstr "hcl:#" x then check else False
   where
      check = length str == 6 && (and $ map hexfilter str)
      str = drop 5 (T.unpack x)
      hexfilter k = k >= 'a' || k <= 'f' || isNumber k

checkpid x = if findstr "pid:" x then check else False
   where
      check = length str == 9 && (and $ map isNumber str)
      str = drop 4 (T.unpack x)

checkecl x = if findstr "ecl:" x then check else False
   where
      check = length str == 3 && eclcheck str
      str = drop 4 (T.unpack x)
      eclcheck "amb" = True
      eclcheck "blu" = True
      eclcheck "brn" = True
      eclcheck "gry" = True
      eclcheck "grn" = True
      eclcheck "hzl" = True
      eclcheck "oth" = True
      eclcheck _     = False

checkcid x = findstr "cid" x

passportcheck k = and (map whatever k)
   where whatever k = checkbyr k || checkhgt k || checkhcl k || checkecl k || checkpid k || checkiyr k || checkeyr k || checkcid k

whatever k = checkbyr k || checkhgt k || checkhcl k || checkecl k || checkpid k || checkiyr k || checkeyr k || checkcid k
