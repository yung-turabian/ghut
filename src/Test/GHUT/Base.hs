
{-# LANGUAGE FlexibleInstances #-}

module Test.GHUT.Base (
 hwAssert,
 hwAssertTrue,
 hwAssertFalse,
 hwTestFunction
) where

-- Hidden utility
hwPrintSuccess :: String -> IO ()
hwPrintSuccess msg = putStrLn ("[GHUNIT : Function] " ++ msg ++ " \x1b[1;92m✓\x1b[0m")

hwPrintFailure :: String -> IO ()
hwPrintFailure msg = putStrLn ("[GHUNIT : Function] " ++ msg ++ " \x1b[1;91m✗\x1b[0m")

hwListCmp :: (Num a, Eq a) => [a] -> [a] -> Bool
hwListCmp [] [] = True
hwListCmp _ [] = False
hwListCmp [] _ = False
hwListCmp (it1:list1) (it2:list2) =
 if it1 == it2 then hwListCmp list1 list2
 else False


class TestFunction a where
 hwTestFunction :: String -> [(String, a, a)] -> IO ()

instance TestFunction Int where
  hwTestFunction msg tests = do
    let results = map (\(args, expected, actual) ->
                         (args, expected, actual, expected == actual)) tests
    let failedTests = filter (\(_, _, _, passed) -> not passed) results
    if null failedTests
     then hwPrintSuccess msg
     else do
      hwPrintFailure msg
      mapM_ (\(args, expected, actual, _) -> putStrLn ("\t" ++ msg ++ " " ++ args ++ "\n\t\tExpected -> " ++ show expected ++ "\n\t\tActual -> " ++ show actual)) failedTests

instance TestFunction [Int] where
 hwTestFunction msg tests = do
   let results = map (\(args, expected, actual) ->
                      (args, expected, actual, hwListCmp expected actual)) tests
   let failedTests = filter (\(_, _, _, passed) -> not passed) results
   if null failedTests
     then hwPrintSuccess msg
     else do
      hwPrintFailure msg
      mapM_ (\(args, expected, actual, _) -> putStrLn ("\t" ++ msg ++ " " ++ args ++ "\n\t\tExpected -> " ++ show expected ++ "\n\t\tActual -> " ++ show actual)) failedTests



class Assert a where
 hwAssert :: a -> a -> Bool

instance Assert Int where
 hwAssert expected actual =
  if expected == actual then True
  else False

instance Assert [Int] where
 hwAssert expected actual =
  if hwListCmp expected actual then True
  else False


hwAssertTrue :: Bool -> Bool -> Bool
hwAssertTrue expected actual =
  if expected == actual then True 
  else False

hwAssertFalse :: Bool -> Bool -> Bool
hwAssertFalse expected actual =
  if expected == actual then True
  else False
