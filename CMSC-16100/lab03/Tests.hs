module Tests where

import Lab3

data TestResult
    = Success
    | Failure String

isSuccess :: TestResult -> Bool
isSuccess Success = True
isSuccess _       = False

message :: TestResult -> String
message Success           = "Success!"
message (Failure message) = message

-- Test a function that takes one argument.
-- Usage: expect1 "myFunc" myFunc arg expectedOutput
expect1 :: (Show a, Show b, Eq b) => String -> (a -> b) -> a -> b -> TestResult
expect1 funcName func input expectedOutput =
    if expectedOutput == actual then
        Success
    else
        Failure $
            "Expected " ++ evaledStr ++
            " to be " ++ show expectedOutput ++
            ", but got " ++ show actual
    where
        actual    = func input
        evaledStr = funcName ++ " " ++ show input


-- This is where you add new test cases.
tests :: [TestResult]
tests =
    [ expect1 "eval" eval
        (Number (-3))
        (-3)
    , expect1 "eval" eval
        (Mult (Plus (Number 2) (Number (-6))) (Plus (Number 3) (Number 2)))
        (-20)
    , expect1 "eval" eval
        (Div (Number 13) (Number 6))
        2
    , expect1 "eval" eval
        (Plus (Mult (Number 2) (Number 3)) (Div (Number 13) (Number (-6))))
        4
    , expect1 "tokenize" tokenize
        "1+2"
        [NumTok 1, PlusTok, NumTok 2]
    , expect1 "tokenize" tokenize
        "1+12345"
        [NumTok 1, PlusTok, NumTok 12345]
      , expect1 "tokenize" tokenize
        "1+-2"
        [NumTok 1, PlusTok, NumTok (-2)]
     , expect1 "tokenize" tokenize
        "1 * -2"
         [NumTok 1, MultTok, NumTok (-2)]
     , expect1 "tokenize" tokenize
         "1 + 2 * 3 + 4"
         [NumTok 1, PlusTok, NumTok 2, MultTok, NumTok 3, PlusTok, NumTok 4]
     , expect1 "tokenize" tokenize
         "1 * 2 + 3 + 4"
         [NumTok 1, MultTok, NumTok 2, PlusTok, NumTok 3, PlusTok, NumTok 4]

     , expect1 "tokenize" tokenize
         "(1+2)"
          [ParensTok[NumTok 1, PlusTok, NumTok 2]]
     , expect1 "tokenize" tokenize
         " (1 + 2 )"
         [ParensTok[NumTok 1, PlusTok, NumTok 2]]
     , expect1 "tokenize" tokenize
         "(1 * 2 + 3)"
         [ParensTok[NumTok 1, MultTok, NumTok 2, PlusTok, NumTok 3]]
     , expect1 "tokenize" tokenize
         "(-10 + 2) * 5"
        [ParensTok [NumTok (-10), PlusTok, NumTok 2], MultTok, NumTok 5]
     , expect1 "tokenize" tokenize
         "5 * (-10 + (2 + 4) * 3)"
        [NumTok 5, MultTok, ParensTok [NumTok (-10), PlusTok, ParensTok [NumTok 2, PlusTok, NumTok 4], MultTok, NumTok 3]]
     , expect1 "tokenize" tokenize
         "5 * (-10 + (2 + 4) * 3) * (3 + 2)"
         [NumTok 5, MultTok , ParensTok [NumTok (-10), PlusTok, ParensTok [NumTok 2, PlusTok, NumTok 4], MultTok, NumTok 3], MultTok, ParensTok [NumTok 3, PlusTok, NumTok 2]]
    , expect1 "tokenize" tokenize
        "(2*(-10+2)+           5)"
       [ParensTok [NumTok 2, MultTok, ParensTok [NumTok (-10), PlusTok, NumTok 2], PlusTok, NumTok 5]]
    , expect1 "parse" parse
        [ParensTok [NumTok 2, MultTok, ParensTok [NumTok (-10), PlusTok, NumTok 2], PlusTok, NumTok 5]]
        (Plus (Mult (Number 2) (Plus (Number (-10)) (Number 2))) (Number 5))
    ]


-- Inspect the below in GHCi.

-- DO NOT MODIFY BELOW THIS LINE IN YOUR SUBMISSION --

successes       = filter isSuccess tests
failures        = filter (not . isSuccess) tests
failureMessages = map message failures

results =
    ( length successes
    , length failures
    , failureMessages
    )

showFailures :: IO ()
showFailures = mapM_ putStrLn failureMessages
