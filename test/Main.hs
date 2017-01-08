module Main where

import Test.Framework (defaultMain)
import TestLongrun (testLongrun)
import TestQueue (testQueue)
import TestSubprocess (testSubprocess)
import TestTimer (testTimer)
import TestVariable (testVariable)
import TestWait (testWait)
import TestBase (testBase)


main :: IO ()
main = defaultMain
    [ testBase
    , testLongrun
    , testQueue
    , testSubprocess
    , testTimer
    , testVariable
    , testWait
    ]
