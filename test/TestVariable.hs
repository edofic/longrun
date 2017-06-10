module TestVariable (
    testVariable
) where

import System.Log.Logger hiding (logM)
import Test.Framework (testGroup, Test)
import Utils (testLogsOfMatch)
import qualified Control.Concurrent.Longrun as Longrun


testVariable :: Test
testVariable = testGroup "testVariable"
    [ testVar1
    , testVar2
    ]

-- | Basic variable
var1 :: Longrun.Process ()
var1 = do
    var <- Longrun.newVar "content"
    val1 <- Longrun.getVar var
    Longrun.logM INFO $ show val1
    Longrun.setVar var "new content"
    val2 <- Longrun.getVar var
    Longrun.logM INFO $ show val2

testVar1 :: Test
testVar1 = testLogsOfMatch "var1" DEBUG var1
    [ (INFO, "\"content\"")
    , (INFO, "\"new content\"")
    ]


-- | Modify variable
var2 :: Longrun.Process ()
var2 = do
    var <- Longrun.newVar (1::Int)
    (oldVal,newVal) <- Longrun.modifyVar var (+1)
    val <- Longrun.getVar var
    Longrun.logM INFO $ show oldVal
    Longrun.logM INFO $ show newVal
    Longrun.logM INFO $ show val

testVar2 :: Test
testVar2 = testLogsOfMatch "var2" DEBUG var2
    [ (INFO, "1")
    , (INFO, "2")
    , (INFO, "2")
    ]
