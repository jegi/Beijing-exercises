{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module RegistryModel1 where

import Control.Concurrent
import Control.Exception(try,ErrorCall)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import StateModel
import Registry


data RegState = RegState{
    tids :: [Step],
    regs :: [(String,Step)]
  }
  deriving Show

instance StateModel RegState where
  data Action RegState = Spawn
                       | WhereIs String
                       | Register String Step
                       | Unregister String
    deriving Show

  data Ret RegState = Tid ThreadId
                    | None ()
    deriving (Eq,Show)

  type ActionMonad RegState = IO

  arbitraryAction :: RegState -> Gen (Action RegState)
  arbitraryAction s =
    oneof [return Spawn,
           Register
             <$> arbitraryName
             <*> elements (tids s)
          ]

  initialState :: RegState
  initialState = RegState [] []

  nextState :: RegState -> Action RegState -> Step -> RegState
  nextState s Spawn step =
    s{tids = step:tids s}
  nextState s (Register name tid) _step =
    s{regs = (name,tid):regs s}
  nextState s _  _ = s

  precondition :: RegState -> Action RegState -> Bool
  precondition s (Register name step) =
       step `elem` tids s
    && name `notElem` map fst (regs s)
  precondition _ _                    = True

  needs :: Action RegState -> [Step]
  needs (Register _ step) = [step]
  needs _                 = []

  perform :: Action RegState -> [Ret RegState] -> IO (Ret RegState)
  perform Spawn _
    = Tid  <$> forkIO (threadDelay 10000000)
  perform (Register name _step) [Tid tid]
    = None <$> register name tid

arbitraryName :: Gen String
arbitraryName = elements allNames

allNames :: [String]
allNames = ["a", "b", "c", "d", "e"]

prop_Registry :: Script RegState -> Property
prop_Registry s = monadicIO $ do
  _ <- run cleanUp
  _ <- runScript s
  assert True

cleanUp :: IO [Either ErrorCall ()]
cleanUp = sequence
  [try (unregister name) :: IO (Either ErrorCall ())
   | name <- allNames]
