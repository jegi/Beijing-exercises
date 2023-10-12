{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RegistryModel2 where

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
                    | MaybeTid (Maybe ThreadId)
                    | Caught (Either ErrorCall ())
    deriving (Eq,Show)

  type ActionMonad RegState = IO

  arbitraryAction :: RegState -> Gen (Action RegState)
  arbitraryAction s =
    oneof [return Spawn,
           Register
             <$> arbitraryName
             <*> elements (tids s),
           Unregister
             <$> arbitraryName,
           WhereIs
             <$> arbitraryName
          ]

  initialState :: RegState
  initialState = RegState [] []

  nextState :: RegState -> Action RegState -> Step -> RegState
  nextState s Spawn step =
    s{tids = step:tids s}
  nextState s (Register name tid) _step =
    s{regs = (name,tid):regs s}
  nextState s (Unregister name) _step =
    s{regs = filter ((/=name) . fst) (regs s)}
  nextState s _  _ = s

  precondition :: RegState -> Action RegState -> Bool
  precondition s (Register _name step) =
       step `elem` tids s
  precondition s (Unregister name) =
       name `elem` map fst (regs s)
  precondition _ _                    = True

  postcondition ::
    RegState -> Action RegState -> (Step -> Ret RegState) -> Ret RegState -> Bool
  postcondition  s (WhereIs name)       stepValue (MaybeTid mtid) =
    (stepValue <$> lookup name (regs s)) == (Tid <$> mtid)
  postcondition _s Spawn                _         (Tid _)  = True
  postcondition  s (Register name step) _         (Caught res) =
    positive s (Register name step) == (res == Right ())
  postcondition _s (Unregister _name)   _         (None _) = True
  postcondition  _ _                    _         _        = False

  monitoring ::
       (RegState, RegState) -> Action RegState -> (Step -> Ret RegState)
    -> Ret RegState -> Property -> Property
  monitoring (_s, s') _act _ _ =
    counterexample $ "\nState: "++show s'++"\n"

  needs :: Action RegState -> [Step]
  needs (Register _ step) = [step]
  needs _                 = []

  perform :: Action RegState -> [Ret RegState] -> IO (Ret RegState)
  perform Spawn _
    = Tid  <$> forkIO (threadDelay 10000000)
  perform (Register name _step) [Tid tid]
    = Caught <$> try (register name tid)
  perform (Unregister name) []
    = None <$> unregister name
  perform (WhereIs name) []
    = MaybeTid <$> whereis name

positive :: RegState -> Action RegState -> Bool
positive s (Register name step) =
     name `notElem` map fst (regs s)
  && step `notElem` map snd (regs s)
positive _s _ = True

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

kill :: ThreadId -> IO ()
kill tid = do
  killThread tid
  yield

