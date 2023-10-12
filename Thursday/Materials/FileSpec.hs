{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, FlexibleContexts #-}

module FileSpec where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Test.QuickCheck
import Test.QuickCheck.Monadic
import StateModel
import Data.Maybe

type FilePtr = Ptr Int -- lying here! But no-one will know.

-- The functions under test.

foreign import ccall "fopen"  fopen  :: CString -> CString -> IO FilePtr
foreign import ccall "fread"  fread  :: Ptr CChar -> CInt -> CInt -> FilePtr -> IO CInt
foreign import ccall "fwrite" fwrite :: Ptr CChar -> CInt -> CInt -> FilePtr -> IO ()
foreign import ccall "fseek"  fseek  :: FilePtr -> CLong -> CInt -> IO ()
foreign import ccall "fclose" fclose :: FilePtr -> IO ()

_SEEKSET = 0  -- The SEEKSET macro. It is always zero, though.

-- Wrappers to make open, read, write and seek easier to call.

open :: String -> String -> IO FilePtr
open name options =
  withCString name $ \name' ->
  withCString options $ \options' ->
  fopen name' options'

-- Write s to the file at the current offset.
write :: FilePtr -> String -> IO ()
write f s =
  withCString s $ \string -> fwrite string 1 (fromIntegral $ length s) f

-- Read (up to) k bytes from the file at the current offset
read_ :: Integral a => FilePtr -> a -> IO String
read_ f k = 
  withCString (replicate 100 ' ') $ \buffer -> do
    i <- fread buffer 1 (fromIntegral k) f
    peekCStringLen (buffer,fromIntegral i)

-- Move the current offset to k bytes from the beginning of the file
seek_ :: Integral a => FilePtr -> a -> IO ()
seek_ f k =
  fseek f (fromIntegral k) _SEEKSET

-- A simple test program. Should return ("Hello"," worl","d!")
test = do
  file <- withCString "data" $ \name -> withCString "wb+" $ fopen name
  write file "Hello world!"
  seek_ file 0
  s  <- read_ file 5
  s2 <- read_ file 5
  s3 <- read_ file 5
  fclose file
  return (s,s2,s3)

-- A simple state-machine property
data State = State{fileptr :: Maybe Step}
  deriving Show

instance StateModel State where
  data Action State = FOpen
    deriving Show
  data Ret State = FilePtr FilePtr
    deriving Show
  type ActionMonad State = IO

  arbitraryAction s = return FOpen
  initialState = State Nothing
  nextState s _ step = s{fileptr = Just step}
  precondition s FOpen = isNothing $ fileptr s
  perform FOpen [] = FilePtr <$> open "test.dat" "wb"

prop_FileIO :: Script State -> Property
prop_FileIO s = monadicIO $ do
  steps <- runScript s
  case steps of
    (Step 1,FilePtr f):_ -> run $ fclose f
    []                   -> return ()
  assert True
  