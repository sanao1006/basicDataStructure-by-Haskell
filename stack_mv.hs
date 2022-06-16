{-# LANGUAGE BangPatterns , BlockArguments ,FlexibleContexts ,FlexibleInstances ,OverloadedStrings ,TypeApplications ,MultiParamTypeClasses ,TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
import Data.IORef
import Control.Monad.Primitive(PrimMonad,PrimState)
import Data.Primitive.MutVar
import Control.Monad.ST
import qualified Data.Vector.Unboxing as VU


type Stack m a = MutVar m (VU.Vector a)

initStack ::(PrimMonad m, VU.Unboxable a) => m(Stack(PrimState m)a)
initStack = newMutVar VU.empty

pushStack ::(PrimMonad m, VU.Unboxable a) => Stack (PrimState m) a -> a -> m()
pushStack st x = modifyMutVar' st (`VU.snoc` x)

popStack :: (PrimMonad m,VU.Unboxable a) => Stack (PrimState m) a -> m()
popStack st = modifyMutVar' st VU.init

topStack :: (PrimMonad m, VU.Unboxable a) => Stack(PrimState m) a -> m a
topStack st = VU.last <$> readMutVar st

sizeStack ::(PrimMonad m, VU.Unboxable a) => Stack(PrimState m) a -> m Int
sizeStack st = VU.length <$> readMutVar st

emptyStack :: (PrimMonad m, VU.Unboxable a) => Stack(PrimState m) a -> m Bool
emptyStack st = do
  stack <- readMutVar st
  if VU.null stack then pure True 
  else pure False 


main :: IO ()
main = do
  a <- initStack ::IO(Stack(PrimState IO)Int)
  pushStack a 100
  pushStack a 200
  pushStack a 10
  popStack a
  print =<< readMutVar a
  popStack a
  popStack a
  print =<< emptyStack a
  pushStack a 20
  print =<< topStack a

  
  let stack_st = runST $ do
      b <- initStack ::  ST s(Stack(PrimState(ST s))Int)
      pushStack b 100
      pushStack b 10
      sizeStack b
  print stack_st
  
