{-# LANGUAGE BangPatterns , BlockArguments ,FlexibleContexts ,FlexibleInstances ,OverloadedStrings ,TypeApplications ,MultiParamTypeClasses ,TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
import Data.IORef
import Control.Monad.Primitive(PrimMonad,PrimState)
import Data.Primitive.MutVar
import Control.Monad.ST
import qualified Data.Vector.Unboxing as VU

type Queue m a = MutVar m(VU.Vector a)
type Stack m a = MutVar m (VU.Vector a)

initQueue  :: (PrimMonad m, VU.Unboxable a) => m(Queue(PrimState m)a)
initQueue = newMutVar VU.empty
initStack ::(PrimMonad m, VU.Unboxable a) => m(Stack(PrimState m)a)
initStack = newMutVar VU.empty

pushQueue :: (PrimMonad m, VU.Unboxable a) => Queue(PrimState m) a -> a -> m()
pushQueue que x = modifyMutVar' que (`VU.snoc` x)
pushStack ::(PrimMonad m, VU.Unboxable a) => Stack (PrimState m) a -> a -> m()
pushStack st x = modifyMutVar' st (`VU.snoc` x)

popQueue :: (PrimMonad m,VU.Unboxable a) => Queue (PrimState m) a -> m()
popQueue que = modifyMutVar' que VU.tail
popStack :: (PrimMonad m,VU.Unboxable a) => Stack (PrimState m) a -> m()
popStack st = modifyMutVar' st VU.init

topStack :: (PrimMonad m, VU.Unboxable a) => Stack(PrimState m) a -> m a
topStack st = VU.last <$> readMutVar st
frontQueue :: (PrimMonad m, VU.Unboxable a) => Queue(PrimState m) a -> m a
frontQueue que = VU.head <$> readMutVar que
backQueue :: (PrimMonad m, VU.Unboxable a) => Queue(PrimState m) a -> m a
backQueue que = VU.last <$> readMutVar que


sizeStack ::(PrimMonad m, VU.Unboxable a) => Stack(PrimState m) a -> m Int
sizeStack st = VU.length <$> readMutVar st
sizeQueue :: (PrimMonad m, VU.Unboxable a) => Queue(PrimState m) a -> m Int
sizeQueue que = VU.length <$> readMutVar que


emptyStack :: (PrimMonad m, VU.Unboxable a) => Stack(PrimState m) a -> m Bool
emptyStack st = do
  stack <- readMutVar st
  if VU.null stack then pure True 
  else pure False 
emptyQueue :: (PrimMonad m, VU.Unboxable a) => Queue(PrimState m) a -> m Bool
emptyQueue que = do
  stack <- readMutVar que
  if VU.null stack then pure True 
  else pure False 

type Deque m a = MutVar m(Seq.Seq a)

initDeque :: (PrimMonad m) => m(Deque (PrimState m) a)
initDeque = newMutVar (Seq.empty)
{-# INLINE initDeque #-}

deqSingleton :: (PrimMonad m) => a -> m(Deque(PrimState m)a)
deqSingleton x = newMutVar(Seq.singleton x)
{-# INLINE deqSingleton #-}

addLast :: (PrimMonad m) => Deque(PrimState m)a -> a -> m()
addLast deq x = modifyMutVar deq(flip(Seq.|>)x)
{-# INLINE addLast #-}

addFirst :: (PrimMonad m) => Deque(PrimState m)a -> a -> m()
addFirst deq x = modifyMutVar deq((Seq.<|)x)
{-# INLINE addFirst #-}

popLast :: (PrimMonad m) => Deque(PrimState m)a -> m()
popLast deq = modifyMutVar deq(seqInit.Seq.viewr)
	where
    	seqInit(seqRest Seq.:> seqLast) = seqRest
{-# INLINE popLast #-}

popFirst :: (PrimMonad m) => Deque(PrimState m)a -> m()
popFirst deq = modifyMutVar deq(seqTail.Seq.viewl)
	where
    	seqTail(seqTop Seq.:< seqRest) = seqRest
{-# INLINE popFirst #-}

peekFirst :: (PrimMonad m) => Deque(PrimState m)a -> m a
peekFirst deq = seqHead . Seq.viewl <$> readMutVar deq 
    where
    	seqHead(seqTop Seq.:< seqRest) = seqTop
{-# INLINE peekFirst #-}

peekLast :: (PrimMonad m) => Deque(PrimState m)a -> m a
peekLast deq = seqLast . Seq.viewr <$> readMutVar deq 
    where
    	seqLast(seqRest Seq.:> seqEnd) = seqEnd
{-# INLINE peekLast #-}

contains :: (PrimMonad m, Eq a) => Deque(PrimState m)a -> a -> m Bool
contains deq x = do
	a <- readMutVar deq
	case ((Seq.elemIndexL)x a) of
		Just _ -> pure True
		Nothing -> pure False
{-# INLINE contains #-}

deqLen :: (PrimMonad m) => Deque(PrimState m)a -> m Int
deqLen deq = Seq.length <$> readMutVar deq
{-# INLINE deqLen #-}

deqIns :: (PrimMonad m) => Deque(PrimState m) a -> Int -> a -> m()
deqIns deq i x = modifyMutVar deq(Seq.insertAt i x)
{-# INLINE deqIns #-}

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
  
