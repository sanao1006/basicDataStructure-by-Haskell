{-# LANGUAGE BangPatterns , BlockArguments ,FlexibleContexts ,FlexibleInstances ,OverloadedStrings ,TypeApplications ,MultiParamTypeClasses ,TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
import Data.IORef
import Control.Monad.Primitive(PrimMonad,PrimState)
import Data.Primitive.MutVar
import Control.Monad.ST
import qualified Data.Vector.Unboxing as VU

newtype Stack m a = Stack{stack :: MutVar(PrimState m)(VU.Vector a)}

initStack :: (PrimMonad m, VU.Unboxable a) => m(Stack m a)
initStack = Stack <$> newMutVar(VU.empty)

pushStack :: (PrimMonad m, VU.Unboxable a) => Stack m a -> a -> m()
pushStack st x = modifyMutVar'(stack st)(`VU.snoc`x)

popStack ::(PrimMonad m, VU.Unboxable a) => Stack m a -> m()
popStack st = modifyMutVar'(stack st)(VU.init)

topStack :: (PrimMonad m, VU.Unboxable a) => Stack m a -> m a
topStack st = VU.last<$>readMutVar(stack st)

sizeStack :: (PrimMonad m, VU.Unboxable a) => Stack m a -> m Int
sizeStack st = VU.length<$>readMutVar(stack st)

emptyStack :: (PrimMonad m, VU.Unboxable a) => Stack m a -> m Bool
emptyStack st = do
  stack' <- readMutVar(stack st)
  if VU.null stack' then pure True
  else pure False

newtype Queue m a = Queue{queue :: MutVar (PrimState m)(VU.Vector a)}

initQueue  :: (PrimMonad m, VU.Unboxable a) => m(Queue m a)
initQueue = Queue <$> newMutVar VU.empty

pushQueue :: (PrimMonad m, VU.Unboxable a) => Queue m a -> a -> m()
pushQueue que x = modifyMutVar' (queue que) (`VU.snoc` x)

popQueue :: (PrimMonad m,VU.Unboxable a) => Queue m a -> m()
popQueue que = modifyMutVar' (queue que) VU.tail

frontQueue :: (PrimMonad m, VU.Unboxable a) => Queue m a -> m a
frontQueue que = VU.head <$> readMutVar (queue que)
backQueue :: (PrimMonad m, VU.Unboxable a) => Queue m a -> m a
backQueue que = VU.last <$> readMutVar (queue que)

sizeQueue :: (PrimMonad m, VU.Unboxable a) => Queue m a -> m Int
sizeQueue que = VU.length <$> readMutVar (queue que)

emptyQueue :: (PrimMonad m, VU.Unboxable a) => Queue m a -> m Bool
emptyQueue que = do
  queue' <- readMutVar (queue que)
  if VU.null queue' then pure True 
  else pure False 

newtype Deque m a = Deque{deque :: MutVar(PrimState m)(Seq.Seq a)}

initDeque :: (PrimMonad m) => m(Deque m a)
initDeque = Deque <$> newMutVar (Seq.empty)
{-# INLINE initDeque #-}

deqSingleton :: (PrimMonad m) => a -> m(Deque m a)
deqSingleton x = Deque <$> newMutVar(Seq.singleton x)
{-# INLINE deqSingleton #-}

addLast :: (PrimMonad m) => Deque m a -> a -> m()
addLast deq x = modifyMutVar(deque deq)(flip(Seq.|>)x)
{-# INLINE addLast #-}

addFirst :: (PrimMonad m) => Deque m a -> a -> m()
addFirst deq x = modifyMutVar (deque deq)((Seq.<|)x)
{-# INLINE addFirst #-}

popLast :: (PrimMonad m) => Deque m a -> m()
popLast deq = modifyMutVar (deque deq)(seqInit.Seq.viewr)
	where
    	seqInit(seqRest Seq.:> seqLast) = seqRest
{-# INLINE popLast #-}

popFirst :: (PrimMonad m) => Deque m a -> m()
popFirst deq = modifyMutVar (deque deq)(seqTail.Seq.viewl)
	where
    	seqTail(seqTop Seq.:< seqRest) = seqRest
{-# INLINE popFirst #-}

peekFirst :: (PrimMonad m) => Deque m a -> m a
peekFirst deq = seqHead . Seq.viewl <$> readMutVar (deque deq) 
    where
    	seqHead(seqTop Seq.:< seqRest) = seqTop
{-# INLINE peekFirst #-}

peekLast :: (PrimMonad m) => Deque m a -> m a
peekLast deq = seqLast . Seq.viewr <$> readMutVar (deque deq) 
    where
    	seqLast(seqRest Seq.:> seqEnd) = seqEnd
{-# INLINE peekLast #-}

contains :: (PrimMonad m, Eq a) => Deque m a -> a -> m Bool
contains deq x = do
	deq' <- readMutVar (deque deq)
	case ((Seq.elemIndexL)x deq') of
		Just _ -> pure True
		Nothing -> pure False
{-# INLINE contains #-}

deqLen :: (PrimMonad m) => Deque m a -> m Int
deqLen deq = Seq.length <$> readMutVar (deque deq)
{-# INLINE deqLen #-}

deqIns :: (PrimMonad m) => Deque m a -> Int -> a -> m()
deqIns deq i x = modifyMutVar (deque deq)(Seq.insertAt i x)
{-# INLINE deqIns #-}
