import Data.IORef

newtype Stack a = Stack{stack :: IORef [a]}

initStack :: IO(Stack a)
initStack = Stack <$> (newIORef [] :: IO(IORef [a]))

pushStack :: Stack a -> a -> IO()
pushStack st x = modifyIORef' (stack st) (<>[x])

popStack :: Stack a -> IO()
popStack st = modifyIORef (stack st) init

emptyStack :: Stack a -> IO Bool
emptyStack st = do
    b <- readIORef(stack st)
    pure $ emptyStack' b
        where
            emptyStack' :: [a] -> Bool
            emptyStack' b
                | null b = True
                | otherwise = False

sizeStack :: Stack a -> IO Int
sizeStack st = pure(length) <*> readIORef(stack st)

topStack :: Stack a -> IO a
topStack st = pure(last) <*> readIORef(stack st)

debugStack :: Show a => Stack a -> IO ()
debugStack st = print =<< readIORef(stack st)

main = do
    a <- initStack
    pushStack a 100
    pushStack a 1
    pushStack a 80
    print =<< readIORef(stack a)
    popStack a
    popStack a
    print =<< readIORef(stack a)
    print =<< sizeStack a
    print =<< emptyStack a
    pushStack a 2652690
    debugStack a
{-
[100,1,80]
[100]
1
False
[100,2652690]
-}
