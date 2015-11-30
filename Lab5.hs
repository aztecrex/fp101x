module Lab5 where

import Control.Monad
import Control.Applicative

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================


action :: Concurrent a -> Action
action (Concurrent f) = f $ const Stop


-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
stop = Concurrent $ \_ -> Stop


-- ===================================
-- Ex. 2
-- ===================================

atom :: IO a -> Concurrent a
atom ioa = Concurrent $ \f -> Atom $ ioa >>= \a -> return $ f a


-- ===================================
-- Ex. 3
-- ===================================

-- forke :: ((a -> Action) -> Action) -> (() -> Action) -> Action
-- forke fa = \f -> f ()

fork :: Concurrent a -> Concurrent ()
fork t = Concurrent $ \f -> Fork (action t) (f ())

par :: Concurrent a -> Concurrent a -> Concurrent a
par (Concurrent g) (Concurrent h) = Concurrent $ \f -> Fork (g f) (h f)


-- ===================================
-- Ex. 4
-- ===================================

-- stop the warnings!
instance Functor Concurrent where
  fmap f (Concurrent g)= Concurrent $ \h -> g $ \x -> h (f x)


instance Applicative Concurrent where
  pure x = Concurrent $ \c -> c x
  (<*>) = error "not implemented"


(>>==) :: ((a -> Action) -> Action) -> (a -> ((b -> Action) -> Action)) -> ((b -> Action) -> Action)
-- ma >>== f = \h -> ma (flip f h)
ma >>== f = \h -> ma $ \j -> f j h

cunpack :: Concurrent a -> ((a -> Action) -> Action)
cunpack (Concurrent f) = f

cpack :: ((a -> Action) -> Action) -> Concurrent a
cpack = Concurrent

capply :: (a -> Concurrent b) -> a -> ((b -> Action) -> Action)
capply f = cunpack . f

instance Monad Concurrent where
    (>>=) m f = let ma = cunpack m
                in cpack $ \h -> ma $ \j -> capply f j h
    return = pure


-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin = error "You have to implement roundRobin"

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331)
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs
