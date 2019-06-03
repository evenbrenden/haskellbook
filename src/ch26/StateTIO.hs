module StateTIO where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import System.Random

type Money = Integer
type Eyes = Integer

dieString :: Eyes -> Eyes -> Eyes -> String
dieString dice1 dice2 sum = mconcat [ show dice1, " + ", show dice2, " = ", show sum ]

roll :: IO (Eyes, Eyes)
roll = do
    dice1 <- rollDice
    dice2 <- rollDice
    return (dice1, dice2)
    where
        rollDice = getStdRandom (randomR (1 :: Eyes, 6 :: Eyes))

anotherRound :: StateT Money IO ()
anotherRound = do
        money <- get
        liftIO $ putStr $ mconcat [ "You're holding $", show money, ", hit it? (y/n) " ]
        hitMe <- liftIO getLine
        case hitMe of
            "y" -> do
                (dice1, dice2) <- liftIO roll
                let sum = dice1 + dice2
                liftIO $ putStr $ mconcat [ "Rolled ", dieString dice1 dice2 sum ]
                case sum == 7 || sum == 11 of
                    True -> do
                        modify (*2)
                        liftIO $ putStrLn "!"
                        anotherRound
                    False -> do
                        liftIO $ putStrLn $ mconcat [ ", too bad!" ]
            "n" -> do
                liftIO $ putStrLn $ mconcat [ "Got out at $", show money, " - smart!" ]
            _ -> do anotherRound

main :: IO ()
main = do
    putStrLn "***********************************"
    putStrLn "**** 7-11 DICE GAME FOR IDIOTS ****"
    putStrLn "***********************************"
    putStrLn ""
    putStrLn "Roll a sum of 7 or 11 to double your bet."
    putStrLn "Roll anything else and lose it all."
    putStrLn ""
    putStr "Place your bet ($): "
    bet' <- getLine
    let bet = read bet'
    execStateT anotherRound bet
    return ()
