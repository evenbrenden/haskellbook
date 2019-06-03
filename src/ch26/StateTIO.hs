module StateTIO where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import System.Random

dieString :: Integer -> Integer -> String
dieString dice1 dice2 = mconcat [ show dice1, " + ", show dice2, " = ", show (dice1 + dice2) ]

roll :: IO (Integer, Integer)
roll = do
    dice1 <- rollDice
    dice2 <- rollDice
    return (dice1, dice2)
    where
        rollDice = getStdRandom (randomR (1 :: Integer, 6))

anotherRound :: StateT Int IO ()
anotherRound = do
        money <- get
        liftIO $ putStrLn $ mconcat [ "You're holding $", show money, ", hit it? (y/n)" ]
        hitMe <- liftIO getLine
        case hitMe of
            "y" -> do
                (dice1, dice2) <- liftIO roll
                let sum = dice1 + dice2
                let str = dieString dice1 dice2
                case sum == 7 || sum == 11 of
                    True -> do
                        modify (*2)
                        liftIO $ putStrLn $ mconcat [ str, "!" ]
                        anotherRound
                    _ -> liftIO $ putStrLn $ mconcat [ str, ", too bad." ]
            "n" -> do
                liftIO $ putStrLn $ mconcat [ "Got out at $", show money ]
            _ -> do anotherRound

main :: IO ()
main = do
    putStrLn "************************"
    putStrLn "**** 7/11 DICE GAME ****"
    putStrLn "************************"
    putStrLn ""
    putStrLn "Place your bet ($): "
    bet' <- getLine
    let bet = read bet'
    execStateT anotherRound bet
    return ()
