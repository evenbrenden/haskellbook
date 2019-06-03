module StateTIO where

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad
import System.Random

type Money = Integer
type Eyes = Integer

roll :: IO (Eyes, Eyes)
roll = do
    dice1 <- rollDice
    dice2 <- rollDice
    return (dice1, dice2)
    where
        rollDice = getStdRandom (randomR (1 :: Eyes, 6 :: Eyes))

sevenOrEleven :: Eyes -> Eyes -> Bool
sevenOrEleven dice1 dice2 = sum == 7 || sum == 11
    where
        sum = dice1 + dice2

anotherRound :: StateT Money (ExceptT () IO) ()
anotherRound = do
        money <- get
        liftIO $ putStr $ mconcat [ "You're holding $", show money, ", hit it? (y/n) " ]
        hitMe <- liftIO getLine
        case hitMe of
            "y" -> do
                (dice1, dice2) <- liftIO roll
                liftIO $ putStrLn $ mconcat [ "Rolled ", show dice1, " and ", show dice2 ]
                guard (sevenOrEleven dice1 dice2)
                modify (*2)
                anotherRound
            "n" -> return ()
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
    outcome <- runExceptT $ execStateT anotherRound bet
    case outcome of
        Left _ -> putStrLn "It does say \"for idiots\"."
        Right money -> putStrLn $ mconcat [ "Got out at $", show money, " - smart!" ]
