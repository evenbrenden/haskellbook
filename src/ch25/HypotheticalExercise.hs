module HypotheticalExercise where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Maybe

newtype Reader r a = Reader { runReader :: r -> a }

rm :: ReaderT a Maybe a
rm = ReaderT (\x -> Just x)

mr :: MaybeT (HypotheticalExercise.Reader (Maybe a)) a
mr = MaybeT (HypotheticalExercise.Reader id)

runRm :: Maybe a -> Maybe a
runRm = HypotheticalExercise.runReader $ runMaybeT mr

runMr :: a -> a
runMr a = fromJust (runReaderT rm a)

-- Do they do the same thing? Not really. There's no reason for one to do to the other as the other does to the one. "Monads do not commute".
