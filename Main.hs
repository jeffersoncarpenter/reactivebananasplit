import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.List (nub)
import System.Random
import System.IO
import Debug.Trace
import Data.IORef
import Reactive.Banana
import Reactive.Banana.Frameworks

data Splits = Splits String ([Segment])
data Segment = Segment String Times
data Times = Times ([Int])

data DSplits = Rename String
             | AddSegment Segment

applyDSplits :: DSplits -> Splits -> Splits
applyDSplits (Rename name) (Splits _ ss) = Splits name ss
applyDSplits (AddSegment segment) (Splits name ss) = Splits name (segment : ss)
  

instance Show Splits where
  show (Splits name ss) = foldr (\s a -> a ++ show s ++ "\n") (name ++ "\n") ss

instance Show Segment where
  show (Segment name times) = name


emptySplits :: Splits
emptySplits = Splits "" []

main :: IO ()
main = do
  displayHelpMessage
  sources <- (newAddHandler)
  network <- setupNetwork sources
  actuate network
  eventLoop sources network

displayHelpMessage :: IO ()
displayHelpMessage = mapM_ putStrLn $
  "Commands are:":
  " rename - name splits":
  " addsegment - add a segment":
  " actuate - actuate event network":
  " quit - quit the program":
  "":
  []

-- Read commands and fire corresponding events
eventLoop :: (EventSource DSplits) -> EventNetwork -> IO ()
eventLoop (es) network = loop
  where
    loop = do
      putStr "> "
      hFlush stdout
      s <- getLine
      case s of
        "rename" -> do
          putStr "name: "
          hFlush stdout
          name <- getLine
          fire es (Rename name)
        "addsegment" -> do
          putStr "name: "
          hFlush stdout
          name <- getLine
          fire es (AddSegment (Segment name (Times [])))
        "actuate" -> actuate network
        "quit" -> return ()
        _ -> putStrLn $ s ++ " - unknown command"
      when (s /= "quit") loop
      
{-----------------------------------------------------------------------------
Event sources
------------------------------------------------------------------------------}
-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you
type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

{-----------------------------------------------------------------------------
Program logic
------------------------------------------------------------------------------}
-- Set up the program logic in terms of events and behaviors.
setupNetwork :: (EventSource DSplits) -> IO EventNetwork
setupNetwork (es) = compile $ do
  eEvents <- fromAddHandler (addHandler es)

  let splits = accumE emptySplits (fmap (\d -> applyDSplits d) eEvents)

  reactimate $ fmap print splits
