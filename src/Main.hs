
module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Exception
import Data.IORef
import System.Environment
import System.IO
import Network.Socket
import Data.List.Split

main :: IO ()
main = do  
  hSetBuffering stdout NoBuffering
  -- me :: Int is the port number of this process
  -- neighbours :: [Int] is a list of the port numbers of the initial neighbours
  -- During the execution, connections may be broken or constructed
  (me, neighbours) <- readCommandLineArguments

  putStrLn $ "I should be listening on port " ++ show me
  putStrLn $ "My initial neighbours are " ++ show neighbours

  -- Listen to the specified port.
  serverSocket <- socket AF_INET Stream 0
  setSocketOption serverSocket ReuseAddr 1
  bind serverSocket $ portToAddress me
  listen serverSocket 1024
  
  -- Let a seperate thread listen for incomming connections
  _ <- forkIO $ listenForConnections serverSocket

  -- Create mvar for the mvar lock
  mvarlock <- newEmptyMVar
  putMVar mvarlock True
  -- Start creating connections with every neighbour
  initialConnections me neighbours
  
  -- The main thread checks for commands
  commandCheck

{--
NIEUW STAPPENPLAN VOOR INITIELE CONNECTIES:  
1. Maak een functie die gewoon checkt of WIJ of de BUREN de connectie moeten opstellen (if me > x then...)
2. Als me > x, dan creeëren we de socket en daarna de handle.
3. We roepen een functie aan met de handle als argument IN EEN NIEUWE THREAD. We forken dus deze functie.
4.  (FUNCTIE A): Deze functie (die dus de handle heeft als argument en op een andere thread draait) handelt de connectie (modify handleConnection)

STAPPENPLAN COMMAND C:
1. Als we C binnenkrijgen, maak dan de socket en handle aan, en fork FUNCTIE A. (zie boven)

--VERSCHILLENDE THREADS PER NODE:
1. Thread voor het luisteren naar inkomende connecties.
2. Thread voor elke socket: deze socket checkt ZELF (in een loop) voor inkomende berichten.
3. Main thread: luistert naar commands

EXTRA NOTES:
NIET in de main thread checkformessages doen.
In de socket thread checkmessages doen.

--}
  -- As an example, connect to the first neighbour. This just
  -- serves as an example on using the network functions in Haskell
  -- case neighbours of
  --   [] -> putStrLn "I have no neighbours :("
  --   neighbour : _ -> do
  --     putStrLn $ "Connecting to neighbour " ++ show neighbour ++ "..."
  --     client <- connectSocket neighbour --Maak een nieuwe Socket voor deze neighbour
  --     chandle <- socketToHandle client ReadWriteMode --Creeër een handle voor deze socket
  --     -- Send a message over the socket
  --     -- You can send and receive messages with a similar API as reading and writing to the console.
  --     -- Use `hPutStrLn chandle` instead of `putStrLn`,
  --     -- and `hGetLine  chandle` instead of `getLine`.
  --     -- You can close a connection with `hClose chandle`.
  --     hPutStrLn chandle $ "Hi process " ++ show neighbour ++ "! I'm process " ++ show me ++ " and you are my first neighbour."
  --     putStrLn "I sent a message to the neighbour"
  --     message <- hGetLine chandle
  --     putStrLn $ "Neighbour send a message back: " ++ show message
  --     hClose chandle

-- This function recursively goes over the neighbours list and
initialConnections :: Int -> [Int] -> IO()
initialConnections _ [] = putStrLn "//I have no more neighbours to connect with" 
initialConnections myport (x:rest) = if myport > x then 
  createHandle x 
else initialConnections myport rest

-- This function first creates a handle for a given portnumber, and then starts a new thread with that handle to handle that connection.
createHandle :: Int -> IO()
createHandle portnumber = do
    client <- connectSocket portnumber
    chandle <- socketToHandle client ReadWriteMode
    _ <- forkIO  $ initialBroadcast chandle
    putStrLn $ "//Connection made with: " ++ show portnumber
    
-- This function will let the new thread broadcast it's information to his neighbours
initialBroadcast :: Handle -> IO()
initialBroadcast handle = do
  --broadcast all je info hier
  connectionHandler handle

-- This handles a single connection
connectionHandler :: Handle -> IO() 
connectionHandler handle = do
  input <- hGetLine handle
  putStrLn $ show input
  connectionHandler handle

-- Loop check for incoming commands
commandCheck :: IO()
commandCheck = do
  raw <- getLine
  if raw == [] then commandCheck
  else do
    let (x:y:xs) = splitOn " " raw in
      if (x == "R") 
        then do 
          showRoutingTable
          commandCheck
        else if (x == "B") 
          then do
            --sendMessage (read y :: Int) (compileMessage xs)
            commandCheck
          else if (x == "C") 
            then do              
              createHandle (read y :: Int)
              commandCheck
            else if (x == "D") 
              then do
                commandCheck
              else do
                putStrLn "Give valid input"
                commandCheck

-- Compiling the list of words into a single message
compileMessage:: [String] -> String
compileMessage [] = ""
compileMessage (x:xs) = x ++ " " ++ compileMessage xs

-- Printing the routing table to the console
showRoutingTable:: IO()
showRoutingTable = putStrLn "Showing routing table"

-- Sending a message to a certain neighbour
sendMessage:: Handle -> String -> IO()
sendMessage handle message = hPutStrLn handle message

readCommandLineArguments :: IO (Int, [Int])
readCommandLineArguments = do
  args <- getArgs
  case args of
    [] -> error "Not enough arguments. You should pass the port number of the current process and a list of neighbours"
    (me:neighbours) -> return (read me, map read neighbours)

portToAddress :: Int -> SockAddr
portToAddress portnumber = SockAddrInet (fromIntegral portnumber) (tupleToHostAddress (127, 0, 0, 1)) -- localhost

--Maak een socket voor het gegeven portnummer.
connectSocket :: Int -> IO Socket
connectSocket portnumber = connect'
  where
    connect' = do
      client <- socket AF_INET Stream 0
      result <- try $ connect client $ portToAddress portnumber
      case result :: Either IOException () of
        Left _ -> do
          threadDelay 1000000
          connect'
        Right _ -> return client

--A fuction which listens for incoming connections (loops)
listenForConnections :: Socket -> IO ()
listenForConnections serverSocket = do
  (connection, _ ) <- accept serverSocket
  chandle <- socketToHandle connection ReadWriteMode
  forkIO $ initialBroadcast chandle
  putStrLn $ "Received connection with: " ++ show serverSocket
  listenForConnections serverSocket

--An MVar-lock to access the routing table
lock :: IO a -> MVar(Bool) -> IO a
lock myfunction mvar = do
  dummy <- takeMVar mvar
  result <- myfunction
  putMVar mvar dummy
  return result