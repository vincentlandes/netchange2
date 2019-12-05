
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

  -- As an example, connect to the first neighbour. This just
  -- serves as an example on using the network functions in Haskell
  case neighbours of
    [] -> putStrLn "I have no neighbours :("
    neighbour : _ -> do
      putStrLn $ "Connecting to neighbour " ++ show neighbour ++ "..."
      client <- connectSocket neighbour --Maak een nieuwe Socket voor deze neighbour
      chandle <- socketToHandle client ReadWriteMode --Creeër een handle voor deze socket
      -- Send a message over the socket
      -- You can send and receive messages with a similar API as reading and writing to the console.
      -- Use `hPutStrLn chandle` instead of `putStrLn`,
      -- and `hGetLine  chandle` instead of `getLine`.
      -- You can close a connection with `hClose chandle`.
      hPutStrLn chandle $ "Hi process " ++ show neighbour ++ "! I'm process " ++ show me ++ " and you are my first neighbour."
      putStrLn "I sent a message to the neighbour"
      message <- hGetLine chandle
      putStrLn $ "Neighbour send a message back: " ++ show message
      hClose chandle

  --Create mvar for the mvar lock
  mvarlock <- newEmptyMVar
  putMVar mvarlock True 
  commandCheck

  threadDelay 1000000000

--Loop check for incoming commands
commandCheck :: IO()
commandCheck = do
  raw <- getLine
  if raw == [] then commandCheck
  else do
    let command = splitOn " " raw in
      if (command!!0 == "R") 
        then do 
          showRoutingTable
          commandCheck
        else if (command!!0 == "B") 
          then do 
            -- bug commad!!2 = first word message
            sendMessage (command!!1) (command!!2)
            commandCheck
          else if (command!!0 == "C") 
            then do 
              makeConnection (command!!1)
              commandCheck
            else if (command!!0 == "D") 
              then do 
                closeConnection (command!!1)
                commandCheck
              else do
                  putStrLn "Give valid input"
                  commandCheck

showRoutingTable:: IO()
showRoutingTable = putStrLn "Showing routing table"

sendMessage:: String -> String -> IO()
sendMessage portnumber message = putStrLn ("Sending message: " ++ message ++ " to " ++ portnumber)

makeConnection:: String -> IO()
makeConnection portnumber = putStrLn ("Starting connection with" ++ portnumber)

closeConnection:: String -> IO()
closeConnection portnumber = putStrLn ("Closing connection with" ++ portnumber)

readCommandLineArguments :: IO (Int, [Int])
readCommandLineArguments = do
  args <- getArgs
  case args of
    [] -> error "Not enough arguments. You should pass the port number of the current process and a list of neighbours"
    (me:neighbours) -> return (read me, map read neighbours)

portToAddress :: Int -> SockAddr
portToAddress portNumber = SockAddrInet (fromIntegral portNumber) (tupleToHostAddress (127, 0, 0, 1)) -- localhost

connectSocket :: Int -> IO Socket
connectSocket portNumber = connect'
  where
    connect' = do
      client <- socket AF_INET Stream 0
      result <- try $ connect client $ portToAddress portNumber
      case result :: Either IOException () of
        Left _ -> do
          threadDelay 1000000
          connect'
        Right _ -> return client

listenForConnections :: Socket -> IO ()
listenForConnections serverSocket = do
  (connection, _) <- accept serverSocket
  _ <- forkIO $ handleConnection connection
  listenForConnections serverSocket

handleConnection :: Socket -> IO ()
handleConnection connection = do
  putStrLn "Got new incomming connection"
  chandle <- socketToHandle connection ReadWriteMode
  hPutStrLn chandle "Welcome"
  message <- hGetLine chandle
  putStrLn $ "Incomming connection send a message: " ++ message
  hClose chandle

--An MVar-lock to access the routing table

lock :: IO a -> MVar(Bool) -> IO a
lock myfunction mvar = do
  dummy <- takeMVar mvar
  result <- myfunction
  putMVar mvar dummy
  return result



