
module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.MVar
import Control.Exception
import Data.IORef
import System.Environment
import System.IO
import Network.Socket
import Data.List.Split
import Data.HashMap.Lazy as Hash
import RoutingTable as RT

type HandleTable = HashMap Int Handle

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
  
  -- Create HandleTable for connections and routingTable
  connections <- newTVarIO empty

  -- Start the initialisation of the routing information
  routingInfo <- newTVarIO (RT.init me neighbours)
  
  -- Let a seperate thread listen for incomming connections
  _ <- forkIO $ listenForConnections me serverSocket routingInfo

  -- Start creating connections with every neighbour
  initialConnections me neighbours connections routingInfo
  
  -- The main thread checks for commands
  commandCheck me connections routingInfo

-- This function recursively goes over the neighbours list and checks if portnr greater or smaller.
initialConnections :: Int -> [Int] -> (TVar HandleTable) -> (TVar RoutingInfo) -> IO()
initialConnections _ [] _ _ = putStrLn "//I have no more neighbours to connect with" 
initialConnections me (x:rest) connections routingInfo = if me > x then 
  createHandle me x connections routingInfo
else initialConnections me rest connections routingInfo

-- This function first creates a handle for a given portnumber, and then starts a new thread with that handle to handle that connection.
createHandle :: Int -> Int -> (TVar HandleTable) -> (TVar RoutingInfo) -> IO()
createHandle me portnumber connections routingInfo = do
    client <- connectSocket portnumber
    chandle <- socketToHandle client ReadWriteMode
    atomically (do
      _connections <- readTVar connections
      writeTVar connections (insert portnumber chandle _connections)) 
    _ <- forkIO $ initialize me chandle connections routingInfo
    putStrLn $ "Connected: " ++ show portnumber
    
-- This function will let the new thread broadcast it's information to his neighbours
initialize :: Int -> Handle -> (TVar HandleTable) -> (TVar RoutingInfo) -> IO()
initialize me handle routingInfo connections = do
  hPutStrLn handle ("mydist " ++ (show me) ++ " 0")
  connectionHandler me handle routingInfo connections

-- This handles a single connection
connectionHandler :: Int -> Handle -> (TVar HandleTable) -> (TVar RoutingInfo) -> IO() 
connectionHandler me handle connections routingInfo = do
  input <- hGetLine handle
  if (input == []) then connectionHandler me handle connections routingInfo
  else do 
    let (command:portnumber:xs) = splitOn " " input in
      if (command == "mydist")
        then do
          putStrLn $ compileMessage xs
      else if (command == "B")
        then do
          if (portnumber == (show me)) then do
            putStrLn $ compileMessage xs
          else do
            let destination = portnumber
            let neighbour = destination -- getNeighbour
            putStrLn "Message for " ++ (show destination) ++ " is relayed to " ++ (show neighbour)
            sendMessage connections neighbour destination (compileMessage xs)
            putStrLn $ compileMessage xs
      else do
        putStrLn "//Unexpected input"
        connectionHandler me handle connections routingInfo
  connectionHandler me handle connections routingInfo

-- Loop check for incoming commands
commandCheck :: Int -> (TVar HandleTable) -> (TVar RoutingInfo) -> IO()
commandCheck me connections routingInfo = do
  raw <- getLine
  if raw == [] then commandCheck me connections routingInfo
  else do
    let (command:portnumber:xs) = splitOn " " raw in
      if (command == "R") 
        then do 
          showRoutingTable
          commandCheck me connections routingInfo
        else if (command == "B") 
          then do
            let destination = (read portnumber :: Int)
            let neighbour = destination
            sendMessage connections neighbour destination (compileMessage xs)
            commandCheck me connections routingInfo
          else if (command == "C") 
            then do              
              createHandle me (read portnumber :: Int) connections routingInfo
              commandCheck me connections routingInfo
            else if (command == "D") 
              then do
                disconnect portnumber connections
                commandCheck me connections routingInfo
              else do
                putStrLn "Give valid input"
                commandCheck me connections routingInfo

-- Compiling the list of words into a single message
compileMessage:: [String] -> String
compileMessage [] = ""
compileMessage (x:xs) = x ++ " " ++ compileMessage xs

-- Printing the routing table to the console
showRoutingTable:: IO()
showRoutingTable = putStrLn "Showing routing table"

-- Disconnect from a given port
disconnect:: (TVar HandleTable) -> Int -> IO ()
disconnect connections portnumber = do
  _connections <- readTVar connections
  if (member portnumber _connections) then
    delete portnumber _connections
    putStrLn "Disconnect: " ++ (show portnumber)
  else putStrLn "//Given portnumber is not connected to you"

-- A function in the STM environment to get a certain handle if it's available in the HandleTable
getHandle :: TVar (HandleTable) -> Int -> STM (Maybe Handle)
getHandle connections portnumber = do
  _connections <- readTVar connections
  if (member portnumber _connections) then
    return $ Just (_connections ! portnumber)
  else return Nothing

-- Sending a message to a certain neighbour
sendMessage:: (TVar HandleTable) -> Int -> Int -> String -> IO()
sendMessage connections portnumber destination message = do
  handle  <- atomically (getHandle connections portnumber)
  case handle of
    Just handle -> hPutStrLn handle ("B " ++ (show destination) ++ " " ++ message)
    Nothing -> putStrLn "Port " ++ (show portnumber) ++ " is not known"

readCommandLineArguments :: IO (Int, [Int])
readCommandLineArguments = do
  args <- getArgs
  case args of
    [] -> error "Not enough arguments. You should pass the port number of the current process and a list of neighbours"
    (me:neighbours) -> return (read me, Prelude.map read neighbours)

portToAddress :: Int -> SockAddr
portToAddress portnumber = SockAddrInet (fromIntegral portnumber) (tupleToHostAddress (127, 0, 0, 1)) -- localhost

--Make a socket given a portnumber
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
listenForConnections :: Int -> Socket -> (TVar RoutingInfo) -> IO ()
listenForConnections me serverSocket routingInfo = do
  (connection, _ ) <- accept serverSocket
  chandle <- socketToHandle connection ReadWriteMode
  initialize me chandle routingInfo
  putStrLn $ "Received connection with: " ++ show serverSocket
  listenForConnections me serverSocket routingInfo

{-- --ROUTING TABLE BIJHOUDEN--
1. Initializeren (dit doet elke node in het netwerk): 
  a. Routing table entry: destination (est. distance, via welke buur)
  b. Voor elke buur (w) die je hebt EN elke node in het netwerk (v): zet ndis[w,v] = N. v(N,w)
  c. Stel een entry in voor jezelf (Du[u]=0) namelijk: 'eigenport (0,eigenport)'
  d. Nbu[u] = local (preferred neighbour)
  e. send mydis(eigenport,0) aan al je neighbours (in initial broadcast)
2. daarna moeten we in handleConnection "mydis" connecties opvangen. (krijgen we van alle buren).
  a. we krijgen een mydis bericht van onze buur (w): mydis(v,d)
  b. pas de routingtable entry aan: v (d,w) (we kunnen v in d stappen berijken via buur w)
  c. recompute routing table van deze node.
3. tot slot de recompute functie.
  a. geef de destination port wiens entry je wil aanpassen mee als argument.
  b. als dit onze eigen port is, dan zet je eigenport(0,eigenport)
  c. anders wordt de entry: destination(d,buur)

--}