
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
  
  -- Let a seperate thread listen for incomming connections
  _ <- forkIO $ listenForConnections me serverSocket

  -- Create HandleTable for connections and routingTable
  connections <- newTVarIO empty

  -- Start creating connections with every neighbour
  initialConnections me neighbours connections

  -- Start the initialisation of the routing information  
  let routingInfo = RT.init me neighbours
  
  -- The main thread checks for commands
  commandCheck me connections

-- This function recursively goes over the neighbours list and checks if portnr greater or smaller.
initialConnections :: Int -> [Int] -> (TVar HandleTable) -> IO()
initialConnections _ [] _ = putStrLn "//I have no more neighbours to connect with" 
initialConnections me (x:rest) connections = if me > x then 
  createHandle me x connections
else initialConnections me rest connections

-- This function first creates a handle for a given portnumber, and then starts a new thread with that handle to handle that connection.
createHandle :: Int -> Int -> (TVar HandleTable) -> IO()
createHandle me portnumber connections = do
    client <- connectSocket portnumber
    chandle <- socketToHandle client ReadWriteMode
    atomically (do
      _connections <- readTVar connections
      writeTVar connections (insert portnumber chandle _connections)) 
    initialBroadcast me chandle
    putStrLn $ "//Connection made with: " ++ show portnumber
    
-- This function will let the new thread broadcast it's information to his neighbours
initialBroadcast :: Int -> Handle -> IO()
initialBroadcast me handle = do
  hPutStrLn handle ("mydist " ++ (show me) ++ " 0")
  connectionHandler handle  

-- This handles a single connection
connectionHandler :: Handle -> IO() 
connectionHandler handle = do
  input <- hGetLine handle
  putStrLn $ show input
  connectionHandler handle

-- Loop check for incoming commands
commandCheck :: Int -> (TVar HandleTable) -> IO()
commandCheck me connections = do
  raw <- getLine
  if raw == [] then commandCheck me connections
  else do
    let (command:portnumber:xs) = splitOn " " raw in
      if (command == "R") 
        then do 
          showRoutingTable
          commandCheck me connections
        else if (command == "B") 
          then do
            sendMessage connections (read portnumber :: Int) (compileMessage xs)
            commandCheck me connections
          else if (command == "C") 
            then do              
              createHandle me (read portnumber :: Int) connections
              commandCheck me connections
            else if (command == "D") 
              then do
                commandCheck me connections
              else do
                putStrLn "Give valid input"
                commandCheck me connections

-- Compiling the list of words into a single message
compileMessage:: [String] -> String
compileMessage [] = ""
compileMessage (x:xs) = x ++ " " ++ compileMessage xs

-- Printing the routing table to the console
showRoutingTable:: IO()
showRoutingTable = putStrLn "Showing routing table"

-- A function in the STM environment to get a certain handle if it's available in the HandleTable
getHandle :: TVar (HandleTable) -> Int -> STM (Maybe Handle)
getHandle connections portnumber = do
  _connections <- readTVar connections
  if (member portnumber _connections) then
    return $ Just (_connections ! portnumber)
  else return Nothing

-- Sending a message to a certain neighbour
sendMessage:: (TVar HandleTable) -> Int -> String -> IO()
sendMessage connections portnumber message = do
  handle  <- atomically (getHandle connections portnumber)
  case handle of
    Just x -> hPutStrLn x message
    Nothing -> putStrLn "ERROR: Portnumber is not known"

readCommandLineArguments :: IO (Int, [Int])
readCommandLineArguments = do
  args <- getArgs
  case args of
    [] -> error "Not enough arguments. You should pass the port number of the current process and a list of neighbours"
    (me:neighbours) -> return (read me, Prelude.map read neighbours)

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
listenForConnections :: Int -> Socket -> IO ()
listenForConnections me serverSocket = do
  (connection, _ ) <- accept serverSocket
  chandle <- socketToHandle connection ReadWriteMode
  initialBroadcast me chandle
  putStrLn $ "Received connection with: " ++ show serverSocket
  listenForConnections me serverSocket

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