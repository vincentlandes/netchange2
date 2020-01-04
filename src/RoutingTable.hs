module RoutingTable where

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
import Data.Hashable

--portnumber => -2 = undef, -1 = local, >0 = portnumber

type RoutingEntry = (Int, Int)                 -- Distance & Prefered Neighbour
type RoutingTable = HashMap Int RoutingEntry   -- Destination & Entry
type Table = HashMap Int Integer               -- Node & Distance

data RoutingInfo = RoutingInfo {
    portnumber:: Int,               -- my portnumber
    allNodes:: [Int],               -- list of all nodes
    neigbourList:: [Int],              -- list of current neighbours (Neighu)
    routingTable:: RoutingTable,    -- 
    distanceTable:: Table,          -- Du
    preferedTable:: Table,          -- Nbu 
    buurDistanceTable:: Table       -- ndisu
}

-- lijst van buren (gegeven)
-- lijst van afstanden naar elke node in het netwerk
    -- broadcast je buren naar al je buren
-- lijst van voorkeurbuur voor elke node/bestemming
-- lijst van de geschatte afstand van een van jou buren naar elke andere node

init:: Int -> [Int] -> RoutingInfo
init me neigbours =     
    RoutingInfo {
        portnumber = me,
        allNodes = nodes,
        neigbourList = neigbours,
        routingTable = empty,
        distanceTable = initDistanceTable me, 
        preferedTable = initPreferedTable me, 
        buurDistanceTable = empty
    }
    where nodes = me : neigbours
-- initBuurDistanceTable:: Table
-- initBuurDistanceTable = insert

initDistanceTable:: Int -> Table
initDistanceTable me = insert me 0 distanceTable
    where distanceTable = empty

initPreferedTable:: Int -> Table
initPreferedTable me = insert me (-1) preferedTable
    where preferedTable = empty

getNeigbour:: (TVar RoutingInfo) -> Int -> Int
getNeigbour routingTable destination = 0 -- function that looks into the preferedTable and give back the neighbour to forward the message to

-- recompute:: Int -> (TVar RoutingInfo) -> (TVar RoutingInfo)
-- recompute node routingInfo = do 
--     routingInfoExtracted <- atomically (readRoutingTable routingInfo)
--     if ((portnumber routingInfoExtracted) == node) then do
--         insert portnumber 0 (distanceTable routingInfoExtracted)
--         writeTVar routingInfo routingInfoExtracted
--     else return routingInfo

-- readRoutingTable :: (TVar RoutingInfo) -> STM (RoutingInfo)
-- readRoutingTable routingInfo = do
--     routingInfo_ <- readTVar routingInfo
--     return routingInfo_
