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

data Path = Local | Undef | Int

type RoutingEntry = TVar (Int, Path)            -- Distance & Prefered Neighbour
type RoutingTable = HashMap Int RoutingEntry    -- Destination & Entry
type DistanceTable = HashMap Path Int           -- Node & Distance
type PreferedTable = HashMap Path Path          -- Destination port & Prefered Neighbour
type BuurDistanceTable = HashMap Int Path       -- Neigbour & Node

data RoutingInfo = RoutingInfo {
    allNodes:: [Int],
    neigbours:: [Int],
    routingTable:: RoutingTable,
    distanceTable:: DistanceTable,
    preferedTable:: PreferedTable, 
    buurDistanceTable:: BuurDistanceTable
}

-- lijst van buren (gegeven)
-- lijst van afstanden naar elke node in het netwerk
    -- broadcast je buren naar al je buren
-- lijst van voorkeurbuur voor elke node/bestemming
-- lijst van de geschatte afstand van een van jou buren naar elke andere node

init:: Int -> RoutingInfo
init me = 
    RoutingInfo {
        portnumber = me
        allNodes = neigbours info,
        neigbours = neigbours info,
        routingTable = initRoutingTable,
        distanceTable = initDistanceTable,
        preferedTable = initPreferedTable,
        buurDistanceTable = initBuurDistanceTable
    }


initRoutingTable:: RoutingTable
initRoutingTable = empty


initDistanceTable:: Int -> DistanceTable
initDistanceTable me = do
    distanceTable <- empty
    insert me 0 distanceTable

initPreferedTable:: Int -> PreferedTable
initPreferedTable me = do
    preferedTable <- empty
    insert me (Path Local) preferedTable

initBuurDistanceTable:: BuurDistanceTable
initBuurDistanceTable = empty