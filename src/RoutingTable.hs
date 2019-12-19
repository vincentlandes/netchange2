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

type RoutingEntry = TVar (Int, Int)            -- Distance & Prefered Neighbour
type RoutingTable = HashMap Int RoutingEntry    -- Destination & Entry
type Table = HashMap Int Integer           -- Node & Distance
          -- Destination port & Prefered Neighbour
      -- Neigbour & Node

data RoutingInfo = RoutingInfo {
    portnumber:: Int,
    allNodes:: [Int],
    neigbours:: [Int],
    routingTable:: RoutingTable,
    distanceTable:: Table,
    preferedTable:: Table, 
    buurDistanceTable:: Table
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
        allNodes = neigbours,
        neigbours = neigbours,
        routingTable = empty,
        distanceTable = initDistanceTable me,
        preferedTable = initPreferedTable me,
        buurDistanceTable = empty
    }

initDistanceTable:: Int -> Table
initDistanceTable me = insert me 0 distanceTable
    where distanceTable = empty

initPreferedTable:: Int -> Table
initPreferedTable me = insert me (-1) preferedTable
    where preferedTable = empty