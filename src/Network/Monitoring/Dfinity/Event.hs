module Network.Monitoring.Dfinity.Event (sendBlock, recvBlock, blockLatency) where

import Data.Monoid (Endo, (<>))
import Network.Monitoring.Riemann.Event.Monoid

-- Timestamp and Duration are both in milliseconds
type Timestamp = Int
type Duration = Int
type Height = Int
type Node = Int
type Size = Int
type Rank = Int
type Percentage = Int
type Kind = String

toService :: Kind -> Height -> String
toService kind height = kind ++ "-" ++ show height

sendBlock :: Node -> Timestamp -> Height -> Rank -> Event
sendBlock node ts height rank = let kind = "send-block" in
  ok (toService kind height) $ timestamp (fromIntegral ts) <> nhrk node height rank kind 

recvBlock :: Node -> Timestamp -> Height -> Rank -> Duration -> Event
recvBlock node ts height rank dt = let kind = "recv-block" in
  ok (toService kind height) $ timestamp (fromIntegral ts) <> metric dt <> nhrk node height rank kind 

blockLatency :: Node -> Height -> Duration -> Event
blockLatency node height dt = let kind = "block-latency" in
  ok (toService kind height) $ metric dt <> nhk node height kind 

nhk :: Node -> Height ->  Kind -> Endo Event
nhk node height kind = 
  attributes [ attribute "node" $ Just (show node)
             , attribute "height" $ Just (show height)
             , attribute "kind" $ Just kind
             ]

nhrk :: Node -> Height -> Rank -> Kind -> Endo Event
nhrk node height rank kind = 
  attributes [ attribute "node" $ Just (show node)
             , attribute "height" $ Just (show height)
             , attribute "rank" $ Just (show rank)
             , attribute "kind" $ Just kind
             ]
