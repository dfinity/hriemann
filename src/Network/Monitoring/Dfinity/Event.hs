module Network.Monitoring.Dfinity.Event (sendBlock, recvBlock, blockLatency, livenessEvent) where

import           Data.Monoid                             (Endo, (<>))
import qualified Network.Monitoring.Riemann.Event        as RE
import           Network.Monitoring.Riemann.Event.Monoid

-- Timestamp and Duration are both in milliseconds
-- type Timestamp = Int
type Duration = Int
type Height = Int
type Node = Int
-- type Size = Int
type Rank = Int
-- type Percentage = Int
type Kind = String

livenessEvent :: Event
livenessEvent = RE.ok "liveness"

sendBlock :: Node -> Height -> Rank -> Event
sendBlock node height rank = let kind = "send-block" in
  ok "send-block" $ nhrk node height rank kind

recvBlock :: Node -> Height -> Rank -> Duration -> Event
recvBlock node height rank dt = let kind = "recv-block" in
  ok "recv-block" $ metric dt <> nhrk node height rank kind

blockLatency :: Node -> Height -> Duration -> Event
blockLatency node height dt = let kind = "block-latency" in
  ok "block-latency" $ metric dt <> nhk node height kind

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
