{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Data.Maybe
import Data.Text.Lazy as T (pack)

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Data.GraphViz
import Data.GraphViz.Attributes.Complete

import Data.Tuple (swap)
import Data.List (nub)
import Control.Applicative

-- 1.

-- type Node = Int
-- type Edge = (Node, Node)

-- type LNode a = (Node, a)
-- type LEdge b = (Node, Node, b)

smallGrNodes :: [LNode String]
smallGrNodes = [(0, "A"), (1, "B"), (2, "C"), (3, "G")]

smallGrEdges :: [LEdge Int]
smallGrEdges = [(0, 1, 3), (1, 2, 4), (1, 3, 2), (2, 3, 5), (3, 2, 5)]

smallGraph :: Gr String Int
smallGraph = mkGraph smallGrNodes smallGrEdges

smallGrDot :: IO FilePath
smallGrDot = runGraphviz (graphToDot quickParams smallGraph) Pdf "graph.pdf"


-- 2. Write a function edgeListGraphToGr that translates an edge-list graph to a Gr graph representation.

data EdgeListGraph a =
  ELG [a] [(a, a)]
  deriving (Eq, Ord, Show, Read)

edgeListGraphToGr :: Eq a => EdgeListGraph a -> Gr a String
edgeListGraphToGr (ELG ns es) =
  mkGraph (edgeListGraphToGrNodes ns)
          (mapEdges ns es)

edgeListGraphToGrNodes :: [a] -> [LNode a]
edgeListGraphToGrNodes ns = zip [0..(length ns-1)] ns

grNodesSwapped :: [b] -> [(b, Node)]
grNodesSwapped = map swap . edgeListGraphToGrNodes

-- LEdge a = (Node, Node, a)
mapEdges :: Eq a => [a] -> [(a, a)] -> [LEdge String]
mapEdges ns =
  mapMaybe (\(l, r) ->
         let ml = lookup l (grNodesSwapped ns)
             mr = lookup r (grNodesSwapped ns)
             s  = ""
         in liftA2 (,,s) ml mr)

-- ? :: Maybe a -> Maybe a -> String -> Maybe (a, a, String)
-- (,,c) :: a -> b -> (a, b, c)
-- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
-- liftA2 :: (a -> b -> (a, b, c)) -> f a -> f b -> f (a, b, c)


-- with fmap (uncurry (,,s)) (toMaybeEdge (ml, mr)))
-- ? :: Maybe (Node, Node) -> String -> Maybe (Node, Node, String)
-- ? :: f (a, b) -> c -> f (a, b, c)

-- ? :: (a, b) -> (a, b, c)
-- uncurry (,,c) :: (a, b) -> (a, b, c)
-- fmap :: ((a, b) -> (a, b, c)) -> f (a, b) -> f (a, b, c)


-- with liftA2 (uncurry (,,)) (toMaybeEdge (ml, mr)) s
-- (,,) :: a -> b -> c -> (a, b, c)

-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- uncurry :: (a -> b -> (c -> (a, b, c)))
--         -> (a, b)
--         -> (c -> (a, b, c))

-- uncurry (,,) :: (a, b) -> c -> (a, b, c)

-- liftA2 :: ((a, b) -> c -> (a, b, c))
--        -> Maybe (a, b)
--        -> Maybe c
--        -> Maybe (a, b, c)

-- toMaybeEdge :: (Maybe a, Maybe a) -> Maybe (a, a)
-- toMaybeEdge (mn, mn') = liftA2 (,) mn mn'

smallELG :: EdgeListGraph String
smallELG =
  ELG ["A", "B", "C", "G"]
      [("A", "B"), ("B", "C"), ("B", "G"), ("C", "G"), ("G", "C")]

edgeListDot :: IO FilePath
edgeListDot =
  runGraphviz (graphToDot quickParams (edgeListGraphToGr smallELG))
              Pdf
              "smallELGGr.pdf"


-- 3. Write a function nextFunGraphToEdgeListGraph that translates a next-fun graph to an edge-list graph

data NextFunGraph a = NFG [a] (a -> [a])

-- node successor function
smallGrNexts :: String -> [String]
smallGrNexts x =
  case x of
    "A" -> ["B"]
    "B" -> ["C", "G"]
    "C" -> ["G"]
    "G" -> ["C"]
    _   -> []

smallGrNFG :: NextFunGraph String
smallGrNFG = NFG ["A", "B", "C", "G"] smallGrNexts

nextFunGraphToEdgeListGraph :: NextFunGraph a -> EdgeListGraph a
nextFunGraphToEdgeListGraph (NFG ns f) =
  ELG ns (edgeNextsToEdgeList f ns)

edgeNextsToEdgeList :: (a -> [a]) -> [a] -> [(a, a)]
-- edgeNextsToEdgeList f ns = [(fr, to) | fr <- ns, to <- f fr]
edgeNextsToEdgeList f ns = do
  fr   <- ns
  to <- f fr
  return (fr, to)

-- edgeNextsToEdgeList smallGrNexts ["A", "B", "C", "G"]
-- [("A","B"),("B","C"),("B","G"),("C","G"),("G","C")]

smallGraph' :: Gr String String
smallGraph' =
  edgeListGraphToGr
    (nextFunGraphToEdgeListGraph
      (NFG ["A", "B", "C", "G"] smallGrNexts))

smallGrDot' :: IO FilePath
smallGrDot' = runGraphviz (graphToDot quickParams smallGraph') Pdf "graph2.pdf"


-- 4.
breadthfirst :: (t -> [t]) -> (t -> Bool) -> t -> [t]
breadthfirst nexts sol x = bfs [x]
  where
    bfs []       = []
    bfs (nd:nds) =
      if sol nd
      then nd : bfs (nds ++ nexts nd)
      else bfs (nds ++ nexts nd)

-- λ> take 5 (breadthfirst smallGrNexts (== "G") "A")
-- ["G","G","G","G","G"]

depthfirst :: (t -> [t]) -> (t -> Bool) -> t -> [t]
depthfirst nexts sol x = dfs [x]
  where
    dfs []       = []
    dfs (nd:nds) =
      if sol nd
      then nd : dfs (nexts nd ++ nds)
      else dfs (nexts nd ++ nds)

-- a search path as a list of nodes
-- the start node of the path is the first of the list and the node reached by the path is the last
type SearchPath a = [a]

liftNextsToPath :: (a -> [a]) -> SearchPath a -> [SearchPath a]
-- liftNextsToPath f sp = [sp ++ [x] | x <- f (last sp)]
liftNextsToPath f sp = do
  next <- f (last sp)
  [sp ++ [next]]

-- liftNextsToPath smallGrNexts ["A", "B"]
-- [["A", "B", "C"], ["A", "B", "G"]]


-- 5.
liftNextsToPathNoDup :: Eq a
                     => (a -> [a]) -> SearchPath a -> [SearchPath a]
liftNextsToPathNoDup f sp = do
  next <- f (last sp)
  [sp ++ [next] | next `notElem` sp]

pathsFromToBFS :: Eq a => (a -> [a]) -> a -> a -> [SearchPath a]
pathsFromToBFS nexts start goal =
  breadthfirst
  (liftNextsToPathNoDup nexts)
  (\path -> last path == goal)
  [start]

pathsFromToDFS :: Eq a => (a -> [a]) -> a -> a -> [SearchPath a]
pathsFromToDFS nexts start goal =
  depthfirst
  (liftNextsToPathNoDup nexts)
  (\path -> last path == goal)
  [start]

-- λ> pathsFromToBFS smallGrNexts "A" "G"
-- [["A","B","G"],["A","B","C","G"]]

-- λ> pathsFromToDFS smallGrNexts "A" "G"
-- [["A","B","C","G"],["A","B","G"]]


-- River Crossing
data Side = Ls | Rs
  deriving (Eq, Ord, Show, Read)

type CfgwState = (Side, Side, Side, Side)


-- 6.
crossCabbage :: CfgwState -> CfgwState
crossCabbage (Ls, f, g, w) = (Rs, f, g, w)
crossCabbage (Rs, f, g, w) = (Ls, f, g, w)

crossFerryman :: CfgwState -> CfgwState
crossFerryman (c, Ls, g, w) = (c, Rs, g, w)
crossFerryman (c, Rs, g, w) = (c, Ls, g, w)

crossGoat :: CfgwState -> CfgwState
crossGoat (c, f, Ls, w) = (c, f, Rs, w)
crossGoat (c, f, Rs, w) = (c, f, Ls, w)

crossWolf :: CfgwState -> CfgwState
crossWolf (c, f, g, Ls) = (c, f, g, Rs)
crossWolf (c, f, g, Rs) = (c, f, g, Ls)


-- 7.
-- verify that a state satisfies the constraints of
-- simultaneous presence of
-- cabbage & goat or goat & wolf on either side
validState :: CfgwState -> Bool

-- C and G can only be on the same side if F is also on that side
validState (Ls, Rs, Ls, _)  = False
validState (Ls, Ls, Ls, _)  = True
validState (Rs, Ls, Rs, _)  = False
validState (Rs, Rs, Rs, _)  = True

-- G and W can only be on the same side if F is also on that side
validState (_, Rs, Ls, Ls) = False
validState (_, Ls, Ls, Ls) = True
validState (_, Ls, Rs, Rs) = False
validState (_, Rs, Rs, Rs) = True

validState (Ls, _, Rs, _) = True
validState (Rs, _, Ls, _) = True


-- 8.
crossBoat :: CfgwState -> [CfgwState]
crossBoat s =
  filter validState $
  [ crossFerryman
  , (crossCabbage . crossFerryman)
  , (crossGoat . crossFerryman)
  , (crossWolf . crossFerryman) ] <*> [s]


-- 9.
crossBoatBFS :: [SearchPath CfgwState]
crossBoatBFS =
  pathsFromToBFS crossBoat (Ls, Ls, Ls, Ls) (Rs, Rs, Rs, Rs)

crossBoatDFS :: [SearchPath CfgwState]
crossBoatDFS =
  pathsFromToDFS crossBoat (Ls, Ls, Ls, Ls) (Rs, Rs, Rs, Rs)

-- obtain node set of search graph
cfgwBFSNodes :: [CfgwState]
cfgwBFSNodes = (nub . concat) crossBoatBFS

-- define a graph in next-fun representation
cfgwBFSNFG :: NextFunGraph CfgwState
cfgwBFSNFG = NFG cfgwBFSNodes crossBoat

-- convert it to edge-list
-- and then to Gr graph representation
cfgwBFSGraph :: Gr CfgwState String
cfgwBFSGraph =
  edgeListGraphToGr (nextFunGraphToEdgeListGraph cfgwBFSNFG)

cfgwBFSDot :: IO FilePath
cfgwBFSDot = runGraphviz (graphToDot quickParams cfgwBFSGraph) Pdf "cfgwBFS.pdf"


-- 10.
cfgwNodes :: [CfgwState]
cfgwNodes =
  filter validState $
  (,,,) <$> [Ls, Rs] <*> [Ls, Rs] <*> [Ls, Rs] <*> [Ls, Rs]

cfgwNFG :: NextFunGraph CfgwState
cfgwNFG = NFG cfgwNodes crossBoat

cfgwGraph :: Gr CfgwState String
cfgwGraph =
  edgeListGraphToGr (nextFunGraphToEdgeListGraph cfgwNFG)

cfgwDot :: IO FilePath
cfgwDot = runGraphviz (graphToDot quickParams cfgwGraph) Pdf "cfgw.pdf"

instance Labellable CfgwState where
  toLabelValue s = StrLabel (T.pack (show s))
