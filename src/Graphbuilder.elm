module Graphbuilder exposing (build, highlightPath)

import Graph exposing (Graph)
import Graph.Node as Node exposing (Node)
import Graph.Edge as Edge exposing (Edge)
import Engine exposing (..)


getAllInteractables : Engine.Model -> List String
getAllInteractables engineModel =
    -- TODO/FIX
    -- not completely true - the client can expose other interactables
    -- such as the "exits" component or hypermedia text
    -- Maybe need to ask the client for this list?
    Engine.getCharactersInCurrentLocation engineModel
        ++ Engine.getItemsInCurrentLocation engineModel
        ++ Engine.getItemsInInventory engineModel
        ++ Engine.getLocations engineModel


worldEq : Engine.Model -> Engine.Model -> Bool
worldEq old new =
    Engine.getItemsInCurrentLocation old
        == Engine.getItemsInCurrentLocation new
        && Engine.getCharactersInCurrentLocation old
        == Engine.getCharactersInCurrentLocation new
        && Engine.getItemsInInventory old
        == Engine.getItemsInInventory new
        && Engine.getLocations old
        == Engine.getLocations new
        && Engine.getCurrentLocation old
        == Engine.getCurrentLocation new
        && Engine.getCurrentScene old
        == Engine.getCurrentScene new
        && Engine.getEnding old
        == Engine.getEnding new


type alias ExploredPaths =
    { previousStates : List Engine.Model
    , paths : List (List Edge)
    , edges : List Edge
    , nodes : List Node
    }


{-|
  - at a given world state
    - for each possible interaction
      - see how the world changed (pushing all accumulated state forward, rather then combining with return from recusive call)
        - if the story ended, return the full path (nodes and edges)
        - if the world is the same, return nothing (or the full path, but mark as a dead end)
        - if the world changed, recur from top
-}
build : Bool -> Engine.Model -> Rules -> ( List (List Edge), Graph Node )
build showNonChangingRules startingEngineModel rules =
    let
        addIfUnique a list =
            if List.member a list then
                list
            else
                a :: list

        beenHereBefore currentWorldState previousStates =
            List.any (worldEq currentWorldState) previousStates

        findMatcingRule : List Edge -> Engine.Model -> String -> String -> ExploredPaths -> ExploredPaths
        findMatcingRule currentPath currentWorldState lastRule currentlyExploring acc =
            let
                ( nextWorldState, maybeMatchedRule ) =
                    Engine.update currentlyExploring currentWorldState

                edge a b =
                    Edge "#000000" a b

                node name =
                    Node.fromName [ name ]
            in
                case
                    ( maybeMatchedRule
                    , Engine.getEnding nextWorldState
                    , beenHereBefore nextWorldState acc.previousStates
                    )
                of
                    ( Just matchedRule, Just ending, _ ) ->
                        -- ending - path complete
                        { acc
                            | paths = addIfUnique (currentPath ++ [ (edge lastRule matchedRule) ]) acc.paths
                            , edges = addIfUnique (edge lastRule matchedRule) acc.edges
                            , nodes = addIfUnique (node matchedRule) acc.nodes
                        }

                    ( Just matchedRule, Nothing, True ) ->
                        -- non-changing rule, just "flavor"
                        if showNonChangingRules && (not <| List.member (Node.fromName [ matchedRule ]) acc.nodes) then
                            { acc
                                | edges = addIfUnique (edge lastRule matchedRule) acc.edges
                                , nodes = addIfUnique (node matchedRule) acc.nodes
                            }
                        else
                            acc

                    ( Just matchedRule, Nothing, _ ) ->
                        -- story continues - keep exploring
                        explore
                            (currentPath ++ [ (edge lastRule matchedRule) ])
                            nextWorldState
                            matchedRule
                            { acc
                                | previousStates = nextWorldState :: acc.previousStates
                                , edges = addIfUnique (edge lastRule matchedRule) acc.edges
                                , nodes = addIfUnique (node matchedRule) acc.nodes
                            }

                    -- only a matched rule can set an ending, so no need to check for endings here
                    ( Nothing, _, True ) ->
                        -- default rule creates a loop -- stop exploring
                        acc

                    ( Nothing, _, False ) ->
                        -- default rule (take item/go to location) - keep exploring, but don't add to graph
                        explore
                            currentPath
                            nextWorldState
                            lastRule
                            { acc | previousStates = nextWorldState :: acc.previousStates }

        explore : List Edge -> Engine.Model -> String -> ExploredPaths -> ExploredPaths
        explore currentPath currentWorldState lastRule acc =
            getAllInteractables currentWorldState
                |> List.foldl (findMatcingRule currentPath currentWorldState lastRule) acc

        { previousStates, paths, edges, nodes } =
            explore [] startingEngineModel "Begining" <| ExploredPaths [ startingEngineModel ] [] [] [ Node.fromName [ "Begining" ] ]
    in
        ( paths, Graph.init edges nodes )


highlightPath : List Edge -> Graph Node -> Graph Node
highlightPath path graph =
    let
        tracePath edge =
            if List.member edge path then
                { edge | color = "#FF0000" }
            else
                edge

        highlighedPath =
            List.map tracePath graph.edges
    in
        Graph.init highlighedPath graph.nodes
