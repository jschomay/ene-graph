module Graphbuilder exposing (build, highlightPath, toGraphViz)

import Graph exposing (..)
import Engine exposing (..)
import Dict


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
    , paths : List (List (Edge String))
    , edges : List (Edge String)
    , nodes : List (Node ( String, Bool ))
    }


{-|
  - at a given world state
    - for each possible interaction
      - see how the world changed (pushing all accumulated state forward, rather then combining with return from recusive call)
        - if the story ended, return the full path (nodes and edges)
        - if the world is the same, return nothing (or the full path, but mark as a dead end)
        - if the world changed, recur from top
-}
build : Engine.Model -> Rules -> ( List (List (Edge String)), Graph ( String, Bool ) String )
build startingEngineModel rules =
    let
        rulesMap =
            Dict.keys rules
                |> List.indexedMap (flip (,))
                |> Dict.fromList

        getRuleId ruleName =
            Dict.get ruleName rulesMap
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault -1

        addIfUnique a list =
            if List.member a list then
                list
            else
                a :: list

        beenHereBefore currentWorldState previousStates =
            List.any (worldEq currentWorldState) previousStates

        edge from to =
            Edge from.id (getRuleId to) "white"

        node ruleName end =
            Node (getRuleId ruleName) ( ruleName, end )

        findMatcingRule : List (Edge String) -> Engine.Model -> Node ( String, Bool ) -> String -> ExploredPaths -> ExploredPaths
        findMatcingRule currentPath currentWorldState lastRule currentlyExploring acc =
            let
                ( nextWorldState, maybeMatchedRule ) =
                    Engine.update currentlyExploring currentWorldState
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
                            | paths = addIfUnique (currentPath ++ [ edge lastRule matchedRule ]) acc.paths
                            , edges = addIfUnique (edge lastRule matchedRule) acc.edges
                            , nodes = addIfUnique (node matchedRule True) acc.nodes
                        }

                    ( Just matchedRule, Nothing, True ) ->
                        -- non-changing rule, just "flavor"
                        acc

                    ( Just matchedRule, Nothing, False ) ->
                        -- story continues - keep exploring
                        explore
                            (currentPath ++ [ edge lastRule matchedRule ])
                            nextWorldState
                            (node matchedRule False)
                            { acc
                                | previousStates = nextWorldState :: acc.previousStates
                                , edges = addIfUnique (edge lastRule matchedRule) acc.edges
                                , nodes = addIfUnique (node matchedRule False) acc.nodes
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
                            { acc
                                | previousStates = nextWorldState :: acc.previousStates
                            }

        explore : List (Edge String) -> Engine.Model -> Node ( String, Bool ) -> ExploredPaths -> ExploredPaths
        explore currentPath currentWorldState lastRule acc =
            getAllInteractables currentWorldState
                |> List.foldl (findMatcingRule currentPath currentWorldState lastRule) acc

        { previousStates, paths, edges, nodes } =
            explore [] startingEngineModel (Node 0 ( "Begin", False )) <| ExploredPaths [ startingEngineModel ] [] [] [ Node 0 ( "Begin", False ) ]
    in
        ( paths, Graph.fromNodesAndEdges nodes edges )


highlightPath : String -> List (Edge String) -> Graph ( String, Bool ) String -> Graph ( String, Bool ) String
highlightPath color path graph =
    let
        tracePath edge =
            if List.member edge path then
                { edge | label = color }
            else
                edge

        highlighedPath =
            List.map tracePath <| edges graph
    in
        Graph.fromNodesAndEdges (nodes graph) highlighedPath


toGraphViz : Graph ( String, Bool ) String -> String
toGraphViz graph =
    let
        edgesString =
            List.map edge (edges graph)
                |> String.join "\n"

        nodesString =
            List.map node (nodes graph)
                |> String.join "\n"

        edgeStyle : Edge String -> String
        edgeStyle edge =
            "penwidth=3"

        edge ({ from, to, label } as edge) =
            Basics.toString from
                ++ " -> "
                ++ Basics.toString to
                ++ " [color=\""
                ++ label
                ++ "\" "
                ++ edgeStyle edge
                ++ "];"

        nodeStyle : Node ( String, Bool ) -> String
        nodeStyle node =
            let
                color =
                    if Tuple.second node.label then
                        "yellow"
                    else
                        "white"
            in
                "fillcolor=" ++ color ++ " style=\"filled,rounded\" fontname=arial"

        node : Node ( String, Bool ) -> String
        node node =
            Basics.toString node.id
                ++ " [shape=box "
                ++ nodeStyle node
                ++ " label=\""
                ++ Tuple.first node.label
                ++ "\"];"
    in
        "digraph Paths { graph [bgcolor=black] rankdir=TB\n" ++ nodesString ++ edgesString ++ "}"
