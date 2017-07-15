port module Main exposing (..)

import Engine exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (..)
import Manifest exposing (..)
import Rules exposing (rulesData)
import ClientTypes exposing (..)
import Graph.GraphViz as GraphViz
import Graph exposing (Graph)
import Graph.Node as Node exposing (Node)
import Graph.Edge as Edge exposing (Edge)


type alias Model =
    { engineModel : Engine.Model
    , graph : Graph Node
    , loading : Bool
    }


type Msg
    = Interact String
    | Loaded


type Tree a
    = Tree a (List (Tree a))


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscribe
        }


init : ( Model, Cmd Msg )
init =
    let
        rules =
            pluckRules

        engineModel =
            Engine.init
                { items = getIds items
                , locations = getIds locations
                , characters = getIds characters
                }
                rules
                |> Engine.changeWorld startingState

        startingState =
            [ moveTo "Cottage"
            , loadScene "start"
            , addLocation "Cottage"
            , addLocation "River"
            , addLocation "Woods"
            , addLocation "Grandma's house"
            , moveItemToLocation "Cape" "Cottage"
            , moveItemToLocation "Basket of food" "Cottage"
            , moveCharacterToLocation "Little Red Riding Hood" "Cottage"
            , moveCharacterToLocation "Mother" "Cottage"
            , moveCharacterToLocation "Wolf" "Woods"
            , moveCharacterToLocation "Grandma" "Grandma's house"
            ]

        graph =
            buildGraph engineModel rules
    in
        ( { engineModel = engineModel
          , graph = graph
          , loading = True
          }
        , drawGraph <| GraphViz.string graph
        )


port drawGraph : String -> Cmd msg


port loaded : (Bool -> msg) -> Sub msg


subscribe : Model -> Sub Msg
subscribe model =
    loaded (always Loaded)


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


{-|
  - at a given world state
    - for each possible interaction
      - see how the world changed (pushing all accumulated state forward, rather then combining with return from recusive call)
        - if the story ended, return the full path (nodes and edges)
        - if the world is the same, return nothing (or the full path, but mark as a dead end)
        - if the world changed, recur from top
-}
buildGraph : Engine.Model -> Rules -> Graph Node
buildGraph startingEngineModel rules =
    let
        addIfUnique a list =
            if List.member a list then
                list
            else
                a :: list

        beenHereBefore currentWorldState previousStates =
            List.any (worldEq currentWorldState) previousStates

        findMatcingRule : Int -> Engine.Model -> String -> String -> ExploredPaths -> ExploredPaths
        findMatcingRule depth currentWorldState lastRule currentlyExploring acc =
            let
                ( nextWorldState, maybeMatchedRule ) =
                    Engine.update currentlyExploring currentWorldState
            in
                case maybeMatchedRule of
                    Just matchedRule ->
                        if Engine.getEnding nextWorldState /= Nothing then
                            let
                                x =
                                    Debug.log "* reahed an ending" <| toString depth ++ " - " ++ currentlyExploring
                            in
                                -- only a matched rule can set an ending
                                { acc
                                    | previousStates = nextWorldState :: acc.previousStates
                                    , edges = addIfUnique (Edge lastRule matchedRule) acc.edges
                                    , nodes = addIfUnique (Node.fromName [ matchedRule ]) acc.nodes
                                }
                        else if beenHereBefore nextWorldState (currentWorldState :: acc.previousStates) then
                            let
                                x =
                                    Debug.log "reached a rule with no changes or a loop" <| toString depth ++ " - " ++ currentlyExploring
                            in
                                -- just "flavor" (not enough room in graph to show)
                                -- or dead end! (or loop?)
                                acc
                        else
                            let
                                x =
                                    Debug.log "* continuing to explore" <| toString depth ++ " - " ++ currentlyExploring
                            in
                                -- rule with changes
                                explore (depth + 1)
                                    nextWorldState
                                    matchedRule
                                    { acc
                                        | previousStates = nextWorldState :: acc.previousStates
                                        , edges = addIfUnique (Edge lastRule matchedRule) acc.edges
                                        , nodes = addIfUnique (Node.fromName [ matchedRule ]) acc.nodes
                                    }

                    Nothing ->
                        let
                            x =
                                Debug.log "reach a loop from a default rule" <| toString depth ++ " - " ++ currentlyExploring
                        in
                            if beenHereBefore nextWorldState (currentWorldState :: acc.previousStates) then
                                -- loop
                                acc
                            else
                                let
                                    x =
                                        Debug.log "* continuing to explore (from default rule)" <| toString depth ++ " - " ++ currentlyExploring
                                in
                                    -- default rule
                                    ("default (take / go) " ++ currentlyExploring)
                                        |> \matchedRule ->
                                            explore (depth + 1)
                                                nextWorldState
                                                matchedRule
                                                { acc
                                                    | previousStates = nextWorldState :: acc.previousStates
                                                    , edges = addIfUnique (Edge lastRule matchedRule) acc.edges
                                                    , nodes = addIfUnique (Node.fromName [ matchedRule ]) acc.nodes
                                                }

        explore : Int -> Engine.Model -> String -> ExploredPaths -> ExploredPaths
        explore depth currentWorldState lastRule acc =
            getAllInteractables currentWorldState
                |> List.foldl (findMatcingRule depth currentWorldState lastRule) acc

        { previousStates, edges, nodes } =
            explore 1 startingEngineModel "Begining" <| ExploredPaths [ startingEngineModel ] [] [ Node.fromName [ "Begining" ] ]
    in
        Graph.init edges nodes


type alias ExploredPaths =
    { previousStates : List Engine.Model
    , edges : List Edge
    , nodes : List Node
    }



-- buildTree : Engine.Model -> Rules -> Tree String
-- buildTree startingEngineModel rules =
--     let
--         findMatcingRule engineModel interactable =
--             case Tuple.mapFirst (worldChanged engineModel) <| Engine.update interactable engineModel of
--                 ( Just newEngineModel, Just ruleName ) ->
--                     Just <|
--                         if Engine.getEnding newEngineModel == Nothing then
--                             Tree (interactable ++ " -> " ++ ruleName) <|
--                                 generateAllPaths newEngineModel
--                         else
--                             Tree (interactable ++ " -> " ++ ruleName ++ " *Ending*") []
--                 ( Just newEngineModel, Nothing ) ->
--                     Just <|
--                         if Engine.getEnding newEngineModel == Nothing then
--                             Tree (interactable ++ " -> default (take / go)") <|
--                                 generateAllPaths newEngineModel
--                         else
--                             Tree (interactable ++ " -> default (take / go) *Ending*") []
--                 ( Nothing, Just ruleName ) ->
--                     Just <| Tree (interactable ++ " -> " ++ ruleName) []
--                 ( Nothing, Nothing ) ->
--                     Nothing
--         generateAllPaths engineModel =
--             getAllInteractables engineModel
--                 |> List.filterMap (findMatcingRule engineModel)
--     in
--         Tree "*Start*" <| generateAllPaths startingEngineModel


getIds : List Entity -> List String
getIds =
    List.map .id


pluckRules : Engine.Rules
pluckRules =
    let
        foldFn :
            RuleData Engine.Rule
            -> Dict String Engine.Rule
            -> Dict String Engine.Rule
        foldFn { summary, interaction, conditions, changes } rules =
            Dict.insert (summary)
                { interaction = interaction
                , conditions = conditions
                , changes = changes
                }
                rules
    in
        List.foldl foldFn Dict.empty rulesData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loaded ->
            { model | loading = False } ! []

        _ ->
            model ! []


view : Model -> Html Msg
view model =
    div [ style [ ( "textAlign", "center" ) ] ]
        [ div [ id "graph" ] []
        , div []
            (if model.loading then
                [ text "Loading..." ]
             else
                []
            )
        ]


treeView : Tree String -> Html Msg
treeView tree =
    let
        olStyle =
            style
                [ ( "borderLeft", "1px solid lightgray" )
                , ( "margin", "5px 25px" )
                , ( "padding", "5px 25px" )
                ]
    in
        case tree of
            Tree a [] ->
                li [] [ text a ]

            Tree a tree ->
                li []
                    [ text a
                    , ol [ olStyle ] <|
                        List.map treeView tree
                    ]
