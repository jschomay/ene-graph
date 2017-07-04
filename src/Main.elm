module Main exposing (..)

import Engine exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (..)
import Tuple exposing (..)
import Manifest exposing (..)
import Rules exposing (rulesData)
import ClientTypes exposing (..)
import Graph exposing (Graph)
import Graph.GraphViz as GraphViz
import Graph.Node as Node exposing (Node)
import Graph.Edge as Edge exposing (Edge)


type alias Model =
    { engineModel : Engine.Model
    , tree : Tree
    , graph : Graph Node
    }


type Tree
    = Tree String (List Tree)


type Msg
    = Interact String


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
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
    in
        ( { engineModel = engineModel
          , tree = buildTree engineModel rules
          , graph = buildGraph engineModel rules
          }
        , Cmd.none
        )


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


worldChanged : Engine.Model -> Engine.Model -> Maybe Engine.Model
worldChanged old new =
    if
        Engine.getItemsInCurrentLocation old
            /= Engine.getItemsInCurrentLocation new
            || Engine.getCharactersInCurrentLocation old
            /= Engine.getCharactersInCurrentLocation new
            || Engine.getItemsInInventory old
            /= Engine.getItemsInInventory new
            || Engine.getLocations old
            /= Engine.getLocations new
            || Engine.getCurrentLocation old
            /= Engine.getCurrentLocation new
            || Engine.getCurrentScene old
            /= Engine.getCurrentScene new
            || Engine.getEnding old
            /= Engine.getEnding new
    then
        Just new
    else
        Nothing


buildGraph : Engine.Model -> Rules -> Graph Node
buildGraph startingEngineModel rules =
    let
        addIfUnique a list =
            if List.member a list then
                list
            else
                a :: list

        recurOrStop name engineModel nodes =
            if Engine.getEnding engineModel == Nothing then
                addIfUnique (Node.fromName [ name ]) (checkAllInteractables engineModel nodes)
            else
                addIfUnique (Node.fromName [ name ++ " *Ending*" ]) nodes

        findMatcingRule : Engine.Model -> String -> List Node -> List Node
        findMatcingRule engineModel interactable nodes =
            case Tuple.mapFirst (worldChanged engineModel) <| Engine.update interactable engineModel of
                -- normal rule match with changes
                ( Just newEngineModel, Just ruleName ) ->
                    recurOrStop (interactable ++ " -> " ++ ruleName) newEngineModel nodes

                ( Just newEngineModel, Nothing ) ->
                    -- default rule match (take an item or move to a location)
                    recurOrStop (interactable ++ " -> default (take / go)") newEngineModel nodes

                ( Nothing, Just ruleName ) ->
                    -- rule with no changes
                    addIfUnique (Node.fromName [ interactable ++ " -> " ++ ruleName ]) nodes

                ( Nothing, Nothing ) ->
                    -- no matching rules
                    nodes

        checkAllInteractables engineModel nodes =
            getAllInteractables engineModel
                |> List.foldl (findMatcingRule engineModel) nodes

        walkStory =
            (,) [] <| checkAllInteractables startingEngineModel []
    in
        uncurry Graph.init walkStory


buildTree : Engine.Model -> Rules -> Tree
buildTree startingEngineModel rules =
    let
        findMatcingRule engineModel interactable =
            case Tuple.mapFirst (worldChanged engineModel) <| Engine.update interactable engineModel of
                ( Just newEngineModel, Just ruleName ) ->
                    Just <|
                        if Engine.getEnding newEngineModel == Nothing then
                            Tree (interactable ++ " -> " ++ ruleName) <|
                                checkAllInteractables newEngineModel
                        else
                            Tree (interactable ++ " -> " ++ ruleName ++ " *Ending*") []

                ( Just newEngineModel, Nothing ) ->
                    Just <|
                        if Engine.getEnding newEngineModel == Nothing then
                            Tree (interactable ++ " -> default (take / go)") <|
                                checkAllInteractables newEngineModel
                        else
                            Tree (interactable ++ " -> default (take / go) *Ending*") []

                ( Nothing, Just ruleName ) ->
                    Just <| Tree (interactable ++ " -> " ++ ruleName) []

                ( Nothing, Nothing ) ->
                    Nothing

        checkAllInteractables engineModel =
            getAllInteractables engineModel
                |> List.filterMap (findMatcingRule engineModel)
    in
        Tree "*Start*" <| checkAllInteractables startingEngineModel


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
    model ! []


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Graph:" ]
        , textarea [ rows 30, cols 150 ] [ text <| GraphViz.string model.graph ]
        , h2 [] [ text "Tree:" ]
        , ol [] [ treeView model.tree ]
        ]


treeView : Tree -> Html Msg
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
