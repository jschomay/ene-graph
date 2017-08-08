port module Main exposing (..)

import Engine exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (..)
import Manifest exposing (..)
import Rules exposing (rulesData)
import ClientTypes exposing (..)
import Graph.GraphViz as GraphViz
import Graph exposing (Graph)
import Graph.Node as Node exposing (Node)
import Graph.Edge as Edge exposing (Edge)
import Graphbuilder


type alias Model =
    { engineModel : Engine.Model
    , graph : Graph Node
    , loading : Bool
    , showNonChangingRules : Bool
    , paths : List (List Edge)
    , highlightPath : Maybe Int
    }


type Msg
    = Interact String
    | Loaded
    | HighlightPath Int


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
            [ loadScene "learnOfMystery"
            , moveTo "Home"
            , moveItemToLocation "Umbrella" "Home"
            , moveItemToLocationFixed "VegatableGarden" "Garden"
            , addLocation "Home"
            , addLocation "Garden"
            , moveCharacterToLocation "Harry" "Garden"
            , moveItemToLocation "Pint" "Pub"
            ]

        -- [ moveTo "Cottage"
        -- , loadScene "start"
        -- , addLocation "Cottage"
        -- , addLocation "River"
        -- , addLocation "Woods"
        -- , addLocation "Grandma's house"
        -- , moveItemToLocation "Cape" "Cottage"
        -- , moveItemToLocation "Basket of food" "Cottage"
        -- , moveCharacterToLocation "Little Red Riding Hood" "Cottage"
        -- , moveCharacterToLocation "Mother" "Cottage"
        -- , moveCharacterToLocation "Wolf" "Woods"
        -- , moveCharacterToLocation "Grandma" "Grandma's house"
        -- ]
        ( paths, graph ) =
            Graphbuilder.build False engineModel rules
    in
        ( { engineModel = engineModel
          , graph = graph
          , loading = True
          , showNonChangingRules = False
          , paths = paths
          , highlightPath = Nothing
          }
        , drawGraph <| GraphViz.string graph
        )


port drawGraph : String -> Cmd msg


port loaded : (Bool -> msg) -> Sub msg


subscribe : Model -> Sub Msg
subscribe model =
    loaded (always Loaded)


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

        HighlightPath i ->
            case List.head <| List.drop i model.paths of
                Nothing ->
                    model ! []

                Just path ->
                    ( { model | highlightPath = Just i, loading = True }
                    , drawGraph <| GraphViz.string <| Graphbuilder.highlightPath path model.graph
                    )

        _ ->
            model ! []


view : Model -> Html Msg
view model =
    div [ style [ ( "textAlign", "center" ) ] ] <|
        [ ul [ class "paths" ] <|
            List.indexedMap (\i _ -> li [ onClick <| HighlightPath i ] [ text <| toString <| i + 1 ])
                model.paths
        , div [ id "graph" ] [ span [] [] ]
        ]
            ++ if model.loading then
                [ div [ class "loading" ] [ text "loading..." ] ]
               else
                []
