port module Main exposing (..)

import Engine exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (..)
import Manifest exposing (..)
import Rules exposing (..)
import ClientTypes exposing (..)
import Graph exposing (..)
import Graphbuilder


type alias Model =
    { engineModel : Engine.Model
    , graph : Graph ( String, Bool ) String
    , loading : Bool
    , paths : List (List (Edge String))
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

        ( paths, graph ) =
            Graphbuilder.build engineModel rules
    in
        ( { engineModel = engineModel
          , graph = graph
          , loading = True
          , paths = paths
          , highlightPath = Nothing
          }
        , drawGraph <| Graphbuilder.toGraphViz graph
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
                    , drawGraph <| Graphbuilder.toGraphViz <| Graphbuilder.highlightPath (color i) path model.graph
                    )

        _ ->
            model ! []


colors : List String
colors =
    [ "orange"
    , "green"
    , "blue"
    , "purple"
    , "red"
    ]


color : Int -> String
color index =
    colors
        |> List.drop (index % List.length colors)
        |> List.head
        |> Maybe.withDefault "white"


view : Model -> Html Msg
view model =
    div [ style [ ( "textAlign", "center" ) ] ] <|
        [ ul [ class "paths" ] <|
            [ h3 [] [ text "Choose a path:" ] ]
                ++ List.indexedMap
                    (\i _ ->
                        li
                            [ onClick <| HighlightPath i
                            , class <|
                                if model.highlightPath == Just i then
                                    "active"
                                else
                                    ""
                            , style [ ( "backgroundColor", color i ) ]
                            ]
                            [ text <| Basics.toString <| i + 1 ]
                    )
                    model.paths
        , div [ id "graph" ] [ span [] [] ]
        ]
            ++ if model.loading then
                [ div [ class "loading" ] [ span [] <| [ text "loading..." ] ] ]
               else
                []
