module Main exposing (..)

import Engine exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (..)
import Tuple exposing (..)
import Manifest exposing (..)
import Rules exposing (rulesData)
import ClientTypes exposing (..)


type alias Model =
    { engineModel : Engine.Model
    , path : Path
    }


type Path
    = Path String (List Path)


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

        path =
            getPath engineModel rules

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
          , path = path
          }
        , Cmd.none
        )


getPath : Engine.Model -> Rules -> Path
getPath startingEngineModel rules =
    let
        getAllInteractables engineModel =
            -- TODO/FIX
            -- not completely true - the client can expose other interactables
            -- such as the "exits" component or hypermedia text
            -- Maybe need to ask the client for this list?
            Engine.getCharactersInCurrentLocation engineModel
                ++ Engine.getItemsInCurrentLocation engineModel
                ++ Engine.getItemsInInventory engineModel
                ++ Engine.getLocations engineModel

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

        findMatcingRule engineModel interactable =
            case Tuple.mapFirst (worldChanged engineModel) <| Engine.update interactable engineModel of
                ( Just newEngineModel, Just ruleName ) ->
                    Just <|
                        if Engine.getEnding newEngineModel == Nothing then
                            Path (interactable ++ " -> " ++ ruleName) <|
                                checkAllInteractables newEngineModel
                        else
                            Path (interactable ++ " -> " ++ ruleName ++ " *Ending*") []

                ( Just newEngineModel, Nothing ) ->
                    Just <|
                        if Engine.getEnding newEngineModel == Nothing then
                            Path (interactable ++ " -> default (take / go)") <|
                                checkAllInteractables newEngineModel
                        else
                            Path (interactable ++ " -> default (take / go) *Ending*") []

                ( Nothing, Just ruleName ) ->
                    Just <| Path (interactable ++ " -> " ++ ruleName) []

                ( Nothing, Nothing ) ->
                    Nothing

        checkAllInteractables engineModel =
            getAllInteractables engineModel
                |> List.filterMap (findMatcingRule engineModel)
    in
        Path "*Start*" <| checkAllInteractables startingEngineModel


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
    ol [] [ pathView model.path ]


olStyle : Html.Attribute Msg
olStyle =
    style
        [ ( "borderLeft", "1px solid lightgray" )
        , ( "margin", "5px 25px" )
        , ( "padding", "5px 25px" )
        ]


pathView : Path -> Html Msg
pathView path =
    case path of
        Path a [] ->
            li [] [ text a ]

        Path a path ->
            li []
                [ text a
                , ol [ olStyle ] <|
                    List.map pathView path
                ]
