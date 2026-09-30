port module Main exposing (main)

import Browser
import Game
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E



-- PORTS


port saveState : E.Value -> Cmd msg



-- MODEL


type alias Model =
    { base : String
    , scratches : List String
    , rolls : List Int -- newest first
    , confirmReset : Bool
    }


defaultModel : Model
defaultModel =
    { base = "0.25"
    , scratches = [ "6", "7", "8", "9" ]
    , rolls = []
    , confirmReset = False
    }


init : D.Value -> ( Model, Cmd Msg )
init flags =
    ( D.decodeValue decoder flags |> Result.withDefault defaultModel, Cmd.none )


decoder : D.Decoder Model
decoder =
    D.map4 Model
        (D.field "base" D.string)
        (D.field "scratches" (D.list D.string))
        (D.field "rolls" (D.list D.int))
        (D.succeed False)


encode : Model -> E.Value
encode model =
    E.object
        [ ( "base", E.string model.base )
        , ( "scratches", E.list E.string model.scratches )
        , ( "rolls", E.list E.int model.rolls )
        ]


{-| Scratches parsed and validated, in multiplier order.
-}
validScratches : Model -> Result String (List Int)
validScratches model =
    let
        parsed =
            List.filterMap (String.trim >> String.toInt) model.scratches
    in
    if List.length parsed /= 4 || List.any (\s -> s < 2 || s > 12) parsed then
        Err "Scratches must be whole numbers from 2 to 12."

    else if List.length (unique parsed) /= 4 then
        Err "Scratches must be four different horses."

    else
        Ok parsed


unique : List Int -> List Int
unique =
    List.foldr
        (\x acc ->
            if List.member x acc then
                acc

            else
                x :: acc
        )
        []


baseValue : Model -> Float
baseValue model =
    String.toFloat model.base
        |> Maybe.withDefault 0
        |> Basics.max 0



-- UPDATE


type Msg
    = SetBase String
    | SetScratch Int String
    | Roll Int
    | Undo
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        m =
            { model | confirmReset = False }

        newModel =
            case msg of
                SetBase s ->
                    { m | base = s }

                SetScratch i s ->
                    { m
                        | scratches =
                            List.indexedMap
                                (\j old ->
                                    if i == j then
                                        String.filter Char.isDigit s |> String.left 2

                                    else
                                        old
                                )
                                m.scratches
                    }

                Roll n ->
                    { m | rolls = n :: m.rolls }

                Undo ->
                    { m | rolls = List.drop 1 m.rolls }

                Reset ->
                    if model.confirmReset then
                        { m | rolls = [] }

                    else
                        { m | confirmReset = True }
    in
    ( newModel, saveState (encode newModel) )



-- VIEW


view : Model -> Html Msg
view model =
    main_ [ class "app" ]
        [ header [ class "top" ]
            [ h1 [] [ span [ class "logo" ] [ text "🐎" ], text "Horse Game" ]
            , button
                [ class "reset"
                , classList [ ( "armed", model.confirmReset ) ]
                , onClick Reset
                , disabled (List.isEmpty model.rolls)
                ]
                [ text
                    (if model.confirmReset then
                        "Tap to confirm"

                     else
                        "New game"
                    )
                ]
            ]
        , viewSetup model
        , case validScratches model of
            Err err ->
                p [ class "alert" ] [ text err ]

            Ok scratches ->
                viewGame model scratches
        , footer []
            [ text "Win odds are computed exactly. "
            , a [ href "https://www.travishinkelman.com/horse-game/" ] [ text "About the game" ]
            ]
        ]


viewSetup : Model -> Html Msg
viewSetup model =
    section [ class "card setup" ]
        [ label [ class "field" ]
            [ span [ class "label" ] [ text "Base $" ]
            , input
                [ type_ "number"
                , attribute "inputmode" "decimal"
                , Html.Attributes.min "0.25"
                , step "0.25"
                , value model.base
                , onInput SetBase
                , class "base"
                ]
                []
            ]
        , div [ class "field" ]
            [ span [ class "label" ] [ text "Scratches" ]
            , div [ class "row" ]
                (List.indexedMap
                    (\i s ->
                        label [ class "scratch" ]
                            [ input
                                [ type_ "text"
                                , attribute "inputmode" "numeric"
                                , attribute "aria-label" ("Scratch " ++ String.fromInt (i + 1))
                                , maxlength 2
                                , value s
                                , onInput (SetScratch i)
                                ]
                                []
                            , small [] [ text ("×" ++ String.fromInt (i + 1)) ]
                            ]
                    )
                    model.scratches
                )
            ]
        ]


viewGame : Model -> List Int -> Html Msg
viewGame model scratches =
    let
        probs =
            Game.winProbabilities scratches model.rolls

        ranked =
            List.sortBy (Tuple.second >> negate) probs

        leader =
            List.head ranked

        rankOf h =
            ranked
                |> List.indexedMap (\i ( horse, _ ) -> ( i + 1, horse ))
                |> List.filter (\( _, horse ) -> horse == h)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 99

        over =
            Game.winner scratches model.rolls
    in
    div [ class "game" ]
        [ div [ class "stats" ]
            [ stat "Kitty" (money (Game.kitty (baseValue model) scratches model.rolls)) Nothing
            , stat "Rolls" (String.fromInt (List.length model.rolls)) Nothing
            , case leader of
                Just ( h, p ) ->
                    stat
                        (if over == Nothing then
                            "Favorite"

                         else
                            "Winner"
                        )
                        ("#" ++ String.fromInt h)
                        (Just ( h, pct p ))

                Nothing ->
                    text ""
            ]
        , case over of
            Just w ->
                div [ class "banner", horseStyle w ] [ text ("🏆 Horse " ++ String.fromInt w ++ " wins!") ]

            Nothing ->
                text ""
        , viewPad scratches (over /= Nothing) model.rolls
        , viewHistory model.rolls
        , section [ class "card track" ]
            (List.map
                (\( h, p ) -> viewLane model.rolls (rankOf h) h p)
                probs
            )
        ]


stat : String -> String -> Maybe ( Int, String ) -> Html msg
stat heading val sub =
    div [ class "stat" ]
        [ span [ class "label" ] [ text heading ]
        , case sub of
            Just ( h, s ) ->
                strong [] [ span [ class "chip", horseStyle h ] [ text (String.fromInt h) ], text s ]

            Nothing ->
                strong [] [ text val ]
        ]


viewPad : List Int -> Bool -> List Int -> Html Msg
viewPad scratches over rolls =
    section [ class "card pad" ]
        (List.map
            (\h ->
                case Game.scratchMultiplier scratches h of
                    Just mult ->
                        button [ class "key scratched", onClick (Roll h), disabled over ]
                            [ text (String.fromInt h), small [] [ text ("×" ++ String.fromInt mult) ] ]

                    Nothing ->
                        button [ class "key", horseStyle h, onClick (Roll h), disabled over ]
                            [ text (String.fromInt h) ]
            )
            Game.allHorses
            ++ [ button [ class "key undo", onClick Undo, disabled (List.isEmpty rolls), attribute "aria-label" "Undo" ]
                    [ text "↶" ]
               ]
        )


viewHistory : List Int -> Html msg
viewHistory rolls =
    div [ class "history" ]
        (if List.isEmpty rolls then
            [ span [ class "empty" ] [ text "Tap a number to record a roll" ] ]

         else
            List.indexedMap
                (\i r -> span [ class "chip", classList [ ( "faded", i > 0 ) ], horseStyle r ] [ text (String.fromInt r) ])
                (List.take 30 rolls)
        )


viewLane : List Int -> Int -> Int -> Float -> Html msg
viewLane rolls rank h p =
    let
        total =
            Game.totalSteps h

        left =
            Basics.max 0 (Game.stepsRemaining rolls h)

        done =
            toFloat (total - left) / toFloat total * 100
    in
    div [ class "lane", classList [ ( "top", rank <= 3 && p > 0 ) ] ]
        [ span [ class "chip", horseStyle h ] [ text (String.fromInt h) ]
        , div [ class "bar" ]
            [ div [ class "fill", style "width" (String.fromFloat done ++ "%"), style "background" (Game.horseColor h) ] []
            , span [ class "left" ] [ text (String.fromInt left ++ " to go") ]
            ]
        , span [ class "pct" ]
            [ text
                (case ( rank, p > 0 ) of
                    ( _, False ) -> ""
                    ( 1, _ ) -> "🥇 "
                    ( 2, _ ) -> "🥈 "
                    ( 3, _ ) -> "🥉 "
                    _ -> ""
                )
            , text (pct p)
            ]
        ]


horseStyle : Int -> Attribute msg
horseStyle h =
    attribute "style"
        ("background:"
            ++ Game.horseColor h
            ++ ";color:"
            ++ (if Game.usesDarkText h then
                    "#1a1a1a"

                else
                    "#fff"
               )
        )


pct : Float -> String
pct p =
    let
        tenths =
            round (p * 1000)
    in
    if p > 0 && tenths == 0 then
        "<0.1%"

    else
        String.fromInt (tenths // 10) ++ "." ++ String.fromInt (modBy 10 tenths) ++ "%"


money : Float -> String
money x =
    let
        cents =
            round (x * 100)
    in
    "$" ++ String.fromInt (cents // 100) ++ "." ++ String.padLeft 2 '0' (String.fromInt (modBy 100 cents))



-- MAIN


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
