module Main exposing (..)

import Html exposing (Html, Attribute, div, input, label, text, button, select, option)
import Html.Attributes exposing (class, value, selected, for, id, type_, checked)
import Html.Events exposing (onInput, onClick, onBlur, onClick)
import Html.Keyed
import Random
import Random.Array
import Array exposing (Array)


main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Kana
    = Hiragana String String
    | Katakana String String


kana : Kana -> String
kana k =
    case k of
        Katakana k r ->
            k

        Hiragana k r ->
            k


romaji : Kana -> String
romaji k =
    case k of
        Katakana k r ->
            r

        Hiragana k r ->
            r


hiragana : Array (Array Kana)
hiragana =
    Array.fromList
        [ Array.fromList
            [ Hiragana "あ" "A"
            , Hiragana "い" "I"
            , Hiragana "う" "U"
            , Hiragana "え" "E"
            , Hiragana "お" "O"
            ]
        , Array.fromList
            [ Hiragana "か" "KA"
            , Hiragana "き" "KI"
            , Hiragana "く" "KU"
            , Hiragana "け" "KE"
            , Hiragana "こ" "KO"
            ]
        , Array.fromList
            [ Hiragana "さ" "SA"
            , Hiragana "し" "SHI"
            , Hiragana "す" "SU"
            , Hiragana "せ" "SE"
            , Hiragana "そ" "SO"
            ]
        , Array.fromList
            [ Hiragana "た" "TA"
            , Hiragana "ち" "CHI"
            , Hiragana "つ" "TSU"
            , Hiragana "て" "TE"
            , Hiragana "と" "TO"
            ]
        , Array.fromList
            [ Hiragana "な" "NA"
            , Hiragana "に" "NI"
            , Hiragana "ぬ" "NU"
            , Hiragana "ね" "NE"
            , Hiragana "の" "NO"
            ]
        , Array.fromList
            [ Hiragana "は" "HA"
            , Hiragana "ひ" "HI"
            , Hiragana "ふ" "HU"
            , Hiragana "へ" "HE"
            , Hiragana "ほ" "HO"
            ]
        , Array.fromList
            [ Hiragana "ま" "MA"
            , Hiragana "み" "MI"
            , Hiragana "む" "MU"
            , Hiragana "め" "ME"
            , Hiragana "も" "MO"
            ]
        , Array.fromList
            [ Hiragana "や" "YA"
            , Hiragana "ゆ" "YU"
            , Hiragana "よ" "YO"
            ]
        , Array.fromList
            [ Hiragana "ら" "RA"
            , Hiragana "り" "RI"
            , Hiragana "る" "RU"
            , Hiragana "れ" "RE"
            , Hiragana "ろ" "RO"
            ]
        , Array.fromList
            [ Hiragana "わ" "WA"
            , Hiragana "を" "WO"
            , Hiragana "ん" "N"
            ]
        ]


type alias Entry =
    { char : Kana, guess : String }


levelKana : Array String
levelKana =
    Array.map (\l -> Array.map (\k -> kana k) l |> Array.toList |> String.join ", ") hiragana


type alias Model =
    { entries : Array Entry
    , entryCount : Int
    , level : Int
    , cumulative : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            Model Array.empty 100 0 True
    in
        ( model, replaceCmd model )


hiraganaForLevel : Int -> Bool -> Array Kana
hiraganaForLevel n cumulative =
    if cumulative then
        (Array.indexedMap
            (\i e ->
                if (i <= n) then
                    e
                else
                    Array.empty
            )
            hiragana
        )
            |> Array.foldr (Array.append) Array.empty
    else
        Array.get n hiragana |> Maybe.withDefault Array.empty


entryGenerator : Int -> Array Kana -> Random.Generator (Array Entry)
entryGenerator n possibilities =
    let
        l =
            Array.length possibilities

        g =
            Random.int 0 (l - 1)
                |> Random.map (\x -> Entry (Array.get x possibilities |> Maybe.withDefault (Hiragana "" "")) "")

        --
    in
        Random.Array.array n g


type Msg
    = Refresh
    | ReplaceEntries (Array Entry)
    | UpdateEntry Int String
    | SetLevel String
    | ToggleCumulative


replaceCmd : Model -> Cmd Msg
replaceCmd m =
    Random.generate ReplaceEntries (entryGenerator m.entryCount (hiraganaForLevel m.level m.cumulative))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( model, replaceCmd model )

        ReplaceEntries entries ->
            ( { model | entries = entries }, Cmd.none )

        UpdateEntry idx val ->
            let
                oldEntry =
                    Array.get idx model.entries |> Maybe.withDefault (Entry (Hiragana "" "") "")

                newEntry =
                    { oldEntry | guess = val }

                newEntries =
                    Array.set idx newEntry model.entries
            in
                ( { model | entries = newEntries }, Cmd.none )

        SetLevel l ->
            case String.toInt l of
                Ok n ->
                    if (n > 0) && (n < (Array.length hiragana)) then
                        let
                            m =
                                { model | level = n }
                        in
                            ( m, replaceCmd m )
                    else
                        ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ToggleCumulative ->
            let
                m =
                    { model | cumulative = (not model.cumulative) }
            in
                ( m, replaceCmd m )


view : Model -> Html Msg
view model =
    div []
        [ controls model.level model.cumulative
        , renderEntries model.entries
        ]


controls : Int -> Bool -> Html Msg
controls n cumulative =
    let
        levelCount =
            Array.length hiragana

        levels =
            List.range 0 (levelCount - 1)
    in
        div [ class "controls" ]
            [ select [ onInput SetLevel ]
                (List.map
                    (\l ->
                        option [ value (toString l), selected (l == n) ]
                            [ text ("Level " ++ (toString (l + 1)) ++ " (" ++ (Array.get l levelKana |> Maybe.withDefault "") ++ ")") ]
                    )
                    levels
                )
            , label []
                [ input [ type_ "checkbox", onClick ToggleCumulative, checked cumulative ] []
                , text "Cumulative"
                ]
            , button [ onClick Refresh ] [ text "Refresh" ]
            ]


renderEntries : Array Entry -> Html Msg
renderEntries l =
    Html.Keyed.node "div" [ class "entries" ] (Array.indexedMap renderEntry l |> Array.toList)


renderEntry : Int -> Entry -> ( String, Html Msg )
renderEntry i e =
    let
        correct =
            String.trim (String.toLower e.guess) == String.trim (String.toLower (romaji e.char))

        className =
            if e.guess == "" then
                ""
            else if correct then
                "correct"
            else
                "incorrect"

        inputId =
            "input" ++ (toString i)
    in
        ( (toString i)
        , div
            [ class ("entry " ++ className)
            ]
            [ input [ onInput (UpdateEntry i), value e.guess, id inputId ] []
            , label [ for inputId ] [ textFor e.char ]
            ]
        )


textFor : Kana -> Html Msg
textFor c =
    text (kana c)
