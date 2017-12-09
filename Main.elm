module Main exposing (..)

import Html exposing (Html, Attribute, div, input, label, text, button, select, option)
import Html.Attributes exposing (class, value, selected, for, id, type_, checked)
import Html.Events exposing (onInput, onClick, onBlur, onClick, onFocus)
import Html.Keyed
import Random
import Array.Hamt as A
import Characters exposing (..)


hiragana : A.Array (A.Array Kana)
hiragana =
    A.append plainHiragana (A.append dakutenHiragana combinationHiragana)


main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Entry =
    { char : Kana, guess : String }


levelKana : A.Array String
levelKana =
    A.map (\l -> A.map (\k -> kana k) l |> A.toList |> String.join ", ") hiragana


type alias Model =
    { entries : List Entry
    , entryCount : Int
    , level : Int
    , cumulative : Bool
    , selectedIdx : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            Model [] 100 0 True Nothing
    in
        ( model, replaceCmd model )


hiraganaForLevel : Int -> Bool -> A.Array Kana
hiraganaForLevel n cumulative =
    if cumulative then
        (A.indexedMap
            (\i e ->
                if (i <= n) then
                    e
                else
                    A.empty
            )
            hiragana
        )
            |> A.foldr (A.append) A.empty
    else
        A.get n hiragana |> Maybe.withDefault A.empty


entryGenerator : Int -> A.Array Kana -> Random.Generator (List Entry)
entryGenerator n possibilities =
    let
        l =
            A.length possibilities

        g =
            Random.int 0 (l - 1)
                |> Random.map (\x -> Entry (A.get x possibilities |> Maybe.withDefault (Hiragana "" "")) "")
    in
        Random.list n g


type Msg
    = Refresh
    | ReplaceEntries (List Entry)
    | UpdateEntry Int String
    | SetLevel String
    | ToggleCumulative
    | Unselect Int
    | Select Int
    | Reveal
    | SetCount String


replaceCmd : Model -> Cmd Msg
replaceCmd m =
    Random.generate ReplaceEntries (entryGenerator m.entryCount (hiraganaForLevel m.level m.cumulative))


get : Int -> List a -> Maybe a
get n xs =
    List.head (List.drop n xs)


updatedAt : Int -> a -> List a -> List a
updatedAt idx item list =
    List.indexedMap
        (\i el ->
            if i == idx then
                item
            else
                el
        )
        list


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
                    get idx model.entries |> Maybe.withDefault (Entry (Hiragana "" "") "")

                newEntry =
                    { oldEntry | guess = val }

                newEntries =
                    updatedAt idx newEntry model.entries
            in
                ( { model | entries = newEntries }, Cmd.none )

        SetLevel l ->
            case String.toInt l of
                Ok n ->
                    if (n >= 0) && (n < (A.length hiragana)) then
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

        Unselect n ->
            if model.selectedIdx == Just n then
                ( { model | selectedIdx = Nothing }, Cmd.none )
            else
                ( model, Cmd.none )

        Select n ->
            ( { model | selectedIdx = Just n }, Cmd.none )

        Reveal ->
            ( { model
                | entries =
                    List.map
                        (\e -> Entry e.char (String.toLower (romaji e.char)))
                        model.entries
              }
            , Cmd.none
            )

        SetCount c ->
            let
                newCount =
                    String.toInt c |> Result.withDefault model.entryCount

                newModel =
                    { model | entryCount = newCount }
            in
                if model.entryCount /= newCount then
                    ( newModel, replaceCmd newModel )
                else
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ controls model.level model.cumulative model.entryCount
          --      , hiraganaList model.level model.cumulative
        , renderEntries model.entries model.selectedIdx
        ]


hiraganaList : Int -> Bool -> Html Msg
hiraganaList level cumulative =
    let
        chars =
            hiraganaForLevel level cumulative
                |> A.map kana
                |> A.toList
                |> String.join " "
    in
        div [] [ text ("Hiragana: " ++ chars) ]


controls : Int -> Bool -> Int -> Html Msg
controls level cumulative entryCount =
    let
        levelCount =
            A.length hiragana

        levels =
            List.range 0 (levelCount - 1)
    in
        div [ class "controls" ]
            [ select [ onInput SetLevel ]
                (List.map
                    (\l ->
                        option [ value (toString l), selected (l == level) ]
                            [ text (A.get l levelKana |> Maybe.withDefault "") ]
                    )
                    levels
                )
            , label []
                [ input [ type_ "checkbox", onClick ToggleCumulative, checked cumulative ] []
                , text "Cumulative"
                ]
            , label []
                [ input
                    [ type_ "range"
                    , onInput SetCount
                    , Html.Attributes.min <| toString 20
                    , Html.Attributes.max <| toString 200
                    , Html.Attributes.step <| toString 10
                    , value <| toString entryCount
                    ]
                    []
                , text <| toString entryCount
                ]
            , button [ onClick Reveal ] [ text "Reveal" ]
            , button [ onClick Refresh ] [ text "Refresh" ]
            ]


renderEntries : List Entry -> Maybe Int -> Html Msg
renderEntries entries selected =
    Html.Keyed.node "div"
        [ class "entries" ]
        (List.indexedMap
            (\i e ->
                renderEntry e
                    i
                    (case selected of
                        Just n ->
                            n == i

                        Nothing ->
                            False
                    )
            )
            entries
        )


renderEntry : Entry -> Int -> Bool -> ( String, Html Msg )
renderEntry entry idx selected =
    let
        correct =
            String.trim
                (String.toLower entry.guess)
                == String.trim (String.toLower (romaji entry.char))

        className =
            if correct then
                "correct"
            else if selected then
                "selected"
            else if entry.guess == "" then
                ""
            else
                "incorrect"

        inputId =
            "input" ++ (toString idx)
    in
        ( (toString idx)
        , div
            [ class ("entry " ++ className)
            ]
            [ input
                [ onInput (UpdateEntry idx)
                , onFocus (Select idx)
                , onBlur (Unselect idx)
                , value entry.guess
                , id inputId
                ]
                []
            , label [ for inputId ] [ text (kana entry.char) ]
            ]
        )
