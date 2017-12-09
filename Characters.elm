module Characters exposing (..)

import Array.Hamt as A


type Kana
    = Hiragana String String
    | Katakana String String

type alias KanaSet = A.Array (A.Array Kana)
type alias Script = {
    plain: KanaSet
    , dakuten: KanaSet
    , combination: KanaSet
}

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

allHiragana : Script
allHiragana =
    { plain = plainHiragana
    , dakuten = dakutenHiragana
    , combination = combinationHiragana
    }

plainHiragana : A.Array (A.Array Kana)
plainHiragana =
    A.fromList
        [ A.fromList
            [ Hiragana "あ" "A"
            , Hiragana "い" "I"
            , Hiragana "う" "U"
            , Hiragana "え" "E"
            , Hiragana "お" "O"
            ]
        , A.fromList
            [ Hiragana "か" "KA"
            , Hiragana "き" "KI"
            , Hiragana "く" "KU"
            , Hiragana "け" "KE"
            , Hiragana "こ" "KO"
            ]
        , A.fromList
            [ Hiragana "さ" "SA"
            , Hiragana "し" "SHI"
            , Hiragana "す" "SU"
            , Hiragana "せ" "SE"
            , Hiragana "そ" "SO"
            ]
        , A.fromList
            [ Hiragana "た" "TA"
            , Hiragana "ち" "CHI"
            , Hiragana "つ" "TSU"
            , Hiragana "て" "TE"
            , Hiragana "と" "TO"
            ]
        , A.fromList
            [ Hiragana "な" "NA"
            , Hiragana "に" "NI"
            , Hiragana "ぬ" "NU"
            , Hiragana "ね" "NE"
            , Hiragana "の" "NO"
            ]
        , A.fromList
            [ Hiragana "は" "HA"
            , Hiragana "ひ" "HI"
            , Hiragana "ふ" "FU"
            , Hiragana "へ" "HE"
            , Hiragana "ほ" "HO"
            ]
        , A.fromList
            [ Hiragana "ま" "MA"
            , Hiragana "み" "MI"
            , Hiragana "む" "MU"
            , Hiragana "め" "ME"
            , Hiragana "も" "MO"
            ]
        , A.fromList
            [ Hiragana "や" "YA"
            , Hiragana "ゆ" "YU"
            , Hiragana "よ" "YO"
            ]
        , A.fromList
            [ Hiragana "ら" "RA"
            , Hiragana "り" "RI"
            , Hiragana "る" "RU"
            , Hiragana "れ" "RE"
            , Hiragana "ろ" "RO"
            ]
        , A.fromList
            [ Hiragana "わ" "WA"
            , Hiragana "を" "WO"
            , Hiragana "ん" "N"
            ]
        ]


dakutenHiragana : A.Array (A.Array Kana)
dakutenHiragana =
    A.fromList
        [ A.fromList
            [ Hiragana "が" "GA"
            , Hiragana "ぎ" "GI"
            , Hiragana "ぐ" "GU"
            , Hiragana "げ" "GE"
            , Hiragana "ご" "GO"
            ]
        , A.fromList
            [ Hiragana "ざ" "ZA"
            , Hiragana "じ" "JI"
            , Hiragana "ず" "ZU"
            , Hiragana "ぜ" "ZE"
            , Hiragana "ぞ" "ZO"
            ]
        , A.fromList
            [ Hiragana "だ" "DA"
            , Hiragana "ぢ" "DZI"
            , Hiragana "づ" "DZU"
            , Hiragana "で" "DE"
            , Hiragana "ど" "DO"
            ]
        , A.fromList
            [ Hiragana "ば" "BA"
            , Hiragana "び" "BI"
            , Hiragana "ぶ" "BU"
            , Hiragana "べ" "BE"
            , Hiragana "ぼ" "BO"
            ]
        , A.fromList
            [ Hiragana "ぱ" "PA"
            , Hiragana "ぴ" "PI"
            , Hiragana "ぷ" "PU"
            , Hiragana "ぺ" "PE"
            , Hiragana "ぽ" "PO"
            ]
        ]


combinationHiragana : A.Array (A.Array Kana)
combinationHiragana =
    A.fromList
        [ A.fromList
            [ Hiragana "きゃ" "KYA"
            , Hiragana "きゅ" "KYU"
            , Hiragana "きょ" "KYO"
            ]
        , A.fromList
            [ Hiragana "ぎゃ" "GYA"
            , Hiragana "ぎゅ" "GYU"
            , Hiragana "ぎょ" "GYO"
            ]
        , A.fromList
            [ Hiragana "しゃ" "SHA"
            , Hiragana "しゅ" "SHU"
            , Hiragana "しょ" "SHO"
            ]
        , A.fromList
            [ Hiragana "じゃ" "JYA"
            , Hiragana "じゅ" "JYU"
            , Hiragana "じょ" "JYO"
            ]
        , A.fromList
            [ Hiragana "ちゃ" "CHA"
            , Hiragana "ちゅ" "CHU"
            , Hiragana "ちょ" "CHO"
            ]
        , A.fromList
            [ Hiragana "ぢゃ" "DZYA"
            , Hiragana "ぢゅ" "DZYU"
            , Hiragana "ぢょ" "DZYO"
            ]
        , A.fromList
            [ Hiragana "にゃ" "NYA"
            , Hiragana "にゅ" "NYU"
            , Hiragana "にょ" "NYO"
            ]
        , A.fromList
            [ Hiragana "ひゃ" "HYA"
            , Hiragana "ひゅ" "HYU"
            , Hiragana "ひょ" "HYO"
            ]
        , A.fromList
            [ Hiragana "びゃ" "BYA"
            , Hiragana "びゅ" "BYU"
            , Hiragana "びょ" "BYO"
            ]
        , A.fromList
            [ Hiragana "ぴゃ" "PYA"
            , Hiragana "ぴゅ" "PYU"
            , Hiragana "ぴょ" "PYO"
            ]
        , A.fromList
            [ Hiragana "みゃ" "MYA"
            , Hiragana "みゅ" "MYU"
            , Hiragana "みょ" "MYO"
            ]
        , A.fromList
            [ Hiragana "りゃ" "RYA"
            , Hiragana "りゅ" "RYU"
            , Hiragana "りょ" "RYO"
            ]
        ]
