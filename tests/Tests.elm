module Tests exposing (..)

import Basics exposing (Order(..))
import Expect
import Test exposing (..)


type alias PokerHand =
    ( String, String, String, String, String )


type HandRank
    = HighCard CardValue


type CardValue
    = C2
    | C3
    | C4
    | C5
    | C6
    | C7
    | C8
    | C9
    | CT
    | CJ
    | CQ
    | CK
    | CA


cardValue : String -> CardValue
cardValue card =
    case String.left 1 card of
        "2" ->
            C2

        "3" ->
            C3

        "4" ->
            C4

        "5" ->
            C5

        "6" ->
            C6

        "7" ->
            C7

        "8" ->
            C8

        "9" ->
            C9

        "T" ->
            CT

        "J" ->
            CJ

        "Q" ->
            CQ

        "K" ->
            CK

        "A" ->
            CA

        _ ->
            CQ


handRank : PokerHand -> HandRank
handRank hand =
    case hand of
        ( _, _, _, q, _ ) ->
            HighCard (cardValue q)


cardValueComparator c1 c2 =
    if c1 == c2 then
        EQ
    else
        case ( c1, c2 ) of
            ( C2, _ ) ->
                LT

            ( C3, C2 ) ->
                GT

            ( C3, _ ) ->
                LT

            ( C4, C2 ) ->
                GT

            ( C4, C3 ) ->
                GT

            ( C4, _ ) ->
                LT

            _ ->
                GT


all : Test
all =
    describe "Poker"
        [ describe "cardValueComparator" <|
            List.map testCardValueCompare
                [ ( C2, EQ, C2 )
                , ( C2, LT, C3 )
                , ( C3, GT, C2 )
                , ( C3, EQ, C3 )
                , ( C3, LT, C4 )
                , ( C3, LT, C5 )
                , ( C4, GT, C2 )
                , ( C4, LT, C5 )
                ]
        , describe "cardValue" <|
            List.map testCardValue
                [ ( "2S", C2 )
                , ( "3S", C3 )
                , ( "4S", C4 )
                , ( "5S", C5 )
                , ( "6S", C6 )
                , ( "7S", C7 )
                , ( "8C", C8 )
                , ( "9S", C9 )
                , ( "TS", CT )
                , ( "JS", CJ )
                , ( "QS", CQ )
                , ( "KS", CK )
                , ( "AS", CA )
                ]
        , describe "HandRank"
            [ test "HighCard 9" <|
                \_ ->
                    Expect.equal (HighCard C9)
                        (handRank ( "2H", "3D", "5S", "9C", "8D" ))
            , test "HighCard QC" <|
                \_ ->
                    Expect.equal (HighCard CQ)
                        (handRank ( "2H", "3D", "5S", "QC", "8D" ))
            , test "HighCard QH" <|
                \_ ->
                    Expect.equal (HighCard CQ)
                        (handRank ( "2H", "3D", "5S", "QH", "8D" ))
            , test "HighCard K" <|
                \_ ->
                    Expect.equal (HighCard CK)
                        (handRank ( "2H", "3D", "QC", "KS", "8D" ))
            ]
        ]


testCardValue : ( String, CardValue ) -> Test
testCardValue ( card, value ) =
    test card <|
        \_ ->
            Expect.equal value
                (cardValue card)


testCardValueCompare ( c1, operator, c2 ) =
    test (String.join " " [ toString c1, toString c2 ]) <|
        \_ ->
            Expect.equal operator
                (cardValueComparator c1 c2)
