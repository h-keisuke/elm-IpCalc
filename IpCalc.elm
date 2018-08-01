module IpCalc exposing (..)

import Html exposing (Html, div, text, input, program, Attribute)
import Html.Events exposing (onInput)
import Html.Attributes as Attr
import String
import Result
import Bitwise exposing (..)


-- Model


type alias Model =
    { ipAddress : IpAddress
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { ipAddress = IpAddress 192 168 0 1 24
    }


type alias IpAddress =
    { octet0 : Int
    , octet1 : Int
    , octet2 : Int
    , octet3 : Int
    , cidr : Int
    }



-- MESSAGES


type Msg
    = ChangeOctet0 String
    | ChangeOctet1 String
    | ChangeOctet2 String
    | ChangeOctet3 String
    | ChangeCidr String



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "Ip Address : "
        , input
            [ onInput ChangeOctet0
            , Attr.type_ "number"
            , Attr.size 3
            , Attr.min "0"
            , Attr.max "255"
            , Attr.maxlength 3
            , Attr.pattern "[0-9]|[0-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]"
            , Attr.required True
            , Attr.defaultValue (toString model.ipAddress.octet0)
            ]
            []
        , text "."
        , input
            [ onInput ChangeOctet1
            , Attr.type_ "number"
            , Attr.size 3
            , Attr.min "0"
            , Attr.max "255"
            , Attr.maxlength 3
            , Attr.pattern "[0-9]|[0-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]"
            , Attr.required True
            , Attr.defaultValue (toString model.ipAddress.octet1)
            ]
            []
        , text "."
        , input
            [ onInput ChangeOctet2
            , Attr.type_ "number"
            , Attr.size 3
            , Attr.min "0"
            , Attr.max "255"
            , Attr.maxlength 3
            , Attr.pattern "[0-9]|[0-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]"
            , Attr.required True
            , Attr.defaultValue (toString model.ipAddress.octet2)
            ]
            []
        , text "."
        , input
            [ onInput ChangeOctet3
            , Attr.type_ "number"
            , Attr.size 3
            , Attr.min "0"
            , Attr.max "255"
            , Attr.maxlength 3
            , Attr.pattern "[0-9]|[0-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]"
            , Attr.required True
            , Attr.defaultValue (toString model.ipAddress.octet3)
            ]
            []
        , text "/"
        , input
            [ onInput ChangeCidr
            , Attr.type_ "number"
            , Attr.size 2
            , Attr.min "1"
            , Attr.max "32"
            , Attr.maxlength 2
            , Attr.pattern "[1-9]|[12][0-9]|3[0-2]"
            , Attr.required True
            , Attr.defaultValue (toString model.ipAddress.cidr)
            ]
            []
        , viewIpAddress model
        , viewNetMask model
        , viewWildCard model
        , viewBroadcast model
        , viewNetwork model
        , viewHosts model
        ]


viewIpAddress : Model -> Html Msg
viewIpAddress model =
    let
        addressStr =
            bitsToString (addressToBits model.ipAddress)
    in
        div []
            [ text "Host Address : "
            , text addressStr
            ]


viewNetMask : Model -> Html Msg
viewNetMask model =
    let
        maskStr =
            bitsToString (cidrToBits model.ipAddress.cidr)
    in
        div []
            [ text "Netmask : "
            , text maskStr
            ]


viewWildCard : Model -> Html Msg
viewWildCard model =
    let
        maskBits =
            cidrToBits model.ipAddress.cidr

        wildCardBits =
            complement maskBits

        wildCardStr =
            bitsToString wildCardBits
    in
        div []
            [ text "Wildcard : "
            , text wildCardStr
            ]


viewBroadcast : Model -> Html Msg
viewBroadcast model =
    let
        addressBits =
            addressToBits model.ipAddress

        maskBits =
            cidrToBits model.ipAddress.cidr

        wildCardBits =
            complement maskBits

        broadcastBits =
            or addressBits wildCardBits

        broadcastStr =
            bitsToString broadcastBits
    in
        div []
            [ text "Broadcast : "
            , text broadcastStr
            ]


viewNetwork : Model -> Html Msg
viewNetwork model =
    let
        addressBits =
            addressToBits model.ipAddress

        maskBits =
            cidrToBits model.ipAddress.cidr

        networkBits =
            and addressBits maskBits

        networkStr =
            (bitsToString networkBits) ++ "/" ++ (toString model.ipAddress.cidr)
    in
        div []
            [ text "Network : "
            , text networkStr
            ]


viewHosts : Model -> Html Msg
viewHosts model =
    let
        hosts =
            complement (cidrToBits model.ipAddress.cidr) + 1
    in
        div []
            [ text "Hosts : "
            , text (toString hosts)
            ]


addressToBits : IpAddress -> Int
addressToBits addr =
    or (shiftLeftBy 24 addr.octet0) (or (shiftLeftBy 16 addr.octet1) (or (shiftLeftBy 8 addr.octet2) addr.octet3))


cidrToBits : Int -> Int
cidrToBits cidr =
    shiftLeftBy (32 - cidr) (complement 0)


bitsToString : Int -> String
bitsToString bits =
    toString (shiftRightZfBy 24 (and bits (shiftLeftBy 24 255)))
        ++ "."
        ++ toString (shiftRightZfBy 16 (and bits (shiftLeftBy 16 255)))
        ++ "."
        ++ toString (shiftRightZfBy 8 (and bits (shiftLeftBy 8 255)))
        ++ "."
        ++ toString (and bits 255)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeOctet0 newOctet0 ->
            ( { model | ipAddress = IpAddress (Result.withDefault 0 (String.toInt newOctet0)) model.ipAddress.octet1 model.ipAddress.octet2 model.ipAddress.octet3 model.ipAddress.cidr }
            , Cmd.none
            )

        ChangeOctet1 newOctet1 ->
            ( { model | ipAddress = IpAddress model.ipAddress.octet0 (Result.withDefault 0 (String.toInt newOctet1)) model.ipAddress.octet2 model.ipAddress.octet3 model.ipAddress.cidr }
            , Cmd.none
            )

        ChangeOctet2 newOctet2 ->
            ( { model | ipAddress = IpAddress model.ipAddress.octet0 model.ipAddress.octet1 (Result.withDefault 0 (String.toInt newOctet2)) model.ipAddress.octet3 model.ipAddress.cidr }
            , Cmd.none
            )

        ChangeOctet3 newOctet3 ->
            ( { model | ipAddress = IpAddress model.ipAddress.octet0 model.ipAddress.octet1 model.ipAddress.octet2 (Result.withDefault 0 (String.toInt newOctet3)) model.ipAddress.cidr }
            , Cmd.none
            )

        ChangeCidr newCidr ->
            ( { model | ipAddress = IpAddress model.ipAddress.octet0 model.ipAddress.octet1 model.ipAddress.octet2 model.ipAddress.octet3 (Result.withDefault 0 (String.toInt newCidr)) }
            , Cmd.none
            )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Main


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
