module Main exposing (main)

import Block exposing (Block)
import BlockIndex
import BlockType
import Blocks
import Browser
import Browser.Events
import Device exposing (Device)
import Dict
import Html exposing (Html)
import Html.Attributes
import Interconnect exposing (Interconnect)
import InterconnectIndex exposing (InterconnectIndex)
import InterconnectIndexSet exposing (InterconnectIndexSet)
import InterconnectType
import Interconnects exposing (Interconnects)
import InterconnectsIndex exposing (InterconnectsIndex)
import Json.Decode
import Svg
import Svg.Attributes
import Svg.Events


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Zoom
    = ActualSize
    | FitToPage


type Focus
    = InterconnectFocus
        { at : InterconnectIndex
        , froms : InterconnectIndexSet
        , thrus : InterconnectIndexSet
        }
    | InterconnectsFocus
        { at : InterconnectsIndex
        , froms : InterconnectIndexSet
        , thrus : InterconnectIndexSet
        }
    | NoFocus


type alias Model =
    { device : Int
    , devices : List Device
    , focus : Focus
    , message : String
    , zoom : Zoom
    }


type Msg
    = KeyPress String
    | SetInterconnectFocus InterconnectIndex
    | SetInterconnectsFocus InterconnectsIndex
    | SetNoFocus


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        decoder =
            Json.Decode.list Device.decode

        devices =
            case Json.Decode.decodeValue decoder flags of
                Ok blocks ->
                    blocks

                Err error ->
                    [ Json.Decode.errorToString error |> Device.empty ]
    in
    ( { device = 0
      , devices = devices
      , focus = NoFocus
      , message = ""
      , zoom = ActualSize
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPress "d" ->
            ( { model
                | device = modBy (List.length model.devices) (model.device + 1)
                , focus = NoFocus
              }
            , Cmd.none
            )

        KeyPress "z" ->
            ( { model
                | zoom =
                    case model.zoom of
                        FitToPage ->
                            ActualSize

                        ActualSize ->
                            FitToPage
              }
            , Cmd.none
            )

        KeyPress "8" ->
            -- up
            move interconnectUp interconnectsUp model

        KeyPress "2" ->
            -- down
            move interconnectDown interconnectsDown model

        KeyPress "4" ->
            -- left
            move interconnectLeft interconnectsLeft model

        KeyPress "6" ->
            -- right
            move interconnectRight interconnectsRight model

        KeyPress key ->
            ( { model | message = key }, Cmd.none )

        SetInterconnectFocus at ->
            ( { model | focus = setInterconnectFocus at model }, Cmd.none )

        SetInterconnectsFocus at ->
            ( { model | focus = setInterconnectsFocus at model }, Cmd.none )

        SetNoFocus ->
            ( { model | focus = NoFocus }, Cmd.none )


move :
    (InterconnectIndex -> InterconnectIndex)
    -> (InterconnectsIndex -> InterconnectsIndex)
    -> Model
    -> ( Model, Cmd Msg )
move moveInterconnect moveInterconnects model =
    case model.focus of
        InterconnectFocus { at } ->
            case setInterconnectFocus (moveInterconnect at) model of
                NoFocus ->
                    ( model, Cmd.none )

                focus ->
                    ( { model | focus = focus }, Cmd.none )

        InterconnectsFocus { at } ->
            case setInterconnectsFocus (moveInterconnects at) model of
                NoFocus ->
                    ( model, Cmd.none )

                focus ->
                    ( { model | focus = focus }, Cmd.none )

        _ ->
            ( model, Cmd.none )


interconnectUp : InterconnectIndex -> InterconnectIndex
interconnectUp { x, y, t, i } =
    { x = x, y = y, t = t, i = i - 2 }


interconnectsUp : InterconnectsIndex -> InterconnectsIndex
interconnectsUp { x, y, t } =
    { x = x, y = y + 1, t = t }


interconnectDown : InterconnectIndex -> InterconnectIndex
interconnectDown { x, y, t, i } =
    { x = x, y = y, t = t, i = i + 2 }


interconnectsDown : InterconnectsIndex -> InterconnectsIndex
interconnectsDown { x, y, t } =
    { x = x, y = y - 1, t = t }


interconnectLeft : InterconnectIndex -> InterconnectIndex
interconnectLeft { x, y, t, i } =
    { x = x, y = y, t = t, i = i - 1 }


interconnectsLeft : InterconnectsIndex -> InterconnectsIndex
interconnectsLeft { x, y, t } =
    { x = x - 1, y = y, t = t }


interconnectRight : InterconnectIndex -> InterconnectIndex
interconnectRight { x, y, t, i } =
    { x = x, y = y, t = t, i = i + 1 }


interconnectsRight : InterconnectsIndex -> InterconnectsIndex
interconnectsRight { x, y, t } =
    { x = x + 1, y = y, t = t }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyPress keyPress


keyPress : Json.Decode.Decoder Msg
keyPress =
    Json.Decode.map KeyPress (Json.Decode.field "key" Json.Decode.string)


setInterconnectFocus : InterconnectIndex -> Model -> Focus
setInterconnectFocus at model =
    let
        device =
            currentDevice model

        focus interconnect =
            InterconnectFocus
                { at = at
                , froms = interconnect.froms
                , thrus = interconnect.thrus
                }
    in
    Blocks.get at device.blocks
        |> Maybe.map (Block.get at.t)
        |> Maybe.andThen (Interconnects.get at.i)
        |> Maybe.map focus
        |> Maybe.withDefault NoFocus


setInterconnectsFocus : InterconnectsIndex -> Model -> Focus
setInterconnectsFocus at model =
    let
        device =
            currentDevice model

        focus muxes =
            InterconnectsFocus
                { at = at
                , froms = Interconnects.froms muxes
                , thrus = Interconnects.thrus muxes
                }
    in
    Blocks.get at device.blocks
        |> Maybe.map (Block.get at.t)
        |> Maybe.map focus
        |> Maybe.withDefault NoFocus


currentDevice : Model -> Device
currentDevice model =
    case model.devices |> List.drop model.device |> List.head of
        Just device ->
            device

        Nothing ->
            Device.empty "Device not found"


view : Model -> Html Msg
view model =
    viewDevice model (currentDevice model)


viewDevice : Model -> Device -> Html Msg
viewDevice model device =
    Svg.svg
        [ Svg.Attributes.width (viewWidth model device)
        , Svg.Attributes.height (viewHeight model device)
        , Svg.Attributes.viewBox (viewBox device)
        , Svg.Attributes.preserveAspectRatio "xMidYMid meet"

        --, Svg.Events.onClick SetNoFocus
        ]
        [ Svg.g [] (Blocks.map (viewBlock device model.focus) device.blocks)
        , Svg.text_
            [ Svg.Attributes.x "20"
            , Svg.Attributes.y "80"
            , Svg.Attributes.class "device"
            ]
            [ Svg.text device.title ]
        , Svg.text_
            [ Svg.Attributes.x "20"
            , Svg.Attributes.y "160"
            ]
            [ Svg.text model.message ]
        ]


viewHeight : Model -> Device -> String
viewHeight model device =
    case model.zoom of
        ActualSize ->
            String.append
                (2.5
                    + (8.5 * toFloat device.height)
                    + 0.5
                    |> String.fromFloat
                )
                "cm"

        FitToPage ->
            "calc(100vh - 4px)"


viewWidth : Model -> Device -> String
viewWidth model device =
    case model.zoom of
        ActualSize ->
            String.append
                ((8.5 * toFloat device.width)
                    + 0.5
                    |> String.fromFloat
                )
                "cm"

        FitToPage ->
            "100vw"


viewBox : Device -> String
viewBox device =
    String.concat
        [ "0 0 "
        , (device.width * 340) + 20 |> String.fromInt
        , " "
        , 100 + (device.height * 340) + 20 |> String.fromInt
        ]


viewBlock : Device -> Focus -> Block -> Html Msg
viewBlock device focus block =
    Svg.g
        [ Svg.Attributes.transform (transformBlock block device)
        , Svg.Attributes.class "block"
        , Svg.Attributes.class (BlockType.toString block.t)
        , Html.Attributes.attribute "tabindex" "0"
        ]
        [ Svg.rect
            [ Svg.Attributes.x "20"
            , Svg.Attributes.y "20"
            , Svg.Attributes.width "320"
            , Svg.Attributes.height "320"
            ]
            []
        , viewInterconnects2 focus
            "C4"
            (InterconnectsIndex.join block InterconnectType.C4)
            block.c4s
            "translate(260, 20)"
            "148"
        , case block.t of
            BlockType.Logic ->
                viewInterconnects2 focus
                    "Local"
                    (InterconnectsIndex.join block InterconnectType.Local)
                    block.locals
                    "translate(100, 20)"
                    "268"

            BlockType.Row ->
                viewInterconnects2 focus
                    "Local"
                    (InterconnectsIndex.join block InterconnectType.Local)
                    block.locals
                    "translate(100, 20)"
                    "188"

            _ ->
                viewInterconnects2 focus
                    "Local"
                    (InterconnectsIndex.join block InterconnectType.Local)
                    block.locals
                    "translate(100, 20)"
                    "108"
        , viewInterconnects2 focus
            "Logic"
            (InterconnectsIndex.join block InterconnectType.Logic)
            block.logics
            "translate(180, 20)"
            "208"
        , viewInterconnects1 focus
            "Input"
            (InterconnectsIndex.join block InterconnectType.Input)
            block.inputs
            "translate(190, 20)"
            "148"
        , viewInterconnects2 focus
            "R4"
            (InterconnectsIndex.join block InterconnectType.R4)
            block.r4s
            "translate(20, 20)"
            "168"
        , Svg.text_
            [ Svg.Attributes.x "180"
            , Svg.Attributes.y "17"
            , Svg.Attributes.class "col"
            ]
            [ block.x |> String.fromInt |> Svg.text ]
        , Svg.g
            [ Svg.Attributes.transform "translate(17, 180)"
            ]
            [ Svg.text_
                [ Svg.Attributes.class "row"
                ]
                [ block.y |> String.fromInt |> Svg.text ]
            ]
        ]


transformBlock : Block -> Device -> String
transformBlock block device =
    String.concat
        [ "translate("
        , 340 * (block.x - device.left) |> String.fromInt
        , ","
        , 100 + (340 * (device.top - block.y)) |> String.fromInt
        , ")"
        ]


viewInterconnects2 :
    Focus
    -> String
    -> InterconnectsIndex
    -> Interconnects
    -> String
    -> String
    -> Html Msg
viewInterconnects2 focus title at set transform height =
    if Interconnects.isEmpty set then
        Svg.g [] []

    else
        let
            rect =
                Svg.rect
                    [ Svg.Attributes.x "16"
                    , Svg.Attributes.y "36"
                    , Svg.Attributes.width "48"
                    , Svg.Attributes.height height

                    --, ( SetInterconnectsFocus at, True )
                    --    |> Json.Decode.succeed
                    --    |> Svg.Events.stopPropagationOn "click"
                    ]
                    []

            text =
                Svg.text_
                    [ Svg.Attributes.x "40"
                    , Svg.Attributes.y "33"
                    ]
                    [ Svg.text title ]

            circles =
                Interconnects.map (viewInterconnect2 at focus) set
        in
        Svg.g
            [ Svg.Attributes.transform transform
            , Svg.Attributes.class "interconnects"
            , Html.Attributes.attribute "tabindex" "0"
            ]
            (rect :: text :: circles)


viewInterconnect2 :
    InterconnectsIndex
    -> Focus
    -> Interconnect
    -> Html Msg
viewInterconnect2 at focus interconnect =
    let
        self =
            InterconnectIndex.join at interconnect.i

        x =
            modBy 2 interconnect.i
                |> (*) 20
                |> (+) 30
                |> String.fromInt

        y =
            interconnect.i
                // 2
                |> (*) 20
                |> (+) 50
                |> String.fromInt

        outer =
            Svg.circle
                [ Svg.Attributes.cx x
                , Svg.Attributes.cy y
                , Svg.Attributes.r "10"
                , Svg.Attributes.class (outerClass focus self)

                --, ( SetInterconnectFocus self, True )
                --    |> Json.Decode.succeed
                --    |> Svg.Events.stopPropagationOn "click"
                ]
                []

        inner =
            Svg.circle
                [ Svg.Attributes.cx x
                , Svg.Attributes.cy y
                , Svg.Attributes.r "6"
                , Svg.Attributes.class (innerClass focus self)

                --, ( SetInterconnectFocus self, True )
                --    |> Json.Decode.succeed
                --    |> Svg.Events.stopPropagationOn "click"
                ]
                []
    in
    Svg.g
        [ Svg.Attributes.class "interconnect"
        , Html.Attributes.attribute "tabindex" "0"
        ]
        [ outer, inner ]


viewInterconnects1 :
    Focus
    -> String
    -> InterconnectsIndex
    -> Interconnects
    -> String
    -> String
    -> Html Msg
viewInterconnects1 focus title at set transform height =
    if Interconnects.isEmpty set then
        Svg.g [] []

    else
        let
            rect =
                Svg.rect
                    [ Svg.Attributes.x "16"
                    , Svg.Attributes.y "36"
                    , Svg.Attributes.width "28"
                    , Svg.Attributes.height height

                    --, ( SetInterconnectsFocus at, True )
                    --    |> Json.Decode.succeed
                    --    |> Svg.Events.stopPropagationOn "click"
                    ]
                    []

            text =
                Svg.text_
                    [ Svg.Attributes.x "30"
                    , Svg.Attributes.y "33"
                    ]
                    [ Svg.text title ]

            circles =
                Interconnects.map (viewInterconnect1 at focus) set
        in
        Svg.g
            [ Svg.Attributes.transform transform
            , Svg.Attributes.class "interconnects"
            , Html.Attributes.attribute "tabindex" "0"
            ]
            (rect :: text :: circles)


viewInterconnect1 :
    InterconnectsIndex
    -> Focus
    -> Interconnect
    -> Html Msg
viewInterconnect1 at focus interconnect =
    let
        self =
            InterconnectIndex.join at interconnect.i

        y =
            interconnect.i
                |> (*) 20
                |> (+) 50
                |> String.fromInt

        outer =
            Svg.circle
                [ Svg.Attributes.cx "30"
                , Svg.Attributes.cy y
                , Svg.Attributes.r "10"
                , Svg.Attributes.class (outerClass focus self)

                --, ( SetInterconnectFocus self, True )
                --    |> Json.Decode.succeed
                --    |> Svg.Events.stopPropagationOn "click"
                ]
                []

        inner =
            Svg.circle
                [ Svg.Attributes.cx "30"
                , Svg.Attributes.cy y
                , Svg.Attributes.r "6"
                , Svg.Attributes.class (innerClass focus self)

                --, ( SetInterconnectFocus self, True )
                --    |> Json.Decode.succeed
                --    |> Svg.Events.stopPropagationOn "click"
                ]
                []
    in
    Svg.g
        [ Svg.Attributes.class "interconnect"
        , Html.Attributes.attribute "tabindex" "0"
        ]
        [ outer, inner ]


outerClass : Focus -> InterconnectIndex -> String
outerClass focus self =
    case focus of
        InterconnectFocus { at, froms, thrus } ->
            if at == self then
                "focus"

            else if InterconnectIndexSet.member self froms then
                "from"

            else if InterconnectIndexSet.member self thrus then
                "clear"

            else
                "outer"

        InterconnectsFocus { at, froms, thrus } ->
            if InterconnectsIndex.member self at then
                "focus"

            else if InterconnectIndexSet.member self froms then
                "from"

            else if InterconnectIndexSet.member self thrus then
                "clear"

            else
                "outer"

        NoFocus ->
            "outer"


innerClass : Focus -> InterconnectIndex -> String
innerClass focus self =
    case focus of
        InterconnectFocus { at, thrus } ->
            if at == self then
                "focus"

            else if InterconnectIndexSet.member self thrus then
                "thru"

            else
                "inner"

        InterconnectsFocus { at, thrus } ->
            if InterconnectsIndex.member self at then
                "focus"

            else if InterconnectIndexSet.member self thrus then
                "thru"

            else
                "inner"

        NoFocus ->
            "inner"
