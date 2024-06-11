module Main exposing (main)

import Block exposing (Block)
import BlockIndex exposing (BlockIndex)
import BlockType
import Blocks
import Browser
import Browser.Dom
import Browser.Events
import Device exposing (Device)
import Dict
import Html exposing (Html)
import Html.Attributes
import Html.Events
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
import Task


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
    = BlockFocus
        { at : BlockIndex
        , froms : InterconnectIndexSet
        , thrus : InterconnectIndexSet
        }
    | InterconnectsFocus
        { at : InterconnectsIndex
        , froms : InterconnectIndexSet
        , thrus : InterconnectIndexSet
        }
    | InterconnectFocus
        { at : InterconnectIndex
        , froms : InterconnectIndexSet
        , thrus : InterconnectIndexSet
        }
    | NoFocus


type Direction
    = Left
    | Right
    | Up
    | Down


type alias Model =
    { device : Int
    , devices : List Device
    , focus : Focus
    , message : String
    , zoom : Zoom
    }


type Msg
    = Ignore
    | NextDevice
    | NextZoom
    | Move Bool Direction
    | SetBlockFocus BlockIndex
    | SetInterconnectsFocus InterconnectsIndex
    | SetInterconnectFocus InterconnectIndex
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
        Ignore ->
            ( model, Cmd.none )

        NextDevice ->
            ( { model
                | device = modBy (List.length model.devices) (model.device + 1)
                , focus = NoFocus
              }
            , Cmd.none
            )

        NextZoom ->
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

        Move True Up ->
            move blockUp blockUp blockUp model

        Move True Down ->
            move blockDown blockDown blockDown model

        Move True Left ->
            move blockLeft blockLeft blockLeft model

        Move True Right ->
            move blockRight blockRight blockRight model

        Move _ Up ->
            move blockUp blockUp interconnectUp model

        Move _ Down ->
            move blockDown blockDown interconnectDown model

        Move _ Left ->
            move blockLeft blockLeft interconnectLeft model

        Move _ Right ->
            move blockRight blockRight interconnectRight model

        SetBlockFocus at ->
            ( { model | focus = blockFocus at model }, Cmd.none )

        SetInterconnectsFocus at ->
            ( { model | focus = interconnectsFocus at model }, Cmd.none )

        SetInterconnectFocus at ->
            ( { model | focus = interconnectFocus at model }, Cmd.none )

        SetNoFocus ->
            ( { model | focus = NoFocus }, Cmd.none )


moveTo : String -> Model -> ( Model, Cmd Msg )
moveTo id model =
    ( model
    , Task.attempt (\_ -> Ignore) (Browser.Dom.focus id)
    )


move :
    (BlockIndex -> BlockIndex)
    -> (InterconnectsIndex -> InterconnectsIndex)
    -> (InterconnectIndex -> InterconnectIndex)
    -> Model
    -> ( Model, Cmd Msg )
move nextBlock nextInterconnects nextInterconnect model =
    case model.focus of
        BlockFocus { at } ->
            let
                next =
                    nextBlock at
            in
            case findBlock next model of
                Nothing ->
                    ( model, Cmd.none )

                Just _ ->
                    moveTo (BlockIndex.id next) model

        InterconnectsFocus { at } ->
            let
                next =
                    nextInterconnects at
            in
            case findInterconnects next model of
                Nothing ->
                    ( model, Cmd.none )

                Just _ ->
                    moveTo (InterconnectsIndex.id next) model

        InterconnectFocus { at } ->
            let
                next =
                    nextInterconnect at
            in
            case findInterconnect next model of
                Nothing ->
                    ( model, Cmd.none )

                Just _ ->
                    moveTo (InterconnectIndex.id next) model

        _ ->
            ( model, Cmd.none )


type alias BlockIndexed a =
    { a | x : Int, y : Int }


blockUp : BlockIndexed a -> BlockIndexed a
blockUp block =
    { block | y = block.y + 1 }


blockDown : BlockIndexed a -> BlockIndexed a
blockDown block =
    { block | y = block.y - 1 }


blockLeft : BlockIndexed a -> BlockIndexed a
blockLeft block =
    { block | x = block.x - 1 }


blockRight : BlockIndexed a -> BlockIndexed a
blockRight block =
    { block | x = block.x + 1 }


interconnectUp : InterconnectIndex -> InterconnectIndex
interconnectUp { x, y, t, i } =
    { x = x, y = y, t = t, i = i - 2 }


interconnectDown : InterconnectIndex -> InterconnectIndex
interconnectDown { x, y, t, i } =
    { x = x, y = y, t = t, i = i + 2 }


interconnectLeft : InterconnectIndex -> InterconnectIndex
interconnectLeft { x, y, t, i } =
    { x = x, y = y, t = t, i = i - 1 }


interconnectRight : InterconnectIndex -> InterconnectIndex
interconnectRight { x, y, t, i } =
    { x = x, y = y, t = t, i = i + 1 }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyPress keyPress


keyPress : Json.Decode.Decoder Msg
keyPress =
    let
        decode key =
            case key of
                "d" ->
                    NextDevice

                "z" ->
                    NextZoom

                _ ->
                    Ignore
    in
    Json.Decode.map decode (Json.Decode.field "key" Json.Decode.string)


keyDown : Json.Decode.Decoder ( Msg, Bool )
keyDown =
    let
        decode ctrl key =
            --case Debug.log "KeyDown" key of
            case key of
                "ArrowLeft" ->
                    ( Move ctrl Left, True )

                "ArrowRight" ->
                    ( Move ctrl Right, True )

                "ArrowUp" ->
                    ( Move ctrl Up, True )

                "ArrowDown" ->
                    ( Move ctrl Down, True )

                _ ->
                    ( Ignore, False )
    in
    Json.Decode.map2 decode
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "key" Json.Decode.string)


findBlock : BlockIndex -> Model -> Maybe Block
findBlock at model =
    let
        device =
            currentDevice model
    in
    Blocks.get at device.blocks


findInterconnects : InterconnectsIndex -> Model -> Maybe Interconnects
findInterconnects at model =
    let
        device =
            currentDevice model
    in
    Blocks.get at device.blocks
        |> Maybe.map (Block.get at.t)


findInterconnect : InterconnectIndex -> Model -> Maybe Interconnect
findInterconnect at model =
    let
        device =
            currentDevice model
    in
    Blocks.get at device.blocks
        |> Maybe.map (Block.get at.t)
        |> Maybe.andThen (Interconnects.get at.i)


blockFocus : BlockIndex -> Model -> Focus
blockFocus at model =
    case findBlock at model of
        Nothing ->
            NoFocus

        Just block ->
            BlockFocus
                { at = at
                , froms = Block.froms block
                , thrus = Block.thrus block
                }


interconnectsFocus : InterconnectsIndex -> Model -> Focus
interconnectsFocus at model =
    case findInterconnects at model of
        Nothing ->
            NoFocus

        Just interconnects ->
            InterconnectsFocus
                { at = at
                , froms = Interconnects.froms interconnects
                , thrus = Interconnects.thrus interconnects
                }


interconnectFocus : InterconnectIndex -> Model -> Focus
interconnectFocus at model =
    case findInterconnect at model of
        Nothing ->
            NoFocus

        Just interconnect ->
            InterconnectFocus
                { at = at
                , froms = interconnect.froms
                , thrus = interconnect.thrus
                }


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
        , Html.Events.onBlur SetNoFocus
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
    let
        at =
            BlockIndex.from block

        rect =
            Svg.rect
                [ Svg.Attributes.x "20"
                , Svg.Attributes.y "20"
                , Svg.Attributes.width "320"
                , Svg.Attributes.height "320"
                ]
                []

        r4 =
            viewInterconnects2 focus
                "R4"
                (InterconnectsIndex.join block InterconnectType.R4)
                block.r4s
                "translate(20, 20)"
                "168"

        local =
            case block.t of
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

        logic =
            viewInterconnects2 focus
                "Logic"
                (InterconnectsIndex.join block InterconnectType.Logic)
                block.logics
                "translate(180, 20)"
                "208"

        input =
            viewInterconnects1 focus
                "Input"
                (InterconnectsIndex.join block InterconnectType.Input)
                block.inputs
                "translate(190, 20)"
                "148"

        c4 =
            viewInterconnects2 focus
                "C4"
                (InterconnectsIndex.join block InterconnectType.C4)
                block.c4s
                "translate(260, 20)"
                "148"

        column =
            Svg.text_
                [ Svg.Attributes.x "180"
                , Svg.Attributes.y "17"
                , Svg.Attributes.class "col"
                ]
                [ block.x |> String.fromInt |> Svg.text ]

        row =
            Svg.g
                [ Svg.Attributes.transform "translate(17, 180)"
                ]
                [ Svg.text_
                    [ Svg.Attributes.class "row"
                    ]
                    [ block.y |> String.fromInt |> Svg.text ]
                ]
    in
    Svg.g
        [ Svg.Attributes.transform (transformBlock block device)
        , Svg.Attributes.class "block"
        , Svg.Attributes.class (BlockType.toString block.t)
        , BlockIndex.id at
            |> Html.Attributes.id
        , Html.Attributes.attribute "tabindex" "0"
        , Html.Events.onFocus (SetBlockFocus at)
        , Html.Events.preventDefaultOn "keydown" keyDown
        ]
        [ rect
        , r4
        , local
        , logic
        , input
        , c4
        , column
        , row
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
            , InterconnectsIndex.id at
                |> Html.Attributes.id
            , Html.Attributes.attribute "tabindex" "0"
            , Html.Events.onFocus (SetInterconnectsFocus at)
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
                ]
                []

        inner =
            Svg.circle
                [ Svg.Attributes.cx x
                , Svg.Attributes.cy y
                , Svg.Attributes.r "6"
                , Svg.Attributes.class (innerClass focus self)
                ]
                []
    in
    Svg.g
        [ Svg.Attributes.class "interconnect"
        , InterconnectIndex.id self
            |> Html.Attributes.id
        , Html.Attributes.attribute "tabindex" "0"
        , Html.Events.onFocus (SetInterconnectFocus self)
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
            , InterconnectsIndex.id at
                |> Html.Attributes.id
            , Html.Attributes.attribute "tabindex" "0"
            , Html.Events.onFocus (SetInterconnectsFocus at)
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
                ]
                []

        inner =
            Svg.circle
                [ Svg.Attributes.cx "30"
                , Svg.Attributes.cy y
                , Svg.Attributes.r "6"
                , Svg.Attributes.class (innerClass focus self)
                ]
                []
    in
    Svg.g
        [ Svg.Attributes.class "interconnect"
        , InterconnectIndex.id self
            |> Html.Attributes.id
        , Html.Attributes.attribute "tabindex" "0"
        , Html.Events.onFocus (SetInterconnectFocus self)
        ]
        [ outer, inner ]


outerClass : Focus -> InterconnectIndex -> String
outerClass focus self =
    case focus of
        BlockFocus { froms, thrus } ->
            if InterconnectIndexSet.member self froms then
                "from"

            else if InterconnectIndexSet.member self thrus then
                "clear"

            else
                "outer"

        InterconnectsFocus { froms, thrus } ->
            if InterconnectIndexSet.member self froms then
                "from"

            else if InterconnectIndexSet.member self thrus then
                "clear"

            else
                "outer"

        InterconnectFocus { froms, thrus } ->
            if InterconnectIndexSet.member self froms then
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
        BlockFocus { thrus } ->
            if InterconnectIndexSet.member self thrus then
                "thru"

            else
                "inner"

        InterconnectsFocus { thrus } ->
            if InterconnectIndexSet.member self thrus then
                "thru"

            else
                "inner"

        InterconnectFocus { at, thrus } ->
            if InterconnectIndexSet.member self thrus then
                "thru"

            else
                "inner"

        NoFocus ->
            "inner"
