module ProgramTestTable exposing (suite)

import Browser
import Expect
import Html exposing (Html)
import Html.Attributes as Attributes
import List.Nonempty as NonEmpty
import Ordering exposing (Ordering)
import ProgramTest exposing (ProgramTest, clickButton, ensureViewHasNot, expectLastEffect, expectViewHas, expectViewHasNot, fillIn, start, withBaseUrl)
import Rudder.Filters as Filters exposing (byValues)
import Rudder.Table as Table exposing (Column, ColumnName(..), Config, Effect(..), FilterOptionsType(..), Model, Msg, Options, OutMsg(..), SortOrder(..), buildOptions, emptyMsg)
import Test exposing (..)
import Test.Html.Selector exposing (all, attribute, class, classes, containing, exactText, tag, text)
import Url exposing (Url)


type alias TestResult msg =
    ( Cmd msg, Maybe (OutMsg msg) )


initElement : List row -> Config row msg -> flags -> ( Model row msg, TestResult msg )
initElement data config _ =
    ( Table.init config data, ( Cmd.none, Nothing ) )


testUpdate : Msg msg -> Model row msg -> ( Model row msg, TestResult msg )
testUpdate msg model =
    let
        ( updatedModel, cmd, outMsg ) =
            Table.update msg model
    in
    ( updatedModel, ( cmd, outMsg ) )


type alias TestEffect parentMsg =
    ( List (Effect parentMsg), Maybe (OutMsg parentMsg) )


initElementWithEffect : List row -> Config row msg -> fags -> ( Model row msg, TestEffect msg )
initElementWithEffect data config _ =
    ( Table.init config data, ( [], Nothing ) )


testInitWithEffect : List row -> Config row msg -> flags -> Url -> () -> ( Model row msg, TestEffect msg )
testInitWithEffect data config flags _ _ =
    initElementWithEffect data config flags


testUpdateWithEffect : Msg msg -> Model row msg -> ( Model row msg, TestEffect msg )
testUpdateWithEffect msg model =
    let
        ( updatedModel, effect, outMsg ) =
            Table.updateWithEffect msg model
    in
    ( updatedModel, ( effect, outMsg ) )


start : List row -> Config row msg -> ProgramTest (Model row msg) (Msg msg) (TestEffect msg)
start data config =
    ProgramTest.createApplication
        { onUrlChange = \_ -> emptyMsg
        , onUrlRequest = \_ -> emptyMsg
        , init = testInitWithEffect data config
        , update = testUpdateWithEffect
        , view = Table.view >> (\view -> { title = "", body = [ view ] })
        }
        |> withBaseUrl "http://localhost:8080/rudder/"
        |> ProgramTest.start []


testConfig : NonEmpty.Nonempty String -> Ordering row -> (row -> Html msg) -> Options row msg -> Config row msg
testConfig columnNames ordering renderHtml options =
    let
        toColumn =
            \columnName ->
                { name = ColumnName columnName
                , renderHtml = renderHtml
                , ordering = ordering
                }
    in
    { columns = columnNames |> NonEmpty.map toColumn
    , sortBy = NonEmpty.head columnNames |> ColumnName
    , sortOrder = Asc
    , options = options
    }


mockConfig : Options row msg -> Config row msg
mockConfig options =
    let
        colNames =
            NonEmpty.Nonempty "" []

        ordering =
            --fixme
            \_ -> \_ -> LT

        renderHtml =
            \_ -> Html.text ""
    in
    testConfig colNames ordering renderHtml options


mockCsvConfig : Config row msg
mockCsvConfig =
    let
        options =
            buildOptions.newOptions
                --fixme
                |> buildOptions.withCsvExport { entryToStringList = \_ -> [], btnAttributes = [] }
    in
    mockConfig options


mockFilterConfig : Config String msg
mockFilterConfig =
    let
        options =
            buildOptions.newOptions
                |> buildOptions.withFilter (SearchInputFilter { predicate = byValues (\d -> [ d ]), state = Filters.empty })
    in
    mockConfig options


mockRefreshConfig : Config row msg
mockRefreshConfig =
    let
        options =
            buildOptions.newOptions
                |> buildOptions.withRefresh []
    in
    mockConfig options


mockData : List { a : String, b : Int }
mockData =
    [ { a = "Alice", b = 45 }
    , { a = "Bob", b = 37 }
    , { a = "Eve", b = 28 }
    ]


mockFilterConfigWithData : Config { a : String, b : Int } msg
mockFilterConfigWithData =
    let
        columns : NonEmpty.Nonempty (Column { a : String, b : Int } msg)
        columns =
            NonEmpty.Nonempty
                { name = ColumnName "Name", renderHtml = .a >> Html.text, ordering = Ordering.byField .a }
                [ { name = ColumnName "Age", renderHtml = .b >> String.fromInt >> Html.text, ordering = Ordering.byField .b } ]

        options =
            buildOptions.newOptions
                |> buildOptions.withFilter (SearchInputFilter { predicate = byValues (\{ a, b } -> [ a, String.fromInt b ]), state = Filters.empty })
    in
    { columns = columns
    , sortBy = NonEmpty.head columns |> .name
    , sortOrder = Asc
    , options = options
    }


suite : Test
suite =
    describe "RudderTableProgram"
        [ describe "csv export"
            [ test "if the csv export option is enabled, the \"Export\" button should be displayed" <|
                \() ->
                    start [] mockCsvConfig
                        |> expectViewHas
                            [ all [ tag "button", containing [ tag "span", containing [ text "Export" ] ] ] ]
            , test "if the csv export option is disabled, the \"Export\" button should not be displayed" <|
                \() ->
                    start [] mockFilterConfig
                        |> expectViewHasNot
                            [ all [ tag "button", containing [ tag "span", containing [ text "Export" ] ] ]
                            ]
            , test "if the export button is clicked, a CsvExportRequested msg should be created" <|
                \() ->
                    let
                        expectedEffect : TestEffect msg
                        expectedEffect =
                            ( [], Just CsvExportRequested )
                    in
                    ProgramTest.createElement
                        { init = initElementWithEffect [] mockCsvConfig
                        , update = testUpdateWithEffect
                        , view = Table.view
                        }
                        |> ProgramTest.start ()
                        |> clickButton "Export CSV"
                        |> expectLastEffect (\effect -> effect |> Expect.equal expectedEffect)
            ]
        , describe "filters"
            [ test "if the searchfilter option is enabled, the SearchFilter should be displayed" <|
                \() ->
                    start [] mockFilterConfig
                        |> expectViewHas
                            [ all
                                [ tag "input"
                                , class "form-control"
                                , attribute (Attributes.type_ "text")
                                , attribute (Attributes.placeholder "Filter...")
                                , attribute (Attributes.value "")
                                ]
                            ]
            , test "if the searchfilter option is disabled, the SearchFilter should not be displayed" <|
                \() ->
                    start [] mockCsvConfig
                        |> expectViewHasNot
                            [ all
                                [ tag "input"
                                , class "form-control"
                                , attribute (Attributes.type_ "text")
                                , attribute (Attributes.placeholder "Filter...")
                                , attribute (Attributes.value "")
                                , attribute (Attributes.value "")
                                ]
                            ]
            , test "if the content of the searchfilter is empty, all entries should be displayed" <|
                \() ->
                    -- FIXME : this test currently does not check whether each entry appears exactly once, it only ensures that every entry appears
                    start mockData mockFilterConfigWithData
                        |> expectViewHas
                            [ all
                                [ tag "input"
                                , class "form-control"
                                , attribute (Attributes.type_ "text")
                                , attribute (Attributes.placeholder "Filter...")
                                , attribute (Attributes.value "")
                                ]
                            , all
                                [ tag "tbody"
                                , containing
                                    [ tag "tr"
                                    , containing [ tag "td", containing [ exactText "Alice" ] ]
                                    , containing [ tag "td", containing [ exactText "45" ] ]
                                    ]
                                , containing
                                    [ tag "tr"
                                    , containing [ tag "td", containing [ exactText "Bob" ] ]
                                    , containing [ tag "td", containing [ exactText "37" ] ]
                                    ]
                                , containing
                                    [ tag "tr"
                                    , containing [ tag "td", containing [ exactText "Eve" ] ]
                                    , containing [ tag "td", containing [ exactText "28" ] ]
                                    ]
                                ]
                            ]
            , test "if the content of the searchfilter is updated, entries that do not match the filter should be omitted" <|
                \() ->
                    -- FIXME : this test does not check whether the entries that are present appear exactly once
                    start mockData mockFilterConfigWithData
                        |> fillIn "SearchFilter" "Search Filter" "e"
                        |> ensureViewHasNot
                            [ tag "tbody"
                            , containing
                                [ tag "tr"
                                , containing [ tag "td", containing [ exactText "Bob" ] ]
                                , containing [ tag "td", containing [ exactText "37" ] ]
                                ]
                            ]
                        |> expectViewHas
                            [ tag "tbody"
                            , containing
                                [ tag "tr"
                                , containing [ tag "td", containing [ exactText "Alice" ] ]
                                , containing [ tag "td", containing [ exactText "45" ] ]
                                ]
                            , containing
                                [ tag "tr"
                                , containing [ tag "td", containing [ exactText "Eve" ] ]
                                , containing [ tag "td", containing [ exactText "28" ] ]
                                ]
                            ]
            ]
        , describe "refresh"
            [ test "if the refresh option is enabled, the Refresh button should be displayed" <|
                \() ->
                    start [] mockRefreshConfig
                        |> expectViewHas
                            [ all [ tag "button", attribute (Attributes.attribute "aria-label" "Refresh"), containing [ tag "i", classes [ "fa", "fa-refresh" ] ] ] ]
            , test "if the refresh option is disabled, the Refresh button should not be displayed" <|
                \() ->
                    start [] mockCsvConfig
                        |> expectViewHasNot
                            [ all [ tag "button", attribute (Attributes.attribute "aria-label" "Refresh"), containing [ tag "i", classes [ "fa", "fa-refresh" ] ] ] ]
            , test "if the refresh button is clicked, a refresh effect should be created" <|
                \() ->
                    let
                        expectedEffect =
                            ( [], Just RefreshRequested )
                    in
                    ProgramTest.createElement
                        { init = initElementWithEffect [] mockRefreshConfig
                        , update = testUpdateWithEffect
                        , view = Table.view
                        }
                        |> ProgramTest.start ()
                        |> clickButton "Refresh"
                        |> expectLastEffect (\effect -> effect |> Expect.equal expectedEffect)
            ]
        ]
