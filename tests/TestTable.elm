module TestTable exposing (..)

import Expect
import Fuzz exposing (..)
import Html
import List.Nonempty as NonEmptyList exposing (Nonempty)
import Ordering
import Rudder.Filters as Filters exposing (SearchFilterState, byValues, getTextValue)
import Rudder.Table as RudderTable exposing (..)
import Test exposing (..)


type alias Model =
    RudderTable.Model String ()


filterFuzz : Fuzzer SearchFilterState
filterFuzz =
    oneOf [ constant Filters.empty, substringFilterFuzz ]


substringFilterFuzz : Fuzzer SearchFilterState
substringFilterFuzz =
    -- Filter without space, because filter is trimmed
    map Filters.substring (map String.fromList (list (intRange 33 126 |> map Char.fromCode)))


nonEmptySubstringFilterFuzz : Fuzzer SearchFilterState
nonEmptySubstringFilterFuzz =
    map Filters.substring (map String.fromList (listOfLengthBetween 1 32 (intRange 33 126 |> map Char.fromCode)))


columnNameFuzz : Fuzzer ColumnName
columnNameFuzz =
    map ColumnName string


columnFuzz : Fuzzer (Column String ())
columnFuzz =
    constant Column
        |> andMap columnNameFuzz
        |> andMap (constant (\_ -> Html.text ""))
        |> andMap (constant Ordering.natural)


nonEmptyListFuzzer : Fuzzer a -> Fuzzer (NonEmptyList.Nonempty a)
nonEmptyListFuzzer aFuzz =
    map2 NonEmptyList.Nonempty aFuzz (list aFuzz)


sortOrderFuzz : Fuzzer SortOrder
sortOrderFuzz =
    oneOfValues [ Asc, Desc ]


optionsFuzz : SearchFilterState -> Fuzzer (Options String ())
optionsFuzz filter =
    constant
        (buildOptions.newOptions
            |> buildOptions.withStorage { key = "", saveToLocalStoragePort = \_ -> Cmd.none }
            |> buildOptions.withFilter (SearchInputFilter { predicate = byValues (\d -> [ d ]), state = filter })
        )


filterModelFuzz : SearchFilterState -> Fuzzer Model
filterModelFuzz filter =
    let
        config =
            constant Config
                |> andMap (nonEmptyListFuzzer columnFuzz)
                |> andMap columnNameFuzz
                |> andMap sortOrderFuzz
                |> andMap (optionsFuzz filter)
    in
    map2 RudderTable.init config dataFuzzer


sortModelFuzz : ColumnName -> SortOrder -> Fuzzer Model
sortModelFuzz sortBy sortOrder =
    let
        config =
            constant Config
                |> andMap (constant (NonEmptyList.singleton { name = sortBy, renderHtml = \_ -> Html.text "", ordering = Ordering.natural }))
                |> andMap (constant sortBy)
                |> andMap (constant sortOrder)
                |> andMap (optionsFuzz Filters.empty)
    in
    map2 RudderTable.init config dataFuzzer


csvColumnFuzz : NonEmptyList.Nonempty String -> Fuzzer (NonEmptyList.Nonempty (Column row msg))
csvColumnFuzz (NonEmptyList.Nonempty head tail) =
    let
        colNameToColumnFuzz =
            \colName ->
                constant Column
                    |> andMap (constant (ColumnName colName))
                    |> andMap (constant (\_ -> Html.text ""))
                    |> andMap (constant (\_ _ -> LT))
    in
    map2 NonEmptyList.Nonempty
        (head |> colNameToColumnFuzz)
        (sequence (List.map colNameToColumnFuzz tail))


csvModelFuzz : NonEmptyList.Nonempty String -> (row -> List String) -> List row -> Fuzzer (RudderTable.Model row msg)
csvModelFuzz columnNames entryToStringList data =
    let
        options =
            buildOptions.newOptions
                |> buildOptions.withCsvExport
                    { entryToStringList = entryToStringList
                    , btnAttributes = []
                    }

        config =
            constant Config
                |> andMap (csvColumnFuzz columnNames)
                |> andMap (constant (NonEmptyList.head columnNames |> ColumnName))
                |> andMap sortOrderFuzz
                |> andMap (constant options)
    in
    map2 RudderTable.init config (constant data)


applyFilterFuzz : Fuzzer ((String -> Bool) -> String -> Bool)
applyFilterFuzz =
    constant identity


dataFuzzer : Fuzzer (List String)
dataFuzzer =
    list rowFuzzer


rowFuzzer : Fuzzer String
rowFuzzer =
    string


suite : Test
suite =
    let
        model ( a, _, _ ) =
            a

        effect ( _, a, _ ) =
            a

        stubFilter =
            Filters.substring "test"

        stubColumnName =
            ColumnName "column"
    in
    describe "Rudder.Table"
        [ describe "filters"
            [ fuzz (filterModelFuzz Filters.empty) "test filter update on table that defines a configuration to save in localStorage" <|
                \m ->
                    let
                        options =
                            storageOptions m

                        expectation =
                            options
                                |> Maybe.map (\{ key, saveToLocalStoragePort } -> Expect.equal [ SaveFilterInLocalStorage key stubFilter saveToLocalStoragePort ])
                                |> Maybe.withDefault (\_ -> Expect.fail "initial model was not configured to save to local storage, please fix the stub fuzz")
                    in
                    updateWithEffect (updateFilter stubFilter) m |> effect |> expectation
            , fuzz (filterModelFuzz Filters.empty) "test filter on table that got updated with a new filter" <|
                \m -> updateWithEffect (updateFilter stubFilter) m |> model |> getFilterOptionValue |> Expect.equal "test"
            , fuzz3 (filterModelFuzz Filters.empty) dataFuzzer substringFilterFuzz "test filter on table when filter matches all entries" <|
                \m d f ->
                    let
                        dataLen =
                            List.length d

                        filterText =
                            getTextValue f

                        -- each data contains the filter string so filtered data is the same
                        data =
                            List.map (String.append filterText) d
                    in
                    m
                        |> updateData data
                        |> updateWithEffect (updateFilter f)
                        |> model
                        |> getRows
                        |> List.length
                        |> Expect.equal dataLen
            , fuzz3 (filterModelFuzz Filters.empty) dataFuzzer nonEmptySubstringFilterFuzz "test empty filter on table after applying filter that doesn't match some entries" <|
                \m d f ->
                    let
                        filterText =
                            getTextValue f

                        data =
                            List.filter (\s -> not (String.contains (String.toLower filterText) (String.toLower s)) && (s /= "")) d

                        dataLen =
                            List.length data

                        firstFilter =
                            m
                                |> updateData data
                                |> updateWithEffect (updateFilter f)
                                |> model

                        secondFilter =
                            firstFilter
                                |> updateWithEffect (updateFilter Filters.empty)
                                |> model

                        firstFilterLen =
                            firstFilter |> getRows |> List.length

                        secondFilterLen =
                            secondFilter |> getRows |> List.length
                    in
                    ( firstFilterLen, secondFilterLen ) |> Expect.equal ( 0, dataLen )
            ]
        , describe "sort"
            [ fuzz (sortModelFuzz stubColumnName Asc) "test sorting on table when toggling already sorted column with Asc" <|
                \m -> updateWithEffect (sortColumn stubColumnName) m |> model |> getSort |> Expect.equal ( stubColumnName, Desc )
            , fuzz (sortModelFuzz stubColumnName Desc) "test sorting on table when toggling already sorted column with Desc" <|
                \m -> updateWithEffect (sortColumn stubColumnName) m |> model |> getSort |> Expect.equal ( stubColumnName, Asc )
            , fuzz2 (sortModelFuzz stubColumnName Asc) columnNameFuzz "test sorting on table when applying a column to sort with Asc order" <|
                \m c -> updateWithEffect (sortColumn c) m |> model |> getSort |> Expect.equal ( c, Asc )
            , fuzz2 (sortModelFuzz stubColumnName Asc) dataFuzzer "test getRows on table that was sorted with Asc order" <|
                \m data ->
                    m
                        |> updateData data
                        |> updateWithEffect (sortColumn stubColumnName)
                        |> model
                        |> getRows
                        |> Expect.equalLists (List.reverse (List.sort data))
            ]
        , describe "csv export"
            [ fuzz (nonEmptyListFuzzer columnFuzz) "test csv export on empty table that does not define a csv export configuration" <|
                \c ->
                    init (buildConfig.newConfig c) []
                        |> updateWithEffect (exportCsv ExportFiltered "filename")
                        |> effect
                        |> Expect.equal [ IgnoreExportCsvMsgNoConfig ]
            , fuzz
                (csvModelFuzz (NonEmptyList.Nonempty "name" []) (\row -> row) [])
                "test csv export on empty table that defines a csv export configuration"
              <|
                \m ->
                    m
                        |> updateWithEffect (exportCsv ExportFiltered "myFile")
                        |> effect
                        |> Expect.equal [ DownloadTableAsCsv (CsvExportData "myFile" "") ]
            , fuzz
                (csvModelFuzz
                    (NonEmptyList.Nonempty "name" [ "age" ])
                    (\{ a, b } -> [ a, String.fromInt b ])
                    [ { a = "Alice", b = 45 }, { a = "Bob", b = 37 } ]
                )
                "test csv export on non-empty table that defines a csv export configuration"
              <|
                \m ->
                    m
                        |> updateWithEffect (exportCsv ExportFiltered "otherFile")
                        |> effect
                        |> Expect.equal [ DownloadTableAsCsv (CsvExportData "otherFile" "name,age\u{000D}\nAlice,45\u{000D}\nBob,37") ]
            , fuzz
                (csvModelFuzz
                    (NonEmptyList.Nonempty "\"Name\"" [ "Age,probably" ])
                    (\{ a, b } -> [ a, String.fromInt b ])
                    [ { a = "\"Al\"ice\"", b = 45 }
                    , { a = "Bo,b", b = 37 }
                    , { a = "\nEve", b = 28 }
                    ]
                )
                "test csv export on non-empty table that defines a csv export configuration, and that contains column names and fields with special characters"
              <|
                \m ->
                    m
                        |> updateWithEffect (exportCsv ExportFiltered "yetAnotherFile")
                        |> effect
                        |> Expect.equal [ DownloadTableAsCsv (CsvExportData "yetAnotherFile" "\"\"\"Name\"\"\",\"Age,probably\"\u{000D}\n\"\"\"Al\"\"ice\"\"\",45\u{000D}\n\"Bo,b\",37\u{000D}\n\"\nEve\",28") ]
            ]
        ]
