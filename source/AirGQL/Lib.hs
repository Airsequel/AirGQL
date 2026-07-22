{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use tuple-section" #-}

module AirGQL.Lib (
  AccessMode (canRead, canWrite, canInsert),
  mkAccessMode,
  readOnly,
  writeOnly,
  insertOnly,
  readAndWrite,
  ColumnEntry (..),
  GqlTypeName (..),
  getEnrichedTable,
  getColumnsFromParsedTableEntry,
  getColumns,
  getRowidColumnName,
  getTables,
  getTableNames,
  getColumnNames,
  getEnrichedTables,
  getPKColumns,
  ObjectType (..),
  parseSql,
  replaceCaseInsensitive,
  sanitizeSql,
  sqlDataToAesonValue,
  sqlDataToText,
  SQLPost (..),
  sqlTypeNameToGQLTypeName,
  TableEntryRaw (..),
  TableEntry (..),
  UniqueConstraint (..),
  ReferencesConstraint (..),
  ReferencesConstraintColumns (..),
  CheckConstraint (..),
  sqlite, -- useful for pretty printing
  splitSqlStatements,
  validateStatementSafety,
  readOnlyPragmas,
  stringToGqlTypeName,
  lintTableCreationCode,
  resolveReferencesConstraintColumns,
  resolveReferencesConstraint,
  sqliteErrorToText,
)
where

import Protolude (
  Applicative (pure),
  Bool (False, True),
  Either (Left, Right),
  Eq ((/=), (==)),
  Exception (toException),
  Generic,
  IO,
  Int,
  Maybe (Just, Nothing),
  Semigroup ((<>)),
  Show,
  Text,
  notElem,
  otherwise,
  show,
  ($),
  (&),
  (&&),
  (.),
  (<$>),
  (<&>),
  (>>=),
  (||),
 )
import Protolude qualified as P

import AirGQL.Utils (collectAllErrorsAsText, quoteText)
import Control.Monad (MonadFail (fail))
import Control.Monad.Catch (catchAll)
import Data.Aeson (FromJSON, ToJSON, Value (Bool, Null, Number, Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isSpace)
import Data.List qualified as List
import Data.Scientific qualified as Scientific
import Data.Text (isInfixOf, toUpper)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple (
  Connection,
  FromRow,
  ResultError (ConversionFailed, errHaskellType, errMessage, errSQLType),
  SQLData (SQLBlob, SQLFloat, SQLInteger, SQLNull, SQLText),
  query_,
 )
import Database.SQLite.Simple qualified as SS
import Database.SQLite.Simple.FromField (FromField (fromField), fieldData)
import Database.SQLite.Simple.Ok (Ok (Errors, Ok))
import Database.SQLite.Simple.QQ qualified as SS
import DoubleXEncoding (doubleXEncodeGql)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (CInt))
import Language.SQL.SimpleSQL.Dialect (
  Dialect (
    diAppKeywords,
    diAutoincrement,
    diBackquotedIden,
    diKeywords,
    diLimit,
    diSquareBracketQuotedIden,
    diWithoutRowidTables
  ),
  ansi2011,
 )
import Language.SQL.SimpleSQL.Parse (ParseError, parseStatement, prettyError)
import Language.SQL.SimpleSQL.Pretty (prettyScalarExpr)
import Language.SQL.SimpleSQL.Syntax (
  ColConstraint (ColCheckConstraint, ColNotNullConstraint),
  ColConstraintDef (ColConstraintDef),
  ColumnDef (ColumnDef),
  InPredValue (InList),
  ScalarExpr (In, NumLit, StringLit),
  Statement (CreateTable),
  TableElement (TableColumnDef),
 )
import Language.SQL.SimpleSQL.Syntax qualified as SQL
import Servant.Docs (ToSample (toSamples), singleSample)


data AccessMode = AccessMode
  { canRead :: Bool
  , canInsert :: Bool
  , canWrite :: Bool
  }
  deriving (Eq, Show)


mkAccessMode :: Bool -> Bool -> Bool -> AccessMode
mkAccessMode read insert write =
  AccessMode
    { canRead = read
    , canInsert = insert || write
    , canWrite = write
    }


readOnly :: AccessMode
readOnly = mkAccessMode True False False


writeOnly :: AccessMode
writeOnly = mkAccessMode False True True


insertOnly :: AccessMode
insertOnly = mkAccessMode False True False


readAndWrite :: AccessMode
readAndWrite = mkAccessMode True True True


data ObjectType = Table | Index | View | Trigger
  deriving (Show, Eq, Generic)


instance ToJSON ObjectType


instance FromJSON ObjectType


instance FromField ObjectType where
  fromField fData = case fieldData fData of
    SQLText "table" -> Ok Table
    SQLText "index" -> Ok Index
    SQLText "view" -> Ok View
    SQLText "trigger" -> Ok Trigger
    sqlData ->
      Errors
        [ toException $
            ConversionFailed
              { errSQLType = "Object Type"
              , errHaskellType = "String"
              , errMessage =
                  "\"" <> show sqlData <> "\" is not a valid object type"
              }
        ]


data TableEntryRaw = TableEntryRaw
  { name :: Text
  , tbl_name :: Text
  , object_type :: ObjectType
  , rootpage :: Int
  , sql :: Text
  }
  deriving (Show, Eq, Generic)


instance ToJSON TableEntryRaw
instance FromRow TableEntryRaw


data PrimaryKeyConstraint = PrimaryKeyConstraint
  { name :: Maybe Text
  , columns :: [Text]
  }
  deriving (Show, Eq, Generic)


data UniqueConstraint = UniqueConstraint
  { name :: Maybe Text
  , columns :: [Text]
  }
  deriving (Show, Eq, Generic)


instance ToJSON UniqueConstraint


data ReferencesConstraintColumns
  = {-| The "to" column is implicit.
    Eg: `a TEXT REFERENCES other_table`
    -}
    ImplicitColumns Text
  | -- | Explicit (from, to) pairs
    ExplicitColumns [(Text, Text)]
  deriving (Show, Eq, Generic)


instance ToJSON ReferencesConstraintColumns


data ReferencesConstraint = ReferencesConstraint
  { name :: Maybe Text
  , table :: Text
  , columns :: ReferencesConstraintColumns
  }
  deriving (Show, Eq, Generic)


instance ToJSON ReferencesConstraint


data CheckConstraint = CheckConstraint
  { name :: Maybe Text
  , predicate :: Text
  , columns :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)


instance ToJSON CheckConstraint


data TableEntry = TableEntry
  { name :: Text
  , tbl_name :: Text
  , object_type :: ObjectType
  , rootpage :: Int
  , sql :: Text
  , statement :: Statement
  , uniqueConstraints :: [UniqueConstraint]
  , referencesConstraints :: [ReferencesConstraint]
  , checkConstraints :: [CheckConstraint]
  , primaryKeyConstraints :: [PrimaryKeyConstraint]
  , columns :: [ColumnEntry]
  }
  deriving (Show, Eq, Generic)


-- | As requested from SQLite
data ColumnEntryRaw = ColumnEntryRaw
  { cid :: Int
  , column_name :: Text
  , datatype :: Text
  , notnull :: Int -- TODO: Should be boolean
  , dflt_value :: Maybe Text
  , primary_key :: Int -- TODO: Should be boolean
  , -- See the docs for the different meanings:
    -- https://www.sqlite.org/pragma.html#pragma_table_xinfo
    -- - 0 means normal
    -- - 1 means hidden column in a virtual table
    -- - 2 and 3 mean generated columns
    hidden :: Int
  }
  deriving (Show, Eq, Generic)


instance FromRow ColumnEntryRaw


data GqlTypeName = GqlTypeName
  { root :: Text
  , full :: Text
  }
  deriving (Show, Eq, Generic)


instance ToJSON GqlTypeName


-- | Enhanced with generated information from SQL query "CREATE TABLE"
data ColumnEntry = ColumnEntry
  { column_name :: Text
  , column_name_gql :: Text
  , datatype :: Text
  -- ^ double-X-encoded GQL identifiers
  , datatype_gql :: Maybe GqlTypeName
  , select_options :: Maybe [Text]
  , notnull :: Bool
  , isRowid :: Bool
  , isGenerated :: Bool
  , isUnique :: Bool
  , isOmittable :: Bool
  -- ^ If column is NON NULL, but will be set automatically
  , isReference :: Bool
  -- ^ This will be `true` when the column is part of some REFERENCES constraint.
  , dflt_value :: Maybe Text
  , primary_key :: Bool
  }
  deriving (Show, Eq, Generic)


instance ToJSON ColumnEntry


data ParsedTable = ParsedTable
  { uniqueConstraints :: [UniqueConstraint]
  , referencesConstraints :: [ReferencesConstraint]
  , checkConstraints :: [CheckConstraint]
  , primaryKeyConstraints :: [PrimaryKeyConstraint]
  , statement :: Statement
  }
  deriving (Show, Eq, Generic)


getTables :: Connection -> IO [TableEntryRaw]
getTables connection = do
  query_
    connection
    [SS.sql|
      SELECT name, tbl_name, type, rootpage, sql
      FROM sqlite_master
      WHERE
        type == 'table' OR
        type == 'view'
    |] ::
    IO [TableEntryRaw]


getTableNames :: Connection -> IO [Text]
getTableNames connection = do
  results :: [SS.Only Text] <-
    query_
      connection
      [SS.sql|
        SELECT tbl_name
        FROM sqlite_master
        WHERE type='table' or type='view'
      |]

  pure (SS.fromOnly <$> results)


getColumnNames :: Connection -> Text -> IO [Text]
getColumnNames connection tableName = do
  results :: [SS.Only Text] <-
    SS.query
      connection
      "SELECT name FROM pragma_table_xinfo(?)"
      [tableName]
  pure (SS.fromOnly <$> results)


-- TODO: investigate whether we ever want to quote the result
nameAsText :: SQL.Name -> Text
nameAsText = \case
  SQL.Name _ name -> name


getFirstName :: Maybe [SQL.Name] -> Maybe Text
getFirstName namesMb = do
  names <- namesMb
  first <- P.head names
  pure (nameAsText first)


getColumnUniqueConstraint ::
  Text ->
  SQL.ColConstraintDef ->
  Maybe UniqueConstraint
getColumnUniqueConstraint col_name = \case
  SQL.ColConstraintDef names SQL.ColUniqueConstraint ->
    Just $
      UniqueConstraint
        { name = getFirstName names
        , columns = [col_name]
        }
  _ -> Nothing


tableUniqueConstraints :: SQL.TableElement -> [UniqueConstraint]
tableUniqueConstraints = \case
  SQL.TableConstraintDef names (SQL.TableUniqueConstraint columns) ->
    [ UniqueConstraint
        { name = getFirstName names
        , columns = P.fmap nameAsText columns
        }
    ]
  SQL.TableColumnDef (SQL.ColumnDef col_name _ constraints) ->
    P.mapMaybe (getColumnUniqueConstraint (nameAsText col_name)) constraints
  _ -> []


getColumnPKConstraint ::
  Text ->
  SQL.ColConstraintDef ->
  Maybe PrimaryKeyConstraint
getColumnPKConstraint col_name = \case
  SQL.ColConstraintDef names (SQL.ColPrimaryKeyConstraint _) ->
    Just $
      PrimaryKeyConstraint
        { name = getFirstName names
        , columns = [col_name]
        }
  _ -> Nothing


tablePKConstraints :: SQL.TableElement -> [PrimaryKeyConstraint]
tablePKConstraints = \case
  SQL.TableConstraintDef names (SQL.TablePrimaryKeyConstraint columns) ->
    [ PrimaryKeyConstraint
        { name = getFirstName names
        , columns = P.fmap nameAsText columns
        }
    ]
  SQL.TableColumnDef (SQL.ColumnDef col_name _ constraints) ->
    P.mapMaybe (getColumnPKConstraint (nameAsText col_name)) constraints
  _ -> []


pkConstraintToUniqueConstraint :: PrimaryKeyConstraint -> UniqueConstraint
pkConstraintToUniqueConstraint pk =
  UniqueConstraint
    { name = pk.name
    , columns = pk.columns
    }


getColumnCheckConstraint ::
  Text ->
  SQL.ColConstraintDef ->
  Maybe CheckConstraint
getColumnCheckConstraint col_name = \case
  SQL.ColConstraintDef names (SQL.ColCheckConstraint expr) ->
    Just $
      CheckConstraint
        { name = getFirstName names
        , columns = Just [col_name]
        , predicate = prettyScalarExpr sqlite expr
        }
  _ -> Nothing


tableCheckConstraints :: SQL.TableElement -> [CheckConstraint]
tableCheckConstraints = \case
  SQL.TableConstraintDef names (SQL.TableCheckConstraint expr) ->
    [ CheckConstraint
        { name = getFirstName names
        , predicate = prettyScalarExpr sqlite expr
        , -- not sure how to do this properly
          columns = Nothing
        }
    ]
  SQL.TableColumnDef (SQL.ColumnDef col_name _ constraints) ->
    P.mapMaybe (getColumnCheckConstraint (nameAsText col_name)) constraints
  _ -> []


getColumnReferencesConstraint ::
  Text ->
  SQL.ColConstraintDef ->
  P.Either Text (Maybe ReferencesConstraint)
getColumnReferencesConstraint col_name = \case
  SQL.ColConstraintDef
    names
    (SQL.ColReferencesConstraint table_names foreign_col_name _ _ _) -> do
      table_name <-
        P.note "Column references constraint has no table name" $
          P.head table_names

      pure $
        Just $
          ReferencesConstraint
            { name = getFirstName names
            , table = nameAsText table_name
            , columns = case foreign_col_name of
                Just explicit_col_name ->
                  ExplicitColumns [(col_name, nameAsText explicit_col_name)]
                Nothing ->
                  ImplicitColumns col_name
            }
  _ -> pure Nothing


tableReferencesConstraints ::
  SQL.TableElement ->
  P.Either Text [ReferencesConstraint]
tableReferencesConstraints = \case
  SQL.TableConstraintDef
    names
    ( SQL.TableReferencesConstraint
        self_columns
        table_names
        foreign_columns
        _
        _
        _
      ) -> do
      table_name <-
        P.note "Table references constraint has no table name" $
          P.head table_names

      columns <- case (self_columns, foreign_columns) of
        ([column], Nothing) ->
          pure $ ImplicitColumns (nameAsText column)
        (_, Nothing) ->
          P.throwError
            "References constraints where more than one column is \
            \implicit are not supported"
        (columns, Just many_foreign_columns) -> do
          P.when (P.length columns /= P.length many_foreign_columns) $ do
            P.throwError
              "Number of columns in references constraint \
              \must be equal"

          pure $
            ExplicitColumns $
              P.zip
                (P.fmap nameAsText columns)
                (P.fmap nameAsText many_foreign_columns)

      pure
        [ ReferencesConstraint
            { name = getFirstName names
            , table = nameAsText table_name
            , columns = columns
            }
        ]
  SQL.TableColumnDef (SQL.ColumnDef col_name _ constraints) ->
    -- => [ColumnConstraint]
    constraints
      -- => [Either Text (Maybe ColumnConstraint)]
      <&> getColumnReferencesConstraint (nameAsText col_name)
      -- => Either Text [Maybe ColumnConstraint]
      & collectAllErrorsAsText
        -- => Either Text [ColumnConstraint]
        <&> P.catMaybes
  _ -> pure []


getTableUniqueIndexConstraints :: SS.Connection -> Text -> IO [UniqueConstraint]
getTableUniqueIndexConstraints connection tableName = do
  indices :: [[SQLData]] <-
    catchAll
      ( SS.query
          connection
          [SS.sql|
            SELECT sql
            FROM sqlite_master
            WHERE tbl_name = ? AND type = 'index'
          |]
          [tableName]
      )
      (\_ -> pure [])

  indices
    <&> \case
      [SQLText sqlTxt]
        -- Get column name from SQL query
        | P.Right (SQL.CreateIndex True indexNames _ columns) <-
            parseSql sqlTxt -> do
            Just $
              UniqueConstraint
                { name = nameAsText <$> P.head indexNames
                , columns = nameAsText <$> columns
                }
      _ -> Nothing
    & P.catMaybes
    & pure


getSqlObjectName :: Statement -> Maybe Text
getSqlObjectName = \case
  SQL.CreateTable names _ _ ->
    names
      & P.head
        <&> nameAsText
  SQL.CreateView _ _ names _ _ ->
    names
      >>= P.head
      <&> nameAsText
  _ -> Nothing


{-| Collects the different kinds of constraints found in a sql statement.

An optional connection can be used to read existing indices for unique
constraints of columns added after table creation.
-}
collectTableConstraints ::
  Maybe SS.Connection ->
  Statement ->
  IO (P.Either Text ParsedTable)
collectTableConstraints connectionMb statement = do
  uniqueIndices <- case (connectionMb, getSqlObjectName statement) of
    (Just conn, Just name) -> getTableUniqueIndexConstraints conn name
    _ -> pure []
  case statement of
    CreateTable _ elements _ -> do
      let referencesConstraintsEither =
            -- => [TableElemenet]
            elements
              -- =>  [Either Text TableElemenet]
              & P.fmap tableReferencesConstraints
              -- =>  Either Text [[TableElemenet]]
              & collectAllErrorsAsText
              -- =>  Either Text [TableElemenet]
              & P.fmap P.join

      let pkConstraints = elements >>= tablePKConstraints
      let uniqueConstraints =
            uniqueIndices
              <> (elements >>= tableUniqueConstraints)
              -- Primary keys are unique by default,
              -- even though they do not have an unique index
              <> (pkConstraints <&> pkConstraintToUniqueConstraint)

      P.for referencesConstraintsEither $ \referencesConstraints -> do
        pure $
          ParsedTable
            { uniqueConstraints = uniqueConstraints
            , referencesConstraints = referencesConstraints
            , checkConstraints = elements >>= tableCheckConstraints
            , primaryKeyConstraints = pkConstraints
            , statement = statement
            }
    _ ->
      pure $
        P.Right $
          ParsedTable
            { uniqueConstraints = uniqueIndices
            , referencesConstraints = []
            , checkConstraints = []
            , primaryKeyConstraints = []
            , statement = statement
            }


enrichTableEntry ::
  SS.Connection ->
  TableEntryRaw ->
  IO (P.Either Text TableEntry)
enrichTableEntry connection tableEntry@(TableEntryRaw{..}) =
  -- Views have no constraints (unique, primary key, references, …) and their
  -- column info is read directly from SQLite via pragmas in
  -- `getColumnsFromParsedTableEntry`. Parsing a view's `CREATE VIEW … AS
  -- SELECT …` body is therefore pointless — `collectTableConstraints` discards
  -- the result for non-`CREATE TABLE` statements. Worse, the approximate SQL
  -- parser rejects plenty of valid SQLite (e.g. a column named "key"), which
  -- would otherwise sink the whole introspection. So for views we skip parsing
  -- the real SQL and feed the parser a trivial placeholder statement instead.
  let
    sqlToParse = case tableEntry.object_type of
      View -> "SELECT 1"
      _ -> tableEntry.sql
  in
    case parseSql sqlToParse of
      P.Left err -> pure $ P.Left (prettyError err)
      P.Right sqlStatement ->
        collectTableConstraints (Just connection) sqlStatement
          <&> P.fmap
            ( \(ParsedTable{..}) ->
                TableEntry{columns = [], ..}
            )


getEnrichedTables :: Connection -> IO (P.Either Text [TableEntry])
getEnrichedTables connection = do
  tables <- getTables connection
  enriched <- P.for tables $ \table -> do
    enrichedEither <- enrichTableEntry connection table
    P.for enrichedEither $ \enriched@TableEntry{..} -> do
      tableColumns <-
        getColumnsFromParsedTableEntry
          connection
          enriched
      pure $
        TableEntry
          { columns = tableColumns
          , ..
          }
  pure $ collectAllErrorsAsText enriched


{-| SQLite allows references constraints to not specify the exact column they
are referencing. This functions tries to recover that information by
looking for primary keys among the columns of the referenced table.
-}
resolveReferencesConstraint :: [TableEntry] -> Text -> Maybe Text
resolveReferencesConstraint tables referencedTable = do
  table <-
    P.find
      (\table -> table.tbl_name == referencedTable)
      tables
  let columns = table.columns
  let pks = P.filter (\column -> column.primary_key) columns
  let nonRowidPks = P.filter (\column -> P.not column.isRowid) pks
  column <- case nonRowidPks of
    [] -> P.find (\column -> column.isRowid) pks
    [column] -> pure column
    -- Note: we currently do not support having composite primary keys
    -- referenced implicitly, as that would lead to multiple complications like:
    -- - figuring out the correct order for the references
    -- - having to perform the "enrichTableEntry" computation in two separate passes
    --
    -- Note 2: Is this that hard to handle? I think there's a good chance we could
    -- do it as long as we keep track of the column order. Not sure it's worth the
    -- hassle though...
    _ -> Nothing
  pure column.column_name


--  See the docs for `resolveReferencesConstraint` for details
resolveReferencesConstraintColumns ::
  [TableEntry] ->
  ReferencesConstraint ->
  Maybe [(Text, Text)]
resolveReferencesConstraintColumns allEntries constraint =
  case constraint.columns of
    ExplicitColumns explicit -> Just explicit
    ImplicitColumns from ->
      case resolveReferencesConstraint allEntries constraint.table of
        Just to -> Just [(from, to)]
        Nothing -> Nothing


-- | Returns a set of warnings related to a given table.
lintTable :: [TableEntry] -> ParsedTable -> [Text]
lintTable allEntries parsed =
  let
    rowidReferenceWarnings =
      parsed.referencesConstraints
        & P.mapMaybe
          ( \constraint ->
              resolveReferencesConstraintColumns allEntries constraint
                & P.fromMaybe []
                & P.find (\(_, to) -> to == "rowid")
                  <&> \case
                    (from, _to) ->
                      "Column "
                        <> quoteText from
                        <> " references the rowid column of table "
                        <> quoteText constraint.table
                        <> ".\n"
                        <> "This is not supported by SQLite:\n"
                        <> "https://www.sqlite.org/foreignkeys.html"
          )

    withoutRowidWarning = case parsed.statement of
      CreateTable names _ True
        | Just name <- getFirstName (Just names) ->
            pure $
              "Table "
                <> quoteText name
                <> " does not have a rowid column. "
                <> "Such tables are not currently supported by Airsequel."
      _ -> []
  in
    rowidReferenceWarnings <> withoutRowidWarning


{-| Lint the sql code for creating a table

An optional connection can be used to retrieve the existing db data, which
is used for things like resolving implicit references constraints (where
the primary key is not explicitly given).

The connection is optional to allow calling this function on-the-fly when
creating the initial table in a databse.
-}
lintTableCreationCode :: Maybe SS.Connection -> Statement -> IO [Text]
lintTableCreationCode connectionMb statement = do
  constraintsEither <- collectTableConstraints connectionMb statement
  allEntriesEither <- case connectionMb of
    Just connection -> getEnrichedTables connection
    Nothing -> pure $ Right []
  pure $ case (constraintsEither, allEntriesEither) of
    (Right _, Left err) -> [err]
    (Left err, Right _) -> [err]
    (Left errL, Left errR) -> [errL, errR]
    (Right parsed, Right allEntries) ->
      lintTable allEntries parsed


getRowidColumnName :: [Text] -> Text
getRowidColumnName colNames
  | "rowid" `notElem` colNames = "rowid"
  | "_rowid_" `notElem` colNames = "_rowid_"
  | "oid" `notElem` colNames = "oid"
  | otherwise = "rowid" -- TODO: Return error to user


{-| Select the column(s) that form this table's primary key. If no non-rowid
columns are marked as part of a PK constraint, the rowid column will be
returned instead.
-}
getPKColumns :: TableEntry -> Maybe [ColumnEntry]
getPKColumns table = do
  let pks = List.filter (\col -> col.primary_key) table.columns

  -- We filter out the rowid column, unless it is the only one
  case pks of
    [] -> Nothing
    [first] | first.isRowid -> Just [first]
    _ -> Just $ List.filter (\col -> P.not col.isRowid) pks


columnDefName :: ColumnDef -> Text
columnDefName (ColumnDef name _ _) = nameAsText name


-- Computes whether a column is NOT NULL
columnIsNonNull :: SQL.ColumnDef -> Bool
columnIsNonNull (ColumnDef _ _ constraints) =
  let isNotNullConstraint = \case
        ColConstraintDef _ ColNotNullConstraint -> True
        _ -> False
  in  P.any isNotNullConstraint constraints


-- For a single column, returns selectable values
-- E.g. ("color", (SelectOptions ["red", "green", "blue"]))
columnSelectOptions :: SQL.ColumnDef -> Maybe SelectOptions
columnSelectOptions (ColumnDef _ _ colConstraints) =
  let
    getSelectOptions ::
      ColConstraintDef ->
      Maybe SelectOptions
    getSelectOptions = \case
      ColConstraintDef
        _
        (ColCheckConstraint (In _ _ (InList options))) ->
          let
            textOnlyOptions =
              options
                <&> \case
                  StringLit _ _ value -> value
                  NumLit value -> value
                  _ -> "UNSUPPORTED"
          in
            Just (SelectOptions textOnlyOptions)
      _ -> Nothing
  in
    colConstraints
      & P.mapMaybe getSelectOptions
      & P.head


getColumnsFromParsedTableEntry ::
  Connection ->
  TableEntry ->
  IO [ColumnEntry]
getColumnsFromParsedTableEntry connection tableEntry = do
  keyColumns :: [[SQLData]] <-
    SS.query
      connection
      "SELECT * FROM pragma_index_info(?)"
      [tableEntry.tbl_name]

  colEntriesRaw :: [ColumnEntryRaw] <-
    P.catches
      ( SS.query
          connection
          "SELECT * FROM pragma_table_xinfo(?)"
          [tableEntry.tbl_name]
      )
      [ P.Handler $ \(error :: SS.SQLError) -> do
          P.putErrText $ sqliteErrorToText error
          pure []
      , P.Handler $ \(error :: SS.FormatError) -> do
          P.putErrText $ show error
          pure []
      , P.Handler $ \(error :: SS.ResultError) -> do
          P.putErrText $ show error
          pure []
      ]

  let
    tableElementsMb = case tableEntry.statement of
      SQL.CreateTable _ tableElements _ ->
        Just tableElements
      _ -> Nothing

    columnDefs = case tableElementsMb of
      Just tableElements ->
        tableElements
          <&> \case
            TableColumnDef columnDef -> Just columnDef
            _ -> Nothing
          & P.catMaybes
      Nothing -> []

    -- As described here: https://www.sqlite.org/withoutrowid.html (Point 5)
    hasRowId :: Bool
    hasRowId = P.null keyColumns

    colNames :: [Text]
    colNames = colEntriesRaw <&> \c -> c.column_name

    rowIdColName :: Text
    rowIdColName = getRowidColumnName colNames

    rowIdColumnEntry :: ColumnEntry
    rowIdColumnEntry =
      ColumnEntry
        { column_name = rowIdColName
        , column_name_gql = rowIdColName
        , datatype = "INTEGER"
        , datatype_gql = Just $ stringToGqlTypeName "Int"
        , select_options = P.Nothing
        , notnull = True
        , isUnique = True
        , isOmittable = True
        , isGenerated = False
        , dflt_value = P.Nothing
        , primary_key = True
        , isReference = False
        , isRowid = True
        }

  let
    entries =
      colEntriesRaw <&> \(ColumnEntryRaw{..}) -> do
        let
          columnDefMb = P.find (\d -> columnDefName d == column_name) columnDefs
          selectOpts = columnDefMb >>= columnSelectOptions
          isNotNull =
            notnull == 1 || primary_key == 1 || case columnDefMb of
              Just columnDef -> columnIsNonNull columnDef
              Nothing -> False

        ColumnEntry
          { column_name_gql = doubleXEncodeGql column_name
          , datatype_gql =
              sqlTypeNameToGQLTypeName
                datatype
                ( P.const
                    (tableEntry.tbl_name <> "_" <> column_name)
                    <$> selectOpts
                )
          , select_options = selectOpts <&> unSelectOptions
          , isUnique =
              P.any
                (\constraint -> column_name `P.elem` constraint.columns)
                tableEntry.uniqueConstraints
          , primary_key =
              primary_key == 1
                || P.any
                  (\constraint -> column_name `P.elem` constraint.columns)
                  tableEntry.primaryKeyConstraints
          , isOmittable =
              (primary_key == 1 && T.isPrefixOf "int" (T.toLower datatype))
                || P.isJust dflt_value
                || P.not isNotNull
          , notnull = isNotNull
          , -- See the comment on the `hidden` property of
            -- the `ColumnEntryRaw` type for an explanation.
            isGenerated = hidden == 2 || hidden == 3
          , isReference =
              tableEntry.referencesConstraints
                & P.any
                  ( \constraint -> case constraint.columns of
                      ImplicitColumns from -> from == column_name
                      ExplicitColumns columns ->
                        columns
                          & P.any (\(from, _) -> from == column_name)
                  )
          , isRowid = False
          , ..
          }
    -- Views don't have a rowid column
    -- (https://stackoverflow.com/q/38519169)
    rowidColumns =
      if hasRowId && tableEntry.object_type /= View
        then [rowIdColumnEntry]
        else []

  pure $ rowidColumns <> entries


getEnrichedTable :: Text -> Connection -> Text -> IO TableEntry
getEnrichedTable dbId connection tableName = do
  tables :: [TableEntryRaw] <-
    SS.query
      connection
      [SS.sql|
        SELECT name, tbl_name, type, rootpage, sql
        FROM sqlite_master
        WHERE name == ?
      |]
      [tableName]

  table <- case P.head tables of
    Just table -> pure table
    Nothing ->
      fail $
        P.fold
          [ "Could not find table info for table "
          , T.unpack tableName
          , " of db "
          , T.unpack dbId
          ]

  enrichmentResultEither <- enrichTableEntry connection table
  case enrichmentResultEither of
    Right result -> pure result
    Left err ->
      fail $
        P.fold
          [ "An error occurred while parsing table "
          , T.unpack tableName
          , " of db "
          , T.unpack dbId
          , ": "
          , T.unpack err
          ]


getColumns :: Text -> Connection -> Text -> IO [ColumnEntry]
getColumns dbId conn tableName =
  let columns = do
        enrichingResult <- getEnrichedTable dbId conn tableName
        getColumnsFromParsedTableEntry conn enrichingResult
  in  catchAll
        columns
        $ \err -> do
          P.putErrText $ P.show err
          pure []


newtype SelectOptions = SelectOptions {unSelectOptions :: [Text]}
  deriving (Show, Eq, Generic)


stringToGqlTypeName :: Text -> GqlTypeName
stringToGqlTypeName name = GqlTypeName{full = name, root = name}


{-| Computes storage class through type affinity
  as described in https://www.sqlite.org/datatype3.html#affname
  with an extension for boolean (Order is important)
  TODO: Add Support for GraphQL's type "ID"
-}
sqlTypeNameToGQLTypeName :: Text -> Maybe Text -> Maybe GqlTypeName
sqlTypeNameToGQLTypeName sqliteType typeNameMb =
  let
    containsText text =
      isInfixOf text $ toUpper sqliteType

    rootType
      -- If it is a view, column might not have a type
      | sqliteType == "" = Nothing
      | containsText "INT" = Just "Int"
      | containsText "CHAR" || containsText "CLOB" || containsText "TEXT" =
          Just "String"
      | containsText "BLOB" = Just "String"
      | containsText "REAL" || containsText "FLOA" || containsText "DOUB" =
          Just "Float"
      | containsText "BOOL" = Just "Boolean"
      | otherwise = Just "Int"
  in
    rootType <&> \root ->
      GqlTypeName
        { root = root
        , full = case typeNameMb of
            P.Just typeName -> doubleXEncodeGql (typeName <> "_" <> root)
            P.Nothing -> root
        }


sqlDataToText :: SQLData -> Text
sqlDataToText = \case
  SQLInteger int64 -> show int64
  SQLFloat double -> show double
  SQLText text -> text
  SQLBlob _ -> "BLOB"
  SQLNull -> "NULL"


-- | WARNING: Also change duplicate `sqlDataToGQLValue`
sqlDataToAesonValue :: Text -> SQLData -> Value
sqlDataToAesonValue datatype sqlData = case sqlData of
  SQLInteger int64 ->
    if isInfixOf "BOOL" $ toUpper datatype
      then case int64 of
        0 -> Bool False
        _ -> Bool True
      else Number $ P.fromIntegral int64 -- Int32
  SQLFloat double -> Number $ Scientific.fromFloatDigits double
  SQLText text -> String text
  SQLBlob _ -> Object $ KeyMap.singleton "type" "blob"
  SQLNull -> Null


{-| Case-insensitively replaces all occurrences of a substring within a string
  with a replacement string.

  Examples:

  >>> replaceCaseInsensitive "hello" "hi" "Hello World"
  "hi World"

  >>> replaceCaseInsensitive "l" "L" "Hello World"
  "HeLLo WorLd"
-}
replaceCaseInsensitive :: Text -> Text -> Text -> Text
replaceCaseInsensitive removable replacement txt =
  let
    len = T.length removable
    process remaining result
      | T.null remaining = result
      | (remaining & T.take len & T.toLower) == (removable & T.toLower) =
          process (remaining & T.drop len) (result <> replacement)
      | otherwise =
          process (remaining & T.drop 1) (result <> T.take 1 remaining)
  in
    process txt ""


{-| Lowercased words with semicolons treated as whitespace,
| so e.g. a terminating `END;` still yields the word "end"
-}
sqlWordsOf :: Text -> [Text]
sqlWordsOf sqlQuery =
  sqlQuery & T.toLower & T.replace ";" " " & T.words


isSqlIdentChar :: P.Char -> Bool
isSqlIdentChar char =
  isAlphaNum char || char == '_'


{-| Apply a rewrite at every position outside of string literals,
| quoted identifiers, and comments. The callback receives whether the
| position starts at a word boundary plus the remaining text, and
| returns the replacement and the text after the matched part.
-}
rewriteSql :: (Bool -> Text -> Maybe (Text, Text)) -> Text -> Text
rewriteSql tryRewrite input =
  let
    -- Copy a quoted section verbatim.
    -- The text starts after the opening quote and doubled closing
    -- quote characters are treated as escapes (`'it''s'`).
    spanQuoted :: P.Char -> Text -> (Text, Text)
    spanQuoted quoteChar text =
      let (inside, closing) = T.break (== quoteChar) text
      in  case T.stripPrefix (T.singleton quoteChar) closing of
            Nothing -> (inside, "") -- Unterminated section
            Just afterQuote ->
              if quoteChar /= ']'
                && T.isPrefixOf (T.singleton quoteChar) afterQuote
                then
                  let (more, rest) = spanQuoted quoteChar (T.drop 1 afterQuote)
                  in  ( inside <> T.replicate 2 (T.singleton quoteChar) <> more
                      , rest
                      )
                else (inside <> T.singleton quoteChar, afterQuote)

    loop :: Bool -> Text -> [Text] -> [Text]
    loop atBoundary text acc =
      case tryRewrite atBoundary text of
        Just (replacement, rest) -> loop True rest (replacement : acc)
        Nothing ->
          case T.uncons text of
            Nothing -> acc
            Just (char, rest)
              | char `P.elem` ['\'', '"', '`', '['] ->
                  let
                    closingChar = if char == '[' then ']' else char
                    (section, rest2) = spanQuoted closingChar rest
                  in
                    loop True rest2 (section : T.singleton char : acc)
              | "--" `T.isPrefixOf` text ->
                  let (comment, rest2) = T.breakOn "\n" text
                  in  loop True rest2 (comment : acc)
              | "/*" `T.isPrefixOf` text ->
                  case T.breakOn "*/" text of
                    (comment, "") -> loop True "" (comment : acc)
                    (comment, rest2) ->
                      loop True (T.drop 2 rest2) ((comment <> "*/") : acc)
              | otherwise ->
                  loop (P.not $ isSqlIdentChar char) rest (T.singleton char : acc)
  in
    loop True input [] & P.reverse & T.concat


{-| Case-insensitively match one SQL word,
| returning the text after it.
| The word must be followed by a non-identifier character (or the end).
-}
matchSqlWord :: Text -> Text -> Maybe Text
matchSqlWord word text = do
  let (candidate, rest) = T.splitAt (T.length word) text
  P.guard $ T.toLower candidate == word
  P.guard $ case T.uncons rest of
    Nothing -> True
    Just (nextChar, _) -> P.not $ isSqlIdentChar nextChar
  pure rest


{-| Case-insensitively match a phrase of SQL words separated by
| arbitrary whitespace, returning the text after the phrase
-}
matchSqlPhrase :: [Text] -> Text -> Maybe Text
matchSqlPhrase phrase text =
  case phrase of
    [] -> Just text
    [word] -> matchSqlWord word text
    word : rest -> do
      afterWord <- matchSqlWord word text
      let afterSpace = T.dropWhile isSpace afterWord
      P.guard $ T.length afterSpace P.< T.length afterWord
      matchSqlPhrase rest afterSpace


{-| Case-insensitively replace a phrase of SQL keywords when it appears
| outside of string literals, quoted identifiers, and comments
-}
replaceSqlPhrase :: [Text] -> Text -> Text -> Text
replaceSqlPhrase phrase replacement =
  rewriteSql $ \atBoundary text -> do
    P.guard atBoundary
    rest <- matchSqlPhrase phrase text
    pure (replacement, rest)


{-| Cut the text short at the first occurrence of the given SQL phrase
| outside of string literals, quoted identifiers, and comments
-}
truncateAtSqlPhrase :: [Text] -> Text -> Text
truncateAtSqlPhrase phrase =
  rewriteSql $ \atBoundary text -> do
    P.guard atBoundary
    _ <- matchSqlPhrase phrase text
    pure ("", "")


{-| Remove a keyword that directly follows a closing paren,
| e.g. `… ) STRICT` or a generated column's `… ) STORED`.
| The paren anchor avoids mangling the keyword when it is used
| as a column name.
-}
removeKeywordAfterParen :: Text -> Text -> Text
removeKeywordAfterParen keyword =
  rewriteSql $ \_ text -> do
    afterParen <- T.stripPrefix ")" text
    rest <- matchSqlWord keyword (T.dropWhile isSpace afterParen)
    pure (")", rest)


{-| Drop leading whitespace, semicolons, and comments,
| returning the text starting at the first significant token
-}
dropSqlNoise :: Text -> Text
dropSqlNoise text =
  if
    | T.null text -> text
    | "--" `T.isPrefixOf` text ->
        dropSqlNoise (text & T.dropWhile (/= '\n') & T.drop 1)
    | "/*" `T.isPrefixOf` text ->
        case T.breakOn "*/" text of
          (_, "") -> ""
          (_, rest) -> dropSqlNoise (T.drop 2 rest)
    | isSpace (T.head text) || T.head text == ';' ->
        dropSqlNoise (T.drop 1 text)
    | otherwise -> text


-- | First SQL word of a statement, ignoring whitespace and comments
firstSqlWord :: Text -> Text
firstSqlWord text =
  dropSqlNoise text & T.takeWhile isSqlIdentChar & T.toLower


{-| Whether the given SQL keyword appears anywhere in the text,
| outside of string literals, quoted identifiers, and comments
-}
containsSqlWord :: Text -> Text -> Bool
containsSqlWord word text =
  T.length (truncateAtSqlPhrase [word] text) P./= T.length text


{-| Read-only / introspection pragmas that are safe to expose via the
| SQL API. Every pragma listed here is read-only in all of its forms
| (bare and `pragma(arg)`), so none of them can mutate the database or
| relax a resource guard. Settable pragmas are deliberately excluded —
| even their read form is rejected — because their `pragma(value)`
| call form is a disguised write (e.g. `PRAGMA foreign_keys(0)`).
-}
readOnlyPragmas :: [Text]
readOnlyPragmas =
  [ "collation_list"
  , "compile_options"
  , "data_version"
  , "database_list"
  , "foreign_key_check"
  , "foreign_key_list"
  , "freelist_count"
  , "function_list"
  , "index_info"
  , "index_list"
  , "index_xinfo"
  , "integrity_check"
  , "module_list"
  , "page_count"
  , "pragma_list"
  , "quick_check"
  , "table_info"
  , "table_list"
  , "table_xinfo"
  ]


data PragmaForm = PragmaRead | PragmaCall | PragmaAssign
  deriving (Eq, Show)


{-| Parse a `PRAGMA [schema.]name [= value | (args)]` statement into its
| pragma name (lowercased, schema prefix stripped) and its form.
| Returns Nothing when the statement is not a PRAGMA.
-}
parsePragmaStatement :: Text -> Maybe (Text, PragmaForm)
parsePragmaStatement statementTxt = do
  afterPragma <- matchSqlWord "pragma" (dropSqlNoise statementTxt)
  let
    -- A pragma may be qualified by a schema, e.g. `main.table_info`.
    -- Keep the segment after the dot as the pragma name.
    (segment, afterSegment) =
      afterPragma & T.dropWhile isSpace & T.span isSqlIdentChar
    (name, afterName) = case T.uncons afterSegment of
      Just ('.', afterDot) -> T.span isSqlIdentChar afterDot
      _ -> (segment, afterSegment)
    form = case T.uncons (T.dropWhile isSpace afterName) of
      Just ('=', _) -> PragmaAssign
      Just ('(', _) -> PragmaCall
      _ -> PragmaRead
  P.guard $ P.not $ T.null name
  pure (T.toLower name, form)


{-| Reject statements that would bypass Airsequel's resource and
| integrity guards even though the parser (and SQLite) accept them:
| non-read-only PRAGMAs, which can relax the storage/heap limits or
| foreign-key enforcement, and `VACUUM … INTO`, which writes an
| arbitrary file on the server. Returns the validation errors, if any.
-}
validateStatementSafety :: Text -> [Text]
validateStatementSafety statementTxt =
  case firstSqlWord statementTxt of
    "pragma" ->
      case parsePragmaStatement statementTxt of
        Just (name, form)
          | form P.== PragmaAssign ->
              ["Setting PRAGMA values is not allowed via the SQL API"]
          | name `notElem` readOnlyPragmas ->
              [ "PRAGMA "
                  <> name
                  <> " is not allowed via the SQL API. "
                  <> "Only read-only pragmas are permitted "
                  <> "(e.g. table_info, foreign_key_list, integrity_check)."
              ]
        _ -> []
    "vacuum"
      | containsSqlWord "into" statementTxt ->
          ["VACUUM … INTO is not allowed via the SQL API"]
    _ -> []


{-| Rewrite SQLite's null-safe `a IS b` comparison to `a = b`,
| which the parser understands. `IS` forms the parser supports
| natively (`IS NULL`, `IS TRUE`, `IS NOT …`, `IS DISTINCT FROM`, …)
| are left untouched.
-}
rewriteGenericIs :: Text -> Text
rewriteGenericIs =
  rewriteSql $ \atBoundary text -> do
    P.guard atBoundary
    rest <- matchSqlWord "is" text
    let nextWord =
          rest
            & T.dropWhile isSpace
            & T.takeWhile isSqlIdentChar
            & T.toLower
    P.guard $
      nextWord `P.notElem` ["not", "null", "true", "false", "unknown", "distinct"]
    pure ("=", rest)


sanitizeSql :: Text -> Text
sanitizeSql sql =
  sql
    -- TODO: Remove after
    --       https://github.com/JakeWheat/simple-sql-parser/issues/27
    & replaceSqlPhrase ["if", "not", "exists"] ""
    & replaceSqlPhrase ["if", "exists"] ""
    -- TODO: Remove after
    --       https://github.com/JakeWheat/simple-sql-parser/issues/37
    & replaceSqlPhrase ["insert", "or", "abort"] "INSERT"
    & replaceSqlPhrase ["insert", "or", "fail"] "INSERT"
    & replaceSqlPhrase ["insert", "or", "ignore"] "INSERT"
    & replaceSqlPhrase ["insert", "or", "replace"] "INSERT"
    & replaceSqlPhrase ["insert", "or", "rollback"] "INSERT"
    -- Must come after the `insert or replace` rule above
    & replaceSqlPhrase ["replace", "into"] "INSERT INTO"
    -- Removing the JSON arrow operator seems to be enough
    -- to make the parser accept all queries containing JSON operators
    & T.replace "->" ""
    -- https://www.sqlite.org/stricttables.html
    & removeKeywordAfterParen "strict"
    -- Generated columns
    -- TODO: Remove after
    --       https://github.com/JakeWheat/simple-sql-parser/issues/46
    & removeKeywordAfterParen "stored"
    & removeKeywordAfterParen "virtual"
    -- Statement types the parser doesn't support at all are replaced
    -- with a dummy statement to accept them.
    -- SQLite itself reports an error during execution if necessary.
    -- ATTACH and DETACH are deliberately not included,
    -- as rejecting them keeps them from being executed.
    & ( \sqlQuery ->
          if firstSqlWord sqlQuery
            `P.elem` [ "pragma"
                     , "vacuum"
                     , "analyze"
                     , "reindex"
                     , "begin"
                     , "end"
                     , "release"
                     ]
            then "SELECT 0" -- Dummy statement to accept the query
            else sqlQuery
      )
    -- TODO: Remove after
    --       https://github.com/JakeWheat/simple-sql-parser/issues/20
    & ( \sqlQuery ->
          if P.all
            (\word -> word `P.elem` sqlWordsOf sqlQuery)
            ["alter", "table", "rename"]
            then "SELECT 0" -- Dummy statement to accept the query
            else sqlQuery
      )
    -- TODO: Remove after
    --       https://github.com/JakeWheat/simple-sql-parser/issues/41
    & ( \sqlQuery ->
          if P.all
            (\word -> word `P.elem` sqlWordsOf sqlQuery)
            ["create", "trigger", "on", "begin", "end"]
            then "SELECT 0" -- Dummy statement to accept the query
            else sqlQuery
      )
    & replaceSqlPhrase ["drop", "trigger"] "DROP TABLE"
    & replaceSqlPhrase ["drop", "index"] "DROP TABLE"
    -- The parser supports neither RETURNING nor upsert clauses,
    -- so DML statements are cut short at those keywords.
    -- TODO: Remove after
    --       https://github.com/JakeWheat/simple-sql-parser/issues/42
    & ( \sqlQuery ->
          if firstSqlWord sqlQuery
            `P.elem` ["insert", "update", "delete", "with"]
            then
              sqlQuery
                & truncateAtSqlPhrase ["on", "conflict"]
                & truncateAtSqlPhrase ["returning"]
            else sqlQuery
      )
    -- TODO: Remove after
    --       https://github.com/JakeWheat/simple-sql-parser/issues/43
    & T.replace "==" "="
    & rewriteGenericIs
    -- The internal table is created without column types
    -- TODO: Remove after
    -- https://github.com/JakeWheat/simple-sql-parser/issues/38#issuecomment-1413340116
    & replaceCaseInsensitive
      "sqlite_sequence(name,seq)"
      "sqlite_sequence(name TEXT,seq INT)"
    -- Internal tables created by `ANALYZE` without column types
    & replaceCaseInsensitive
      "sqlite_stat1(tbl,idx,stat)"
      "sqlite_stat1(tbl TEXT,idx TEXT,stat TEXT)"
    & replaceCaseInsensitive
      "sqlite_stat4(tbl,idx,neq,nlt,ndlt,sample)"
      "sqlite_stat4(tbl TEXT,idx TEXT,neq TEXT,nlt TEXT,ndlt TEXT,sample BLOB)"
    -- TODO: Remove after
    --       https://github.com/JakeWheat/simple-sql-parser/issues/40
    & replaceSqlPhrase ["not", "null", "default"] "DEFAULT"
    & replaceSqlPhrase ["glob"] "LIKE"


-- | SQLite dialect
sqlite :: Dialect
sqlite =
  ansi2011
    { diLimit = True
    , diAutoincrement = True
    , diAppKeywords =
        ansi2011.diAppKeywords
          <> [ "abs"
             , -- https://www.sqlite.org/lang_mathfunc.html
               "acos"
             , "acosh"
             , "asin"
             , "asinh"
             , "atan"
             , "atan2"
             , "atanh"
             , "ceil"
             , "ceiling"
             , "cos"
             , "cosh"
             , "degrees"
             , "exp"
             , "floor"
             , "ln"
             , "log"
             , "log"
             , "log10"
             , "log2"
             , "mod"
             , "pi"
             , "pow"
             , "power"
             , "radians"
             , "sin"
             , "sinh"
             , "sqrt"
             , "tan"
             , "tanh"
             , "trunc"
             , -- These are reserved keywords as well, but must also be
               -- callable as their scalar function forms, e.g. replace(X,Y,Z).
               -- "match" is intentionally excluded: it is only the FTS MATCH
               -- operator and has no usable scalar function form.
               "replace"
             , "like"
             , "glob"
             ]
    , diKeywords =
        [ "abort"
        , "action"
        , "add"
        , "after"
        , "all"
        , "alter"
        , "always"
        , "analyze"
        , "and"
        , "as"
        , "asc"
        , "attach"
        , "autoincrement"
        , "before"
        , "begin"
        , "between"
        , "by"
        , "cascade"
        , "case"
        , "cast"
        , "check"
        , "collate"
        , "column"
        , "commit"
        , "conflict"
        , "constraint"
        , "create"
        , "cross"
        , "current"
        , "database"
        , "default"
        , "deferrable"
        , "deferred"
        , "delete"
        , "desc"
        , "detach"
        , "distinct"
        , "do"
        , "drop"
        , "each"
        , "else"
        , "end"
        , "escape"
        , "except"
        , "exclude"
        , "exclusive"
        , "exists"
        , "explain"
        , "fail"
        , "filter"
        , "first"
        , "following"
        , "for"
        , "foreign"
        , "from"
        , "full"
        , "generated"
        , "glob"
        , "group"
        , "groups"
        , "having"
        , "if"
        , "ignore"
        , "immediate"
        , "in"
        , "index"
        , "indexed"
        , "initially"
        , "inner"
        , "insert"
        , "instead"
        , "intersect"
        , "into"
        , "is"
        , "isnull"
        , "join"
        , "last"
        , "left"
        , "like"
        , "limit"
        , "match"
        , "materialized"
        , "natural"
        , "no"
        , "not"
        , "nothing"
        , "notnull"
        , -- although "null" is on the official list of keywords, adding it here
          --  seems to break "select NULL as ..." statements
          -- , "null"
          "nulls"
        , "of"
        , "offset"
        , "on"
        , "or"
        , "order"
        , "others"
        , "outer"
        , "over"
        , "partition"
        , "plan"
        , "pragma"
        , "preceding"
        , "primary"
        , "query"
        , "raise"
        , "range"
        , "recursive"
        , "references"
        , "regexp"
        , "reindex"
        , "release"
        , "rename"
        , "replace"
        , "restrict"
        , "returning"
        , "right"
        , "rollback"
        , "row"
        , "rows"
        , "savepoint"
        , "select"
        , "set"
        , "table"
        , "temp"
        , "temporary"
        , "then"
        , "ties"
        , "to"
        , "transaction"
        , "trigger"
        , "unbounded"
        , "union"
        , "unique"
        , "update"
        , "using"
        , "vacuum"
        , "values"
        , "view"
        , "virtual"
        , "when"
        , "where"
        , "window"
        , "with"
        , "without"
        ]
    , diBackquotedIden = True -- https://sqlite.org/lang_keywords.html
    , diSquareBracketQuotedIden = True -- https://sqlite.org/lang_keywords.html
    , diWithoutRowidTables = True -- https://www.sqlite.org/withoutrowid.html
    }


parseSql :: Text -> P.Either ParseError Statement
parseSql sqlQuery =
  parseStatement sqlite "" P.Nothing $ sanitizeSql sqlQuery


foreign import ccall unsafe "sqlite3_complete"
  sqlite3Complete :: CString -> IO CInt


{-| Whether the text ends in a complete SQL statement, i.e. a semicolon
| that is not inside a string literal, a comment,
| or a trigger's `BEGIN … END` block.
| Wraps SQLite's own `sqlite3_complete`.
-}
sqlIsComplete :: Text -> IO Bool
sqlIsComplete text = do
  result <- BS.useAsCString (encodeUtf8 text) sqlite3Complete
  pure $ result == 1


-- | Whether the text contains only whitespace, semicolons, and comments
sqlIsBlank :: Text -> Bool
sqlIsBlank = loop
  where
    loop text =
      case T.uncons text of
        Nothing -> True
        Just (char, rest)
          | "--" `T.isPrefixOf` text ->
              loop (text & T.dropWhile (/= '\n') & T.drop 1)
          | "/*" `T.isPrefixOf` text ->
              let (_, after) = T.breakOn "*/" text
              in  T.null after || loop (T.drop 2 after)
          | isSpace char || char == ';' -> loop rest
          | otherwise -> False


{-| Split a SQL script into its individual statements.
| Statement boundaries are found with SQLite's `sqlite3_complete`,
| so semicolons inside string literals, comments,
| and trigger definitions (`BEGIN … END`) are handled correctly.
| Statements without any SQL content are omitted.
-}
splitSqlStatements :: Text -> IO [Text]
splitSqlStatements sqlText = do
  (pendingMb, statementsReversed) <-
    P.foldM
      ( \(pendingMb, statements) segment -> do
          let candidate = case pendingMb of
                P.Nothing -> segment
                P.Just pending -> pending <> ";" <> segment
          isComplete <- sqlIsComplete (candidate <> ";")
          pure $
            if isComplete
              then (P.Nothing, (candidate <> ";") : statements)
              else (P.Just candidate, statements)
      )
      (P.Nothing, [])
      (T.splitOn ";" sqlText)

  -- Text after the last semicolon is an unterminated final statement.
  -- Execute it anyway so SQLite can report its own error if it's invalid.
  let statements = P.reverse $ case pendingMb of
        P.Nothing -> statementsReversed
        P.Just pending -> pending : statementsReversed

  pure $ statements & P.filter (P.not . sqlIsBlank)


newtype SQLPost = SQLPost
  { query :: Text
  }
  deriving (Eq, Show, Generic)


instance ToJSON SQLPost
instance FromJSON SQLPost


instance ToSample AirGQL.Lib.SQLPost where
  toSamples _ = singleSample $ SQLPost "SELECT * FROM users"


-- | Converts a SQL error to text, possibly adding airsequel-specific context.
sqliteErrorToText :: SS.SQLError -> Text
sqliteErrorToText error =
  let
    addendum = case SS.sqlErrorDetails error of
      "database or disk is full" ->
        Just $
          "This might be caused by the database exceeding "
            <> "the maximum page count for your current plan."
      _ -> Nothing
  in
    show error <> case addendum of
      Nothing -> ""
      Just text -> "\n" <> text
