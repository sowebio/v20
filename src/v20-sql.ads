------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20-Sql.ads
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
------------------------------------------------------------------------------
--  @summary
--  V20 library SQLite binding
--
--  @description
--  High level SQLite binding
--
--  @authors
--  Dmitry Kazakov - dk - http://www.dmitry-kazakov.de (low level SQLite binding in sql directory)
--  Stéphane Rivière - sr - sriviere@soweb.io (high level SQLite binding, some low level binding hacks)
--
--  @versions
--  see v20.ads
------------------------------------------------------------------------------

-- Schema
with Ada.Containers.Vectors;
with Ada.Text_IO;

--  SQLite
with Interfaces;
with SQLite;

with v20.Vst; use v20.Vst;

package v20.Sql is

   Table_Dont_Exists : exception;

   subtype Statement is SQLite.Statement;
   subtype Datatype  is SQLite.Datatype;
   subtype Integer_64 is Interfaces.Integer_64;

   --  Command           Name          Attribute
   --  -------------------------------------------------
   --  Database_Name     Gnx
   --  Database_Pragma   journal_mode  WAL
   --  Database_Pragma   synchronous   FULL
   --  Database_Pragma   foreign_keys  ON
   --  Database_Schema   prog_version  x.y
   --  Table_Name        Cluster
   --  Column_Name       Number        INTEGER
   --  Column_Constraint Number        UNIQUE
   --  Column_Name       Default       INTEGER
   --  Column_Name       Domain        TEXT
   --  Column_Name       Email         TEXT
   --  Column_Name       Manager       INTEGER
   --  Column_Name       API_EP        TEXT
   --  Column_Name       API_AK        TEXT
   --  Column_Name       API_AS        TEXT
   --  Column_Name       API_CK        TEXT
   --  Table_Constraint  Number        PRIMARY KEY
   --  Table_Options
   --  Index_Name        Idx           Number
   --  Index_Constraint  Idx           UNIQUE

   type Schema_Command is (Null_Command,
                           Database_Name, Database_Pragma,
                           Table_Name, Table_Constraint,
                           Column_Name, Column_Constraint,
                           Index_Name, Index_Constraint);

   type Row_Count_Mode is (All_Rows, Not_Null_Rows, Distinct_Rows);

   package Schema_Command_List is new Ada.Text_IO.Enumeration_IO (Enum => Schema_Command);

   type Schema_Line is record
      Command : Schema_Command ;
      Name : VString;
      Attribute : VString;
      Comment : VString;
   end record;

   package Schema_Lines_List is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Schema_Line);

   Status_Need_Update : constant Integer := 500;
   Status_No_Code     : constant Integer := 501;

   --  https://www.sqlite.org/rescode.html
   --  Gap in numbering means unused code
   Info_Ok            : constant Integer := 0;
   Info_Row           : constant Integer := 100;
   Info_Done          : constant Integer := 101;

   Error_Generic      : constant Integer := 1;
   Error_Internal     : constant Integer := 2;
   Error_Perm         : constant Integer := 3;
   Error_Abort        : constant Integer := 4;
   Error_Busy         : constant Integer := 5;
   Error_Locked       : constant Integer := 6;
   Error_No_Mem       : constant Integer := 7;
   Error_Readonly     : constant Integer := 8;
   Error_Interrupt    : constant Integer := 9;
   Error_In_Out       : constant Integer := 10;
   Error_Corrupt      : constant Integer := 11;
   Error_Not_Found    : constant Integer := 12;
   Error_Full         : constant Integer := 13;
   Error_Cant_Open    : constant Integer := 14;
   Error_Protocol     : constant Integer := 15;
   Error_Schema       : constant Integer := 17;
   Error_Too_Big      : constant Integer := 18;
   Error_Constraint   : constant Integer := 19;
   Error_Mismatch     : constant Integer := 20;
   Error_Misuse       : constant Integer := 21;
   Error_No_Lfs       : constant Integer := 22;
   Error_Authent      : constant Integer := 23;
   Error_Range        : constant Integer := 25;
   Error_Not_A_DB     : constant Integer := 26;

   --
   procedure Bind (Parameter : Positive; Value : Integer);
   procedure Bind (Local_Handle_Statement : Statement; Parameter : Positive; Value : Integer);
   procedure Bind (Parameter : Positive; Value : VString);
   procedure Bind (Local_Handle_Statement : Statement;  Parameter : Positive; Value : VString);
   --  Set a parameter of statement.
   --  The parameters to be bound are usually specified as ? in the command
   --  text (see  Prepare). Each such parameter has to be bound to a value.
   --  The position of a parameter is specified by its index, i.e. by  the
   --  position of ? in the command  text. The first parameter has the
   --  position 1.
   --  Exceptions :
   --    Constraint_Error - Command or Parameter is invalid
   --    Data_Error       - Data base error
   --    End_Error        - Not found (table does not exist)
   --    Status_Error     - Access errors
   --    Use_Error        - File access related errors

   function Column_Integer (Position : Positive) return Integer;
   function Column_Integer (Local_Handle_Statement : Statement; Position : Positive) return Integer;
   -- Get a column in the current result row
   -- Exceptions :
   --    Constraint_Error - Command is an invalid handle

   function Column_Text (Position : Positive) return VString;
   function Column_Text (Local_Handle_Statement : Statement; Position : Positive) return VString;
   -- Get a column in the current result row
   -- Exceptions :
   --    Constraint_Error - Command is an invalid handle

   function Column_Count return Natural;
   function Column_Count (Local_Handle_Statement : Statement) return Natural;
   -- Column_Count -- Get the number of columns in the current result set
   -- Exceptions :
   --    Constraint_Error - Command is an invalid handle

   function Column_Exists (Table_Name : String; Column_Name : String) return Boolean;
   function Column_Exists (Table_Name : VString; Column_Name : VString) return Boolean;
   -- Return true if Column_Name exists. NO NEED TO PASS A LOCAL STATEMENT
   --  Exceptions :
   --    Constraint_Error - Base is an invalid handle
   --    Data_Error       - Data base error
   --    End_Error        - Not found (table does not exist)
   --    Status_Error     - Access error
   --    Use_Error        - File open error

   function Column_Type (Position : Positive) return Natural;
   function Column_Type (Local_Handle_Statement : Statement; Position : Positive) return Natural;
   -- Get a column type in the current result row
   -- Exceptions :
   --    Constraint_Error - Command is an invalid handle

   procedure Delete (Table_Name : VString ; Where_Condition : VString);
   -- Delete a row in Table_Name specifying a Where_Condition.
   --  Exceptions :
   --    Constraint_Error - Base is an invalid handle
   --    Data_Error       - Data base error
   --    End_Error        - Not found (table does not exist)
   --    Status_Error     - Access error
   --    Use_Error        - File open error

   function Error (Information : String ; Information_Extended : out VString) return Integer;
   --  SQLite error processing

   procedure Error (Exception_Hook : Ada.Exceptions.Exception_Occurrence);
   --  SQLite error processing

   function Error_Display (Error_Code : Integer) return VString;
   --  SQLite status, info and error display

   procedure Exec (Command : String);
   procedure Exec (Command : VString);

   --  Execute a SQL command when no output is needed. It's a wrapper around
   --  Prepare, Step and Finalize, that allows an application to run multiple
   --  statements of SQL without having to use a lot of code. Command is UTF-8
   --  encoded.
   --  Exceptions :
   --    Constraint_Error - Base is an invalid handle
   --    Data_Error       - Data base error
   --    End_Error        - Not found (table does not exist)
   --    Status_Error     - Access error
   --    Use_Error        - File open error

   function Get_Config (Parameter : String) return VString;
   function Get_Config (Parameter : VString) return VString;
   -- Get configuration Value from Parameter stored in Config table.

      function Get_Version return VString;
   --  SQLite version (x.x.x format).

   function Index_Exists (Index_Name : VString) return Boolean;
   -- Return true if Column_Name exists.
   --  Exceptions :
   --    Constraint_Error - Base is an invalid handle
   --    Data_Error       - Data base error
   --    End_Error        - Not found (table does not exist)
   --    Status_Error     - Access error
   --    Use_Error        - File open error

   procedure Insert (Table_Name : VString; Columns_Values : VString);
   -- Create a row in Table_Name with Columns_Values. The special character ^
   -- is used to separate column/value pairs and the special character ~ is
   -- used to distinguish the name of a column from its value.
   --  Sql.Insert (+"Cluster", +"Number~1234" & "^" & "Domain~genesix.org")
   --  Exceptions :
   --    Constraint_Error - Base is an invalid handle
   --    Data_Error       - Data base error
   --    End_Error        - Not found (table does not exist)
   --    Status_Error     - Access error
   --    Use_Error        - File open error

   function Last_Insert_RowID return Integer_64;
   --  The function usually returns the rowid of the most recent successful
   --  INSERT into a rowid table or virtual table. Inserts into WITHOUT ROWID
   --  tables are not recorded. If no successful INSERTs into rowid tables
   --  have ever occurred on the database, then the function returns zero.
   --  Each entry in most SQLite tables (except for WITHOUT ROWID tables) has
   --  a unique 64-bit signed integer key called the "rowid". The rowid is
   --  always available as an undeclared column named ROWID, OID, or _ROWID_
   --  as long as those names are not also used by explicitly declared
   --  columns. If the table has a column of type INTEGER PRIMARY KEY then
   --  that column is another alias for the rowid (this text comes from
   --  https://www.sqlite.org/c3ref/last_insert_rowid.html).

   function Last_RowID (Table_Name : VString) return Integer;
   -- Returns last existing RowID in Table_Name.

   procedure Open (Database_File_Name : VString);
   --  Open a database.
   --  Exceptions:
   --    Data_Error - Data base error
   --    Use_Error  - File open error

   procedure Prepare (Statement_To_Prepare : VString);
   function Prepare (Statement_To_Prepare : VString) return Statement;
   --  Prepare a SQL command when an output is needed. Command is UTF-8
   --  encoded.
   --  The statement life-cycle looks like this:
   --    declare
   --       Command : Statement := Prepare (DB, "SQL command");
   --    begin
   --       Bind (Command, ...); -- Binding parameters
   --       while Step (Command) loop
   --          ... Column (Command) ... -- Taking the results out
   --       end loop;
   --       Reset (Command);
   --  Exceptions :
   --    Constraint_Error - Base is an invalid handle
   --    Data_Error       - Data base error
   --    End_Error        - Not found (table does not exist)
   --    Status_Error     - Access error
   --    Use_Error        - File open error

   function Read (Table_Name : VString; Columns : VString; Condition : VString := +"") return VString;
   -- Returns an extraction from Table_Name with comma delimited Columns and
   -- standard SQL Condition (like WHERE, ORDER BY, LIMIT). The extraction is
   -- formatted with standard v20 CD constant as Column delimiter and RD
   -- constant as Row delimiter.
   --  Exceptions :
   --    Constraint_Error - Base is an invalid handle
   --    Data_Error       - Data base error
   --    End_Error        - Not found (table does not exist)
   --    Status_Error     - Access error
   --    Use_Error        - File open error

   function Row_Count (Table_Name : String; Option : String := "*") return Natural;
   --  Returns counted rows in Table_Name with Options All_Rows, Not_Null_Rows,
   --  Distinct_Rows. Type Row_Count_Mode (All_Rows, Not_Null_Rows, Distinct_Rows)

   procedure Reset;
   procedure Reset (Local_Handle_Statement : Statement);
   --  Reset -- Complete SQL command execution, make it ready to execute again
   --  Exceptions :
   --    Constraint_Error - Command is an invalid handle

   function Schema_Need_Update (Database_FullName : String ; Major : Natural; Minor : Natural) return Integer;
   function Schema_Need_Update (Database_FullName : VString ; Major : Natural; Minor : Natural) return Integer;
   -- Return True if program version is inferior to required schema version. Useful to
   -- avoid running Schema_CRUD each time the program is ran.

   procedure Schema_Load (Command : in Schema_Command := Null_Command ;
                             Name : in String := "";
                        Attribute : in String := "";
                          Comment : in String := "");
   -- Load a schema line. Commands will be executed with Schema_Update in code
   -- source order

   procedure Schema_Update;
   -- Create, read, update and delete operations on database schema after
   -- loading schema by Schema_Load

   function Search (Table_Name : VString; Condition : VString) return Boolean;
   -- Return True if Condition verified.

   procedure Set_Config (Parameter : String ; Value : String);
   procedure Set_Config (Parameter : VString ; Value : VString);
   -- Store configuration Parameter and Value to Config table.

   procedure Step;
   procedure Step (Local_Handle_Statement : Statement);
   function Step return Boolean;
   function Step (Local_Handle_Statement : Statement) return Boolean;
   --  Execute prepared command
   --  When  the  result is False, the Command execution has been completed.
   --  In  this  case the next operation should be Reset. When the result is
   --  True  there  is  a  row  of  data  produced  by the command. The next
   --  operation can be Step to get another row or else Reset to  reset  the
   --  statement. After calling Reset, the parameters can be rebound before
   --  another execution of the parameter is initiated by doing Step.
   --  Exceptions :
   --    Constraint_Error - Command is an invalid handle
   --    Data_Error       - Data base error
   --    End_Error        - Not found (table does not exist)
   --    Status_Error     - Access error
   --    Use_Error        - File open error

   function Table_Exists (Table_Name : String) return Boolean;
   function Table_Exists (Table_Name : VString) return Boolean;
   -- Return true if Table_Name exists.
   -- Exceptions :
   --    Constraint_Error - Base is an invalid handle
   --    Data_Error       - Data base error
   --    Status_Error     - Access error
   --    Use_Error        - File open error

   procedure Update (Table_Name : VString; Columns_Values : VString;
                     Where_Condition : VString);
   -- Update a row in Table_Name with Columns_Values specifying a
   -- Where_Condition. The special character ^ is used to separate
   -- column/value pairs and the special character ~ is used to distinguish
   -- the name of a column from its value.
   -- Sql.Update (+"Cluster", +"Domain~genesix2.org", +"Number = 1234");
   --  Exceptions :
   --    Constraint_Error - Base is an invalid handle
   --    Data_Error       - Data base error
   --    End_Error        - Not found (table does not exist)
   --    Status_Error     - Access error
   --    Use_Error        - File open error

------------------------------------------------------------------------------
private

   Database_Full_Name : VString := +"";

   Handle_Database : SQLite.Data_Base;
   Handle_Statement : SQLite.Statement;

   Schema : Schema_Lines_List.Vector ;

   -- Schema version
   Version_Major : Natural := 0;
   Version_Minor : Natural := 0;

------------------------------------------------------------------------------
end v20.Sql;
------------------------------------------------------------------------------
