-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20-Sql.adb
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V20 library SQLite binding
--
--  @description
--  Highlevel SQLite binding
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io (high level SQLite binding, some low level binding hacks)
--  Dmitry Kazakov - dk - http://www.dmitry-kazakov.de (low level SQLite binding in sql directory)
--
--  @versions
--  see .ads
-------------------------------------------------------------------------------

-- Some usefull SQLite tricks

-- How to do UPSERT
--  * First pass (record does not exists)
--
--  Update on a non existent record
--  UPDATE System SET Value='0.0' WHERE Parameter='Shema_Version';
--  Success : 0 record(s) affected
--
--  Create ok because Changes() = 0 (no change from the last operation)
--  INSERT INTO System (Parameter, Value) SELECT 'Shema_Version', '0.0' WHERE (Select Changes() = 0);
--  Success : 1 record(s) affected
--
--  * Second pass (record already exists)
--
--  Update an existent record successfull
--  UPDATE System SET Value='0.1' WHERE Parameter='Shema_Version';
--  Success : 1 record(s) affected
--
--  No create since Changes() = 1 (last operation has change something)
--  INSERT INTO System (Parameter, Value) SELECT 'Shema_Version', '0.1' WHERE (Select Changes() = 0);
--  Success : 0 record(s) affected


--with Ada.Exceptions;         use Ada.Exceptions;
--with Ada.IO_Exceptions;

with Ada.Exceptions;
with Ada.IO_Exceptions;

with Interfaces.C;

with v20.Fls;
with v20.Log;

package body v20.Sql is

   package AE  renames Ada.Exceptions;

   --
   procedure Bind (Parameter : Positive; Value : Integer) is
   begin
      SQLite.Bind (Handle_Statement, Parameter, Integer_64 (Value));
   end Bind;

   procedure Bind (Local_Handle_Statement : Statement; Parameter : Positive; Value : Integer) is
   begin
      SQLite.Bind (Local_Handle_Statement, Parameter, Integer_64 (Value));
   end Bind;

   procedure Bind (Parameter : Positive; Value : VString) is
   begin
      SQLite.Bind (Handle_Statement, Parameter, To_String (Value));
   end Bind;

   procedure Bind (Local_Handle_Statement : Statement;  Parameter : Positive; Value : VString) is
   begin
      SQLite.Bind (Local_Handle_Statement, Parameter, To_String (Value));
   end Bind;

   --
   function Column_Integer (Position : Positive) return Integer is
   begin
      -- Very dirty hack to avoid "error: ambiguous operand in conversion"
      return Integer'Value (Interfaces.C.int'Image (SQLite.Column (Handle_Statement, Position)));
   end Column_Integer;

   function Column_Integer (Local_Handle_Statement : Statement; Position : Positive) return Integer is
   begin
      -- Very dirty hack to avoid "error: ambiguous operand in conversion"
      return Integer'Value (Interfaces.C.int'Image (SQLite.Column (Local_Handle_Statement, Position)));
   end Column_Integer;

   function Column_Text (Position : Positive) return VString is
   begin
      return To_VString (SQLite.Column (Handle_Statement, Position));
   end Column_Text;

   function Column_Text (Local_Handle_Statement : Statement; Position : Positive) return VString is
   begin
      return To_VString (SQLite.Column (Local_Handle_Statement, Position));
   end Column_Text;

   --
   function Column_Count return Natural is
   begin
      return SQLite.Column_Count (Handle_Statement);
   end Column_Count;

   function Column_Count (Local_Handle_Statement : Statement) return Natural is
   begin
      return SQLite.Column_Count (Local_Handle_Statement);
   end Column_Count;

   --
   function Column_Exists (Table_Name : VString; Column_Name : VString) return Boolean is
      Result : Natural := 0;
      Local_Statement : constant Statement :=  Prepare (
                   "SELECT COUNT(*) AS cntrec FROM pragma_table_info('" &
                   Table_Name & "') WHERE name='" & Column_Name & "'");
   begin

      Exec (+"SAVEPOINT SV_Column_Exists;");
      Step (Local_Statement);
      Result := Column_Integer (Local_Statement, 1);
      Reset (Local_Statement);
      Exec ("RELEASE SV_Column_Exists;");

      return (Result = 1);

   end Column_Exists;

   function Column_Exists (Table_Name : String; Column_Name : String) return Boolean is
   begin
      return Column_Exists (To_VString (Table_Name), To_VString (Column_Name));
   end Column_Exists;

   --
   function Column_Type (Position : Positive) return Natural is
   begin
      return To_Integer (Datatype'Image (SQLite.Column_Type (Handle_Statement, Position)));
   end Column_Type;

   function Column_Type (Local_Handle_Statement : Statement; Position : Positive) return Natural is
   begin
      return To_Integer (Datatype'Image (SQLite.Column_Type (Local_Handle_Statement, Position)));
   end Column_Type;

   --
   procedure Delete (Table_Name : VString ; Where_Condition : VString) is
      SQL_Request : VString;
   begin
      SQL_Request := "DELETE FROM " & Table_Name & " WHERE " & Where_Condition & ";";
      Log.Dbg ("Delete_From: " & SQL_Request);
      Exec (SQL_Request);
   end Delete;

   --
   procedure Exec (Command : String) is
   begin
      SQLite.Exec (Handle_Database, Command);
   exception
      when Fault : others =>
         Log.Err ("Sql.Exec > " & Command);
         Error (Fault);
   end Exec;

   procedure Exec (Command : VString) is
   begin
      SQLite.Exec (Handle_Database, To_String (Command));
   exception
      when Fault : others =>
         Log.Err ("Sql.Exec > " & Command);
         Error (Fault);
   end Exec;

   --
   function Error (Information : String ; Information_Extended : out VString) return Integer is
      Exception_Information : VString := To_VString (Information);
      Exception_Information_Extended : VString := +"Extended information unavailable, see error code number";
      Exception_Information_Number : Integer := 0;
   begin

      if not Empty (Exception_Information) then

         Exception_Information := Field_By_Index (Exception_Information, 1, CR);
         Exception_Information := Field_By_Index (Exception_Information, 2, "[");
         Exception_Information := Stript_Chars (Exception_Information, +" ]" & CRLF);

         -- If Error_Code here
         if Is_Numeric (Exception_Information) then
            Exception_Information_Number := To_Integer (Exception_Information);
            Exception_Information_Extended := Error_Display (Exception_Information_Number);
            Information_Extended := Exception_Information_Extended;
         end if;

      else
         --  Send raw information
         Information_Extended := +"No Error_Code present, send remaining raw information: " & Exception_Information;
      end if;

      return Exception_Information_Number;
   end Error;

    --
   procedure Error (Exception_Hook : AE.Exception_Occurrence) is
      Exception_Information : VString := To_VString (AE.Exception_Information (Exception_Hook));
      Exception_Information_Extended : VString := +"Extended information unavailable, see error code number";
      Exception_Information_Number : Integer := 0;
   begin

      if not Empty (Exception_Information) then

         Exception_Information := Field_By_Index (Exception_Information, 1, CR);
         Exception_Information := Field_By_Index (Exception_Information, 2, "[");
         Exception_Information := Stript_Chars (Exception_Information, +" ]" & CRLF);

         -- If Error_Code here
         if Is_Numeric (Exception_Information) then
            Exception_Information_Number := To_Integer (Exception_Information);
            Exception_Information_Extended := Error_Display (Exception_Information_Number);
         end if;

      else
         Exception_Information_Number := Status_No_Code;
      end if;

      Log.Err ("Genesix DB exception: " & Error_Display (Exception_Information_Number));

   end Error;

   function Error (Exception_Hook : AE.Exception_Occurrence) return Integer is
      Exception_Information : VString := To_VString (AE.Exception_Information (Exception_Hook));
      Exception_Information_Number : Integer := 0;
   begin

      if not Empty (Exception_Information) then

         Exception_Information := Field_By_Index (Exception_Information, 1, CR);
         Exception_Information := Field_By_Index (Exception_Information, 2, "[");
         Exception_Information := Stript_Chars (Exception_Information, +" ]" & CRLF);

         -- If Error_Code here
         if Is_Numeric (Exception_Information) then
            Exception_Information_Number := To_Integer (Exception_Information);
         end if;
      end if;

      return Exception_Information_Number;
   end Error;

   --
   function Error_Display (Error_Code : Integer) return VString is
      Error_String : VString;
   begin

      if Error_Code = Status_Need_Update then
         Error_String := +"Database need update";
      elsif Error_Code = Status_No_Code then
         Error_String := +"No Error_Code present";
      elsif Error_Code = Info_Ok then
         Error_String := +"Operation successful";
      elsif Error_Code = Info_Row then
         Error_String := +"Another row of output is available";
      elsif Error_Code = Info_Done then
         Error_String := +"Operation has completed";
      elsif Error_Code = Error_Generic then
         Error_String := +"Generic error code when no other error code can be used";
      elsif Error_Code = Error_Internal then
         Error_String := +"Internal error";
      elsif Error_Code = Error_Perm then
         Error_String := +"Permission error accessing a newly created database";
      elsif Error_Code = Error_Abort then
         Error_String := +"Operation was aborted";
      elsif Error_Code = Error_Busy then
         Error_String := +"Database is busy";
      elsif Error_Code = Error_Locked then
         Error_String := +"Database is locked";
      elsif Error_Code = Error_No_Mem then
         Error_String := +"No memory available";
      elsif Error_Code = Error_Readonly then
         Error_String := +"Database is in read only mode";
      elsif Error_Code = Error_Interrupt then
         Error_String := +"An operation was interrupted";
      elsif Error_Code = Error_In_Out then
         Error_String := +"Read or write disk error";
      elsif Error_Code = Error_Corrupt then
         Error_String := +"The database is corrupted";
      elsif Error_Code = Error_Not_Found then
         Error_String := +"Multiple contexts error, see SQLite documentation";
      elsif Error_Code = Error_Full then
         Error_String := +"The disk is full";
      elsif Error_Code = Error_Cant_Open then
         Error_String := +"Can't open database or working file";
      elsif Error_Code = Error_Protocol then
         Error_String := +"Locking protocol problem";
      elsif Error_Code = Error_Schema then
         Error_String := +"Schema has changed during operation";
      elsif Error_Code = Error_Too_Big then
         Error_String := +"String or blob too large";
      elsif Error_Code = Error_Constraint then
         Error_String := +"A SQL constraint violation occurred";
      elsif Error_Code = Error_Mismatch then
         Error_String := +"A type mismatch occurred";
      elsif Error_Code = Error_Misuse then
         Error_String := +"Incorrect use of SQLite interface";
      elsif Error_Code = Error_No_Lfs then
         Error_String := +"No Large File Support, database can't grow";
      elsif Error_Code = Error_Authent then
         Error_String := +"Not authorized SQL prepared";
      elsif Error_Code = Error_Range then
         Error_String := +"A parameter number argument in Bind or Column routines is out of range";
      elsif Error_Code = Error_Not_A_DB then
         Error_String := +"Not a SQLite database";
      else
         Error_String := +"Error_Code unknown: " & Trim_Left (To_VString (Error_Code));
      end if;

      return Error_String;

   end Error_Display;

   --
   function Get_Config (Parameter : VString) return VString is
      Local_Statement : Statement;
      Result_Statement : VString := +"";
   begin

      -- If Database opened, read if table and column exist
      if not Empty (Database_Full_Name) then
         if Table_Exists ("Sys_Config") then
            if Column_Exists ("Sys_Config", "Parameter") and
               Column_Exists ("Sys_Config", "Value") then

               Local_Statement := Prepare ("SELECT Value FROM Sys_Config WHERE Parameter='" & Parameter & "'");
               Exec ("SAVEPOINT SV_Get_Config;");
               Step (Local_Statement);
               Result_Statement := Column_Text (Local_Statement, 1);
               Reset (Local_Statement);
               Exec ("RELEASE SV_Get_Config;");

            end if;
         end if;
      end if;

      return Result_Statement;

   end Get_Config;

   function Get_Config (Parameter : String) return VString is
   begin
      return Get_Config (To_VString (Parameter));
   end Get_Config;

   --
   function Index_Exists (Index_Name : VString) return Boolean is

      Result : Natural := 0;
      Local_Statement : constant Statement :=  Prepare (
                   "SELECT COUNT(*) FROM sqlite_master WHERE type='index'" &
                   " and name='" & Index_Name & "';");
   begin
      Exec ("SAVEPOINT SV_Index_Exists;");
      Step (Local_Statement);
      Result := Column_Integer (Local_Statement, 1);
      Reset (Local_Statement);
      Exec ("RELEASE SV_Index_Exists;");

      return (Result = 1);

   end Index_Exists;

   --
   function Get_Version return VString is
   begin
      return "sqlite v" & To_VString (SQLite.Version);
   end Get_Version;

   procedure Insert (Table_Name : VString; Columns_Values : VString) is
      Current_Column, Current_Value, Insert_Columns_Names, Insert_Columns_Values,
      Sql_Request : VString := +"";
      Counter_Columns : constant Natural := Field_Count (Columns_Values, CD);
      Local_Statement : Statement;
   begin

      if not Table_Exists (Table_Name) then
         Log.Err ("Sql.Insert > Table does not exists: " & Table_Name);
         raise Table_Dont_Exists with To_String (Table_Name);
      end if;

      -- Read table schema
      Exec ("SAVEPOINT SV_Insert;");
      Local_Statement := Prepare (+"PRAGMA table_info (" & Table_Name & ")");

      Log.Dbg ("Counter_Columns: " & To_VString (Column_Count (Local_Statement)));

      --  Iterate through each column
      while Step (Local_Statement) loop
         -- Check each field in parameter against the current table's column
         for Index in 1 .. Counter_Columns loop
            Current_Column := Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 1, ND);

            --Log.Dbg ("Current_Columns_Values: " & Current_Column);
            --Log.Dbg ("Current_Column_Database: " & Column_Text (Local_Statement, 2));

            -- If field name and column name match
            if Current_Column = Column_Text (Local_Statement, 2) then
               -- Fill Name and Value, according to field type
               Current_Value := Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 2, ND);
               Insert_Columns_Names := Insert_Columns_Names & Current_Column & ",";

               if (Column_Text (Local_Statement, 3) = "INTEGER") then
                  Insert_Columns_Values := Insert_Columns_Values & Current_Value & ",";
               elsif (Column_Text (Local_Statement, 3) = "TEXT") then
                  -- Single quotes outside string and, inside string, escape single quote with pair of single quotes
                  Insert_Columns_Values := Insert_Columns_Values & "'" & Replace_Pattern (Current_Value, +"'", +"''") & "',";
               elsif (Column_Text (Local_Statement, 3) = "BLOB") then
                  -- Single quotes outside string and, inside string, escape single quote with pair of single quotes
                  Insert_Columns_Values := Insert_Columns_Values & "'" & Replace_Pattern (Current_Value, +"'", +"''") & "',";
               end if;
            end if;
         end loop;
      end loop;

      Reset (Local_Statement);
      Exec ("RELEASE SV_Insert;");

      Log.Dbg ("Insert_Columns_Names: " & Insert_Columns_Names);
      Log.Dbg ("Insert_Columns_Values: " & Insert_Columns_Values);

      -- If at least one Field/Value pair has been processed
      if (Index (Insert_Columns_Names, ",") > 0) and
         (Index (Insert_Columns_Values, ",") > 0) then

         -- Trailing comma deletion
         Insert_Columns_Names := Slice (Insert_Columns_Names, 1, Length (Insert_Columns_Names) - 1);
         Insert_Columns_Values := Slice (Insert_Columns_Values, 1, Length (Insert_Columns_Values) - 1);

         Sql_Request := "INSERT INTO " & Table_Name & " (" & Insert_Columns_Names & ") VALUES (" & Insert_Columns_Values & ");";
         Log.Dbg ("Insert_Into: " & Sql_Request);

         Exec (Sql_Request);

      end if;

   end Insert;

   function Last_Insert_RowID return Integer_64 is
   begin
      return SQLite.Last_Insert_Row (Handle_Database);
   end Last_Insert_RowID ;

   function Last_RowID (Table_Name : VString) return Integer is
      Result : Natural := 0;
      Local_Statement : constant Statement :=  Prepare (+"SELECT MAX(rowid) FROM " & Table_Name & ";");
   begin
      Exec (+"SAVEPOINT SV_Last_RowID;");
      Step (Local_Statement);
      Result := Column_Integer (Local_Statement, 1);
      Reset (Local_Statement);
      Exec (+"RELEASE SV_Last_RowID;");
      return Result;
   end Last_RowID ;

   --
   procedure Open (Database_File_Name : VString) is
   begin
      Handle_Database := SQLite.Open (To_String (Database_File_Name));
   end Open;

   --
   procedure Prepare (Statement_To_Prepare : VString) is
   begin
      Handle_Statement := SQLite.Prepare (Handle_Database, To_String (Statement_To_Prepare));
   end Prepare;

   function Prepare (Statement_To_Prepare : VString) return SQLite.Statement is
   begin
      return SQLite.Prepare (Handle_Database, To_String (Statement_To_Prepare));
   end Prepare;

   --
   function Read (Table_Name : VString; Columns : VString; Condition : VString := +"") return VString is
      Sql_Request, Sql_Result : VString := +"";
      Counter_Columns : constant Natural:= Field_Count (Columns, ",");
      Local_Statement : Statement;
   begin

      -- Read table schema
      Exec ("SAVEPOINT SV_Read;");
      Sql_Request := +"SELECT " & Columns & " FROM " & Table_Name & " " & Condition & ";";
      Log.Dbg ("Sql_Request: " & Sql_Request);
      Local_Statement := Prepare (Sql_Request);

      --Log.Dbg ("Sql_Column_Count: " & Integer'Image (Column_Count (Local_Statement)));
      --Log.Dbg ("Counter_Columns: " & To_VString (Integer'Image(Counter_Columns)));

      -- Iterate result(s) line(s)
      while Step (Local_Statement) loop
         -- Iterate choosen columns
         for Index in 1..Counter_Columns loop
            --Log.Dbg ("Current_Column_Text: " & Column_Text (Local_Statement, Index));
            Sql_Result := Sql_Result & Column_Text (Local_Statement, Index) & CD;
         end loop;
         if (Index (Sql_Result, CD) > 0) then
            -- Delete last CD and add row delimiter
            Sql_Result := Slice (Sql_Result, 1, Length (Sql_Result) - 1) & RD;
         end if;
      end loop;

      Reset (Local_Statement);
      Exec ("RELEASE SV_Read;");

      -- Delete last RD
      if Length (Sql_Result) >= 2 then -- to handle one digit answer with a trailing RD = 2 chars
         --  Old code
         --  Sql_Result := Slice (Sql_Result, 1, Length (Sql_Result) - 1);
         --  New code
         if Slice (Sql_Result, Length (Sql_Result), Length (Sql_Result)) = RD then
            Sql_Result := Slice (Sql_Result, 1, Length (Sql_Result) - 1);
         end if;
      end if;

      Log.Dbg ("Read: " & Sql_Result);

      return Sql_Result;

   end Read;

   --
   function Search (Table_Name : VString; Condition : VString) return Boolean is
   Search : constant VString := Read (Table_Name, +"*", Condition);
   Result : Boolean := False;
   begin
      -- old code was too straight
      -- return (length (Read (Table_Name, +"*", Condition) > 0)

      -- Check for succesful search answer
      if Search = ("1" & RD) then
         --Log.Msg ("Sql.Read result (result 1\): " & To_Hex (Search));
         Result := True;
      else
         --  Check for not empty answer
         if Length (Search) > 0 then
            --Log.Msg ("Sql.Read result (not empty): " & To_Hex (Search));
            Result := True;
         end if;
      end if;

      return Result;
   end Search;

   --
   function Row_Count (Table_Name : String; Option : String := "*") return Natural is
      Result : Natural := 0;
      Local_Statement : constant Statement :=  Prepare (+"SELECT COUNT(" & Option & ") FROM " & Table_Name & ";");
   begin
      Exec (+"SAVEPOINT SV_Row_Count;");
      Step (Local_Statement);
      Result := Column_Integer (Local_Statement, 1);
      Reset (Local_Statement);
      Exec (+"RELEASE SV_Row_Count;");
      return Result;
   end Row_Count;

   --
   procedure Reset is
   begin
     SQLite.Reset (Handle_Statement);
   end Reset;

   procedure Reset (Local_Handle_Statement : Statement) is
   begin
      SQLite.Reset (Local_Handle_Statement);
   end Reset;

   function Schema_Need_Update (Database_FullName : VString ; Major : Natural; Minor : Natural) return Integer is
      Database_Version : Natural;
      Schema_Version : constant Natural := (Major * 10) + Minor;
   begin

      --  if not Fls.Exists (Database_FullName) then
      --     Log.Err ("Sql.Schema_Need_Update: Can't open database: " & Database_FullName);
      --     raise Table_Dont_Exists with To_String (Database_FullName);
      --  end if;

      -- Preload Database definition
      Schema_Load (Database_Name, To_String (Database_FullName));

      -- sqlite doc : The DELETE journaling mode is the normal behavior. In the DELETE mode, the rollback
      -- journal is deleted at the conclusion of each transaction. Indeed, the delete operation is the action
      -- that causes the transaction to commit.
      Schema_Load (Database_Pragma,"journal_mode","DELETE"); -- .."journal_mode","WAL");

      -- sqlite doc : When synchronous is FULL (2), the SQLite database engine will use the xSync method of the
      -- VFS to ensure that all content is safely written to the disk surface prior to continuing. This ensures
      -- that an operating system crash or power failure will not corrupt the database. FULL synchronous is very
      -- safe, but it is also slower. FULL is the most commonly used synchronous setting when not in WAL mode.
      Schema_Load (Database_Pragma,"synchronous","FULL");

      -- sqlite doc : As of SQLite version 3.6.19, the default setting for foreign key enforcement is OFF.
      Schema_Load (Database_Pragma,"foreign_keys","ON");

      -- Open DB and eventually apply pragmas
      for I of Schema loop
         if I.Command = Database_Name then
            Database_Full_Name := I.Name;
            Log.Dbg ("Open Database_Name: " &  Database_Full_Name);
            Open (Database_Full_Name);
         elsif I.Command = Database_Pragma then
            Log.Dbg ("Load Database_Pragma: " & I.Name & "=" & I.Attribute);
            Exec ("PRAGMA " & I.Name & "=" & I.Attribute);
         else
            exit;
         end if;
      end loop;

      -- Preload Sys_Config table definition
      Schema_Load (Table_Name,        "Sys_Config");
      Schema_Load (Column_Name,       "Parameter", "TEXT");
      Schema_Load (Column_Constraint, "Parameter", "UNIQUE");
      Schema_Load (Table_Constraint,  "Parameter", "PRIMARY KEY");
      Schema_Load (Column_Name,       "Value",     "TEXT");
      Schema_Load (Index_Name, "Idx_Config_Parameter","Parameter");

      -- Preload Sys_Schema table definition
      Schema_Load (Table_Name,        "Sys_Schema");
      Schema_Load (Column_Name,       "Entity",  "TEXT");
      Schema_Load (Column_Name,       "Comment", "TEXT");

      -- Store schema version in private memory for later writing of this
      -- current version in Schema_Update, which must be done at the very
      -- end of the update process to ensure the completion of the update
      Version_Major := Major;
      Version_Minor := Minor;

      -- Database schema version
      Database_Version := (To_Integer (Field_By_Index (Get_Config ("Schema_Version"), 1, ".")) * 10) +
        To_Integer (Field_By_Index (Get_Config ("Schema_Version"), 2, "."));

      -- False if Database_Version is >= Schema_Version => no need updating
      if (Database_Version < Schema_Version) then
         return Status_Need_Update;
      else
         return Info_Ok;
      end if;

   exception
      when Fault : Ada.IO_Exceptions.Data_Error =>
         return Error (Fault);
      when Fault : Ada.IO_Exceptions.Status_Error =>
         return Error (Fault);

   end Schema_Need_Update;

   function Schema_Need_Update (Database_FullName : String ; Major : Natural; Minor : Natural) return Integer is
   begin
      return Schema_Need_Update (To_VString (Database_FullName), Major, Minor);
   end Schema_Need_Update;

   procedure Schema_Update is

      Current_Table_Name, Current_Table_Constraint, Current_Table_Comment,
      Current_Column_Name, Current_Column_Type, Current_Column_Constraint,
      Current_Column_Comment, Current_Index_Name, Current_Index_Key,
      Current_Index_Constraint : VString := +"";

      Current_Table_Not_Exists : Boolean := False;
      Columns_Counter, Index_Counter, Constraint_Counter : Natural := 0;

      --  type Parsing_States is (Idle, Init, Table, Column, Index,
      --                          Table_Name, Table_Constraint,
      --                          Column_Name, Column_Constraint,
      --                          Index_Name, Constraint);
      --
      --  State : Parsing_States := Idle;
      --  State_Memory :  Parsing_States := Init;

      procedure Clear_Column is
      begin
         Current_Column_Name := +"";
         Current_Column_Type := +"";
         Current_Column_Comment := +"";
         Current_Column_Constraint := +"";
         Constraint_Counter := 0;
      end Clear_Column;

      procedure Create_Column is
      begin

         -- Test if non empty column name to handle table break
         -- when previous table column already exists
         if not Empty (Current_Column_Name) then
            if Column_Exists (Current_Table_Name, Current_Column_Name) then
               Log.Dbg ("Existing Table: " & Current_Table_Name &
                          " - Existing Column: " & Current_Column_Name &
                          " " & Current_Column_Type &
                          " " & Current_Column_Constraint);
            else
               Log.Msg ("Table: " & Current_Table_Name &
                          " - Create Column: " & Current_Column_Name &
                          " " & Current_Column_Type &
                          " " & Current_Column_Constraint);
               Exec ("ALTER TABLE " & Current_Table_Name  &
                       " ADD COLUMN " & Current_Column_Name &
                       " " & Current_Column_Type &
                       " " & Current_Column_Constraint & ";");
            end if;

            -- Create column comment
            if Table_Exists ("Sys_Schema") then
               if Column_Exists ("Sys_Schema", "Entity") and
                  Column_Exists ("Sys_Schema", "Comment") then
                  Exec ("INSERT INTO Sys_Schema (Entity, Comment) VALUES ('" &
                             Current_Table_Name & " " &
                             Current_Column_Name & " " &
                             Current_Column_Type & " " &
                             Current_Column_Constraint &
                             "', '" & Current_Column_Comment & "')");
               end if;
            end if;

            Clear_Column;

         end if;
      end Create_Column;

      procedure Clear_Index is
      begin
         Current_Index_Name := +"";
         Current_Index_Key := +"";
         Current_Index_Constraint := +"";
      end Clear_Index;

      procedure Create_Index is
      begin
         -- Test if non empty column name to handle table break
         -- when previous table index already exists
         if not Empty (Current_Index_Name) then
            if Index_Exists (Current_Index_Name) then
               Log.Dbg ("Existing Table: " & Current_Table_Name &
                          " - Existing Index: " & Current_Index_Name &
                          " " & Current_Index_Key &
                          " " & Current_Index_Constraint);
            else
               Log.Msg ("Table: " & Current_Table_Name &
                          " - Creating Index: " & Current_Index_Name &
                          " " & Current_Index_Key &
                          " " & Current_Index_Constraint);

               Exec ("CREATE " & Current_Index_Constraint &
                     " INDEX " & Current_Index_Name  &
                        " ON " & Current_Table_Name &
                          " (" & Current_Index_Key & ");");
            end if;
            Clear_Index;
         end if;
      end Create_Index;

      procedure Create_Table is
      begin
         if Current_Table_Not_Exists then
            Log.Msg ("Create Table: " & Current_Table_Name &
                       " - Create Column: " & Current_Column_Name &
                       " " & Current_Column_Type &
                       " " & Current_Column_Constraint &
                       " " & Current_Table_Constraint);

            Exec (+"CREATE TABLE " & Current_Table_Name &
                    " (" & Current_Column_Name &
                    " " & Current_Column_Type &
                    " " & Current_Column_Constraint &
                    " " & Current_Table_Constraint & ")");

            -- Record table comment
            if Table_Exists ("Sys_Schema") then
               if Column_Exists ("Sys_Schema", "Entity") and
                 Column_Exists ("Sys_Schema", "Comment") then
                  Exec ("INSERT INTO Sys_Schema (Entity, Comment) VALUES ('" &
                          Current_Table_Name & "', '" & Current_Table_Comment & "')");
               end if;
            end if;

            Current_Table_Not_Exists := False;
            Current_Table_Constraint := +"";
            Current_Table_Comment := +"";
            Clear_Column;
         else
            Create_Column;
         end if;
      end Create_Table;

      procedure Finalize_Loop is
      begin
         Create_Table;
         if Columns_Counter >= 2 then
            Create_Column;
         end if;
         Create_Index;
      end Finalize_Loop;

   begin

      -- Empty Sys_Table for later filling from scratch
      if Table_Exists ("Sys_Schema") then
         Exec ("DELETE FROM Sys_Schema;");
         Exec ("VACUUM;");
      end if;

      for I of Schema loop

         --Schema_Command_List.Put (I.Command);
         --Tio.Put_Line (" - " & I.Name & " - " & I.Attribute);

         if    I.Command = Null_Command then
            null;

         --  elsif I.Command = Database_Name then
         --     Database_Full_Name := To_Lower (I.Name) & ".db";
         --     Log.Dbg ("Open Database_Name: " &  Database_Full_Name);
         --     Open (Database_Full_Name);
         --
         --  elsif I.Command = Database_Pragma then
         --     Log.Dbg ("Load Database_Pragma: " & I.Name & "=" & I.Attribute);
         --     Exec ("PRAGMA " & I.Name & "=" & I.Attribute);

         elsif I.Command = Table_Name then

            -- Wait first column read if table has to be created as creating a
            -- table without on column is not supported in SQLite nor SQL standard
            if Columns_Counter = 1 then
               Create_Table;
            end if;

            -- Process eventually a remaining column
            Create_Column;

            -- Last table command must have been read to eventually create the last index
            Create_Index;

            Current_Table_Name := I.Name;
            Current_Table_Comment := I.Comment;
            Log.Dbg ("Load Table_Name: " & Current_Table_Name & " " & Current_Table_Comment);

            Current_Table_Not_Exists := not Table_Exists (I.Name);
            Log.Dbg ("Table_Name exists: " & To_VString (not Current_Table_Not_Exists));

            Columns_Counter := 0;

         elsif I.Command = Table_Constraint then

            Current_Table_Constraint := I.Attribute;
            Log.Dbg ("Load Table_Constraint: " & Current_Table_Constraint);

         elsif I.Command = Column_Name then

            -- Wait first column read if table has to be created as creating a
            -- table without on column is not supported in SQLite nor SQL standard
            if Columns_Counter = 1 then
               Create_Table;
            else
               -- If previous line was a column constrain
               --if (Constraint_Counter > 0) then
                  Create_Column;
               --end if;
            end if;

            Current_Column_Name := I.Name;
            Current_Column_Type := I.Attribute;
            Current_Column_Comment := I.Comment;
            Log.Dbg ("Load Column_Name: " & Current_Column_Name & " " & Current_Column_Type & " " & Current_Column_Comment) ;

            Columns_Counter := Columns_Counter + 1;

            -- Create additional column only if table is already created with
            -- at least one existing column
            --if Columns_Counter >= 2 then
            --   Create_Column;
            --end if;

         elsif I.Command = Column_Constraint then

            Current_Column_Constraint := I.Attribute;
            Log.Dbg ("Load Column_Constraint: " & Current_Column_Constraint);

            Constraint_Counter := Constraint_Counter + 1;

         elsif I.Command = Index_Name then

            -- Previous table command could be an column creation
            if Columns_Counter >= 2 then
               Create_Column;
            end if;

            -- Previous table command could be an index creation
            Create_Index;

            Current_Index_Name := I.Name;
            Current_Index_Key := I.Attribute;
            Log.Dbg ("Load Index_Name: " & Current_Index_Name & " " & Current_Index_Key);

            Index_Counter := Index_Counter + 1;

         elsif I.Command = Index_Constraint then

            Current_Index_Constraint := I.Attribute;
            Log.Dbg ("Load Index_Constraint: " & Current_Index_Constraint);

         end if;

         -- Finalize
         if I = Schema.Last_Element then
            Finalize_Loop;

            -- Update schema
            Set_Config (+"Schema_Version", To_VString (Version_Major) & "." & To_VString (Version_Minor));

            -- Default setup in config at init
            if Empty (Get_Config ("Cluster_Default")) then
               Set_Config ("Cluster_Default", "1");
            end if;

         end if;

      end loop;

      -- Display Schema list for tests
      --  for I of Schema loop
      --     Schema_Command_List.Put (I.Command);
      --     Tio.Put_Line (" - " & I.Name & " - " & I.Attribute);
      --  end loop;

   end Schema_Update;

   procedure Set_Config (Parameter : VString ; Value : VString) is
   begin

      -- If Database opened, read if table and column exist
      if not Empty (Database_Full_Name) then
         if Table_Exists ("Sys_Config") then
            if Column_Exists ("Sys_Config", "Parameter") and
               Column_Exists ("Sys_Config", "Value") then

               Exec ("UPDATE Sys_Config SET Value='" &
                       Value & "' WHERE Parameter='" & Parameter & "'");
               Exec ("INSERT INTO Sys_Config (Parameter, Value) SELECT '" &
                       Parameter & "', '" & Value & "' WHERE (Select Changes() = 0)");
            end if;
         end if;
      end if;

   end Set_Config;

   procedure Set_Config (Parameter : String ; Value : String) is
   begin
      Set_Config (To_VString (Parameter), To_VString (Value));
   end Set_Config;

   procedure Schema_Load (Command: in Schema_Command := Null_Command;
                            Name : in String := "";
                       Attribute : in String := "";
                         Comment : in String := "") is
   begin
      Schema.Append ((Command, To_VString (Name), To_VString (Attribute), To_VString (Comment)));
   end Schema_Load;

   --
   procedure Step is
   begin
     SQLite.Step (Handle_Statement);
   end Step;

   procedure Step (Local_Handle_Statement : Statement) is
   begin
      SQLite.Step (Local_Handle_Statement);
   end Step;

   function Step return Boolean is
   begin
     return SQLite.Step (Handle_Statement);
   end Step;

   function Step (Local_Handle_Statement : Statement) return Boolean is
   begin
      return SQLite.Step (Local_Handle_Statement);
   end Step;

   --
   function Table_Exists (Table_Name : String) return Boolean is
   begin
      return SQLite.Table_Exists (Handle_Database, Table_Name);
   end Table_Exists;

   function Table_Exists (Table_Name : VString) return Boolean is
   begin
      return SQLite.Table_Exists (Handle_Database, To_String(Table_Name));
   end Table_Exists;

   procedure Update (Table_Name : VString; Columns_Values : VString; Where_Condition : VString) is
      Current_Column, Current_Value, Update_Columns_Values,
      Sql_Request : VString := +"";
      Counter_Columns : constant Natural := Field_Count (Columns_Values, CD);
      Local_Statement : Statement;
   begin

      -- Read table schema
      Exec ("SAVEPOINT SV_Update;");
      Local_Statement := Prepare (+"PRAGMA table_info (" & Table_Name & ")");

      --  Iterate through each column
      while Step (Local_Statement) loop
         -- Check each field in parameter against the current table's column
         for Index in 1 .. Counter_Columns loop
            Current_Column := Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 1, ND);
            -- If field name and column name match

            --Table_Column := Column_Text (Local_Statement, 2);
            --if Current_Column = Table_Column then

            if Current_Column = Column_Text (Local_Statement, 2) then
               -- Fill Name and Value, according to field type
               Current_Value := Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 2, ND);
               Update_Columns_Values := Update_Columns_Values & Current_Column & " = ";

               if (Column_Text (Local_Statement, 3) = "INTEGER") then
                  Update_Columns_Values := Update_Columns_Values & Current_Value & ",";
               elsif (Column_Text (Local_Statement, 3) = "TEXT") then
                  -- Single quotes outside string and, inside string, escape single quote with pair of single quotes
                  Update_Columns_Values := Update_Columns_Values & "'" & Replace_Pattern (Current_Value, +"'", +"''") & "',";
               end if;
               exit; -- No need to iterate further after match
            end if;
         end loop;
      end loop;

      Reset (Local_Statement);
      Exec ("RELEASE SV_Update;");

      Log.Dbg ("Update_Columns_Values: " & Update_Columns_Values);

      -- If at least one Field/Value pair has been processed
      if (Index (Update_Columns_Values, ",") > 0) then

         -- Trailing comma deletion
         Update_Columns_Values := Slice (Update_Columns_Values, 1, Length (Update_Columns_Values) - 1);

         Sql_Request := "UPDATE " & Table_Name & " SET " & Update_Columns_Values & " WHERE " & Where_Condition & ";";
         Log.Dbg ("Update: " & Sql_Request);

         Exec (Sql_Request);

      end if;

   end Update;

------------------------------------------------------------------------------
end v20.Sql;
------------------------------------------------------------------------------
