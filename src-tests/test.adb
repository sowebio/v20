------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      test.adb
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
------------------------------------------------------------------------------
--  @summary
--  V20 library project file
--
--  @description
--  Build application and documentation
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  20210317 - 0.1 - sr - initial release
--  20210402 - 0.2 - sr - basic cfg log prg tio vst packages coded
--  20210403 - 0.3 - sr - template with command line handling
--  20210730 - 0.4 - sr - cosmetics enhancements
--  20211220 - 0.5 - sr - cursor on/off control
--  20211229 - 0.6 - sr - sqlite integration
------------------------------------------------------------------------------

with GNAT.Command_Line;
with GNAT.OS_Lib;
with GNAT.Strings;

with SQLite;       -- For Sql_Low_Level_Demo
with Interfaces.C; -- For Sql_Low_Level_Demo

with v20; use v20;
with v20.Cfg;
--with v20.Crl; use v20.Crl; -- for operators
with v20.Fls;
with v20.Log;
with v20.Prg;
with v20.Tio;
with v20.Sys;
with v20.Sql;
with v20.Vst; use v20.Vst;

procedure Test is

   package GCL renames GNAT.Command_Line;
   package GOL renames GNAT.OS_Lib;
   package GS renames GNAT.Strings;

   TAB : constant VString := Vst.To_VString(Character'Val (9)); -- Tab

   type Animation is array (1 .. 7) of Character;
   Progress : constant Animation := ('/', '-', '\', '|', '/', '-', '|');
   Result : Integer := 0;

   Config : GCL.Command_Line_Configuration;
   Cursor_Animation_1 : aliased Boolean := False;
   Cursor_Animation_2 : aliased Boolean := False;
   Memory_Reports : aliased Boolean := False;
   String_Option : aliased GS.String_Access;
   Long_Option : aliased Integer := 0;

   ----------------------------------------------------------------------------
   procedure Sql_High_Level_Demo is
      Count : constant Integer := 10;
      Columns, Counter : Natural := 0;
      Key, Value : VString := +"";
   begin

      --  Version
      Tio.Line;
      Tio.Put_Line ("Version SQLite: " & Sql.Get_Version);

      --  Open
      Tio.Line;
      Tio.Put_Line ("Create: DB");

      Sql.Open (+"sqlite_high_level_test.db");

      --  Write ahead log transaction mode, safe write to avoid corruption
      Sql.Exec (+"PRAGMA journal_mode=WAL; PRAGMA synchronous=FULL");

      -- Table setup
      Sql.Exec (+"DROP TABLE IF EXISTS test_table");
      Sql.Exec (+"CREATE TABLE test_table (key TEXT PRIMARY KEY, value TEXT, vnum INTEGER)");

      -- Read table schema
      Tio.Line;
      Tio.Put_Line ("Read test_table schema:");

      Sql.Exec (+"BEGIN TRANSACTION;");

      Sql.Prepare (+"PRAGMA table_info (test_table)");
      Columns := Sql.Column_Count;
      Tio.Put_Line ("Column count: " & To_VString (Integer'Image(Columns)));

      -- Iterate result(s) line(s)
      Tio.Put_Line ("         cid    name    type  notnull dfltval  pkey ");

      while Sql.Step loop
         Counter := Counter + 1;
         Tio.Put ("Row " & To_VString (Integer'Image(Counter)) & " : " );
         for Index in 1..Columns loop
            Tio.Put (Sql.Column_Text (Index));
            Tio.Put (TAB);
         end loop;
         Tio.Line;
      end loop;

      Sql.Reset;
      Sql.Exec (+"COMMIT;");

      --  Insert
      Tio.Line;
      Tio.Put_Line ("Insert:");

      Sql.Prepare (+"INSERT INTO test_table VALUES (?, ?, ?)");

      for Index in 1..Count loop
         Sql.Exec (+"BEGIN TRANSACTION;");
         --  Primary key so keys must be unique
         Key := "key" & Trim_Left(To_VString (Integer'Image(Index)));
         Value := "value"& Trim_Left(To_VString (Integer'Image(Index)));
         Tio.Put_Line ("Insert Key: " & Key & " with value: " & Value);
         Sql.Bind (1, Key);
         Sql.Bind (2, Value);
         Sql.Bind (3, Index);
         Sql.Step;
         Sql.Reset;
         Sql.Exec (+"COMMIT;");
      end loop;

      -- Add another keys with same value for further test
      Sql.Exec (+"INSERT INTO test_table (Key, Value, vnum) VALUES ('key11','value4',11);");
      Sql.Exec (+"INSERT INTO test_table (Key, Value, vnum) VALUES ('key12','value4',12);");
      Sql.Exec (+"INSERT INTO test_table (Key, Value, vnum) VALUES ('key13','value4',13);");
      Sql.Exec (+"INSERT INTO test_table (Key, Value, vnum) VALUES ('key14','value4',14);");

      Tio.Put ("Last_Insert_Row_ID: ");
      Tio.Put_Line (Sql.Last_Insert_RowID);

      --  Search (value4)
      Tio.Line;
      Tio.Put_Line ("Search (value4):");

      Sql.Prepare (+"SELECT key, value, vnum FROM test_table WHERE value=? ORDER BY key");

      Sql.Exec (+"BEGIN TRANSACTION;");
      Sql.Bind (1, +"value4");

      Columns := Sql.Column_Count;
      Tio.Put_Line ("Column count: " & To_VString (Integer'Image(Columns)));

      -- Iterate result(s) line(s)
      while Sql.Step loop
         Counter := Counter + 1;
         Tio.Put ("Row " & To_VString (Integer'Image(Counter)) & " : " );
         for Index in 1..Columns loop
            -- Tio.Put (Sql.Column_Text (Index) & " (T" & Trim_Left (To_VString (Sql.Datatype'Image (Sql.Column_Type (Index)))) & ") ");
            Tio.Put (Sql.Column_Text (Index) & " (T" & Trim_Left (To_VString (Sql.Column_Type (Index))) & ") ");
         end loop;
         Tio.Line;
      end loop;

      Sql.Reset;
      Sql.Exec (+"COMMIT;");

      --  Update (String)
      Tio.Line;
      Tio.Put_Line ("Update:");

      Sql.Prepare (+"UPDATE test_table SET value=? WHERE key=?");

      for Index in 1..Count loop
         Sql.Exec (+"BEGIN TRANSACTION;");
         Key := +"key" & Trim_Left (To_VString(Integer'Image(Index)));
         Value := +"value"& Trim_Left (To_VString(Integer'Image(Index + 10)));
         Tio.Put_Line ("Update Key: " & Key & " with new value: " & Value);
         Sql.Bind (1, Key);
         Sql.Bind (2, Value);
         Sql.Step;
         Sql.Reset;
         Sql.Exec (+"COMMIT;");
      end loop;

      --  Delete
      Tio.Line;
      Tio.Put_Line ("Delete:");

      Sql.Prepare (+"DELETE FROM test_table WHERE key=?");

      for Index in 1..Count / 3 loop
         Sql.Exec (+"BEGIN TRANSACTION;");
         Key := +"key" & Trim_Left (To_VString(Integer'Image(Index)));
         Tio.Put_Line ("Delete row with Key: " & Key);
         Sql.Bind (1, Key);
         Sql.Step;
         Sql.Reset;
         Sql.Exec (+"COMMIT;");
      end loop;

   end Sql_High_Level_Demo;

   ----------------------------------------------------------------------------
   procedure Sql_Low_Level_Demo is -- For reference only (not part of v20 API)
      DBH : SQLite.Data_Base;
      Count : constant Integer := 10;
   begin

      --  Version
      Tio.Line;
      Tio.Put_Line ("Version SQLite: " & SQLite.Version);

      --  Open
      Tio.Line;
      Tio.Put_Line ("Create: DB");

      DBH := SQLite.Open ("sqlite_low_level_test.db");

      --  Write ahead log transaction mode, safe write to avoid corruption
      SQLite.Exec (DBH, "PRAGMA journal_mode=WAL; PRAGMA synchronous=FULL");

      -- Table setup
      SQLite.Exec (DBH, "DROP TABLE IF EXISTS test_table");
      SQLite.Exec (DBH, "DROP TABLE IF EXISTS node");

      SQLite.Exec (DBH, "CREATE TABLE test_table (key TEXT PRIMARY KEY, value TEXT, vnum INTEGER)");

      SQLite.Exec (DBH, "CREATE TABLE node  " &
                     "(" &
                      "Node_Num INTEGER UNIQUE, " & -- Node number (201) 1...40
                      "Node_Cpu INTEGER,     " & -- Node cpu (15) 0..n
                      "Node_Memory INTEGER,  " & -- Node memory (64) GB
                      "Node_Disk INTEGER,    " & -- Node disk (980) GB
                      "PRIMARY KEY (Node_Num)" &
                     ")");

      SQLite.Exec (DBH, "CREATE UNIQUE INDEX Idx ON Node (Node_Num)");

      -- Generate:
      -- Node domain name: n(201)c(1).(genesix.org)
      -- Node IP: 10.0.(1).(201)
      -- maximum ressource information for instance ressource limits computing

      --  Insert
      Tio.Line;
      Tio.Put_Line ("Insert:");

      declare
         DBQ : constant SQLite.Statement := SQLite.Prepare ( DBH, "INSERT INTO test_table VALUES (?, ?, ?)");
      begin

         for Index in 1..Count loop

            SQLite.Exec (DBH, "BEGIN TRANSACTION;");
            declare
               --  Primary key so keys must be unique
               Key   : aliased  String := To_String (+"key" & Trim_Left(To_VString(Integer'Image(Index))));
               Value : aliased  String := To_String (+"value"& Trim_Left(To_VString(Integer'Image(Index))));
            begin
               Tio.Put_Line ("Insert Key: " & Key & " with value: " & Value);
               SQLite.Bind (DBQ, 1, Key'Access);
               SQLite.Bind (DBQ, 2, Value'Access);
               SQLite.Bind (DBQ, 3, Interfaces.C.int (0) );
               SQLite.Step (DBQ);
               SQLite.Reset (DBQ);
            end;
            SQLite.Exec (DBH, "COMMIT;");
         end loop;

         -- Add another keys with same value for further test
         SQLite.Exec (DBH, "INSERT INTO test_table (Key, Value, vnum) VALUES ('key11','value4',11);");
         SQLite.Exec (DBH, "INSERT INTO test_table (Key, Value, vnum) VALUES ('key12','value4',12);");
         SQLite.Exec (DBH, "INSERT INTO test_table (Key, Value, vnum) VALUES ('key13','value4',13);");
         SQLite.Exec (DBH, "INSERT INTO test_table (Key, Value, vnum) VALUES ('key14','value4',14);");

      end;

      --  Search (key, with string)
      Tio.Line;
      Tio.Put_Line ("Search (key, with string):");

      declare
         DBQ : constant SQLite.Statement := SQLite.Prepare ( DBH, "SELECT value FROM test_table WHERE key=?");
      begin

         SQLite.Exec (DBH, "BEGIN TRANSACTION;");
         declare
            --  Primary key so key must be unique
            Key   : String := "key4";
            Value : String := "value4";
         begin
            SQLite.Bind (DBQ, 1, Key);
            SQLite.Step (DBQ);
            Tio.Put_Line ("Column count: " & To_VString(Integer'Image(SQLite.Column_Count (DBQ))));
            if Value = SQLite.Column (DBQ, 1) then
               Tio.Put_Line ("Search value equal to value found: " & Value);
            else
               Tio.Put_Line ("Error in SELECT, searched for: " & Key &
                               ", found " & SQLite.Column (DBQ, 1) & ", expected " & Value);
            end if;
            SQLite.Reset (DBQ);
         end;
         SQLite.Exec (DBH, "COMMIT;");
      end;

      --  Search (key, with access)
      Tio.Line;
      Tio.Put_Line ("Search (key, with access):");

      declare
         DBQ : constant SQLite.Statement := SQLite.Prepare ( DBH, "SELECT value FROM test_table WHERE key=?");
      begin

         SQLite.Exec (DBH, "BEGIN TRANSACTION;");
         declare
            --  Primary key so key must be unique
            Key   : aliased String := "key4";
            Value : aliased String := "value4";
         begin
            SQLite.Bind (DBQ, 1, Key'Access);
            SQLite.Step (DBQ);
            Tio.Put_Line ("Column count: " & To_VString(Integer'Image(SQLite.Column_Count (DBQ))));
            if Value = SQLite.Column (DBQ, 1) then
               Tio.Put_Line ("Search value equal to value found: " & Value);
            else
               Tio.Put_Line ("Error in SELECT, searched for: " & Key &
                               ", found " & SQLite.Column (DBQ, 1) & ", expected " & Value);
            end if;
            SQLite.Reset (DBQ);
         end;
         SQLite.Exec (DBH, "COMMIT;");
      end;

      --  Search (value4)
      Tio.Line;
      Tio.Put_Line ("Search (value4):");

      declare
         DBQ : constant SQLite.Statement := SQLite.Prepare ( DBH, "SELECT key, value, vnum FROM test_table WHERE value=? ORDER BY key");
         Columns : Natural := 0;
         Counter : Natural := 0;
      begin

         SQLite.Exec (DBH, "BEGIN TRANSACTION;");
         declare
            Value : aliased String := "value4";
         begin
            SQLite.Bind (DBQ, 1, Value'Access);

            Columns := SQLite.Column_Count (DBQ);
            Tio.Put_Line ("Column count: " & To_VString(Integer'Image(Columns)));

            -- Iterate result(s) line(s)
            while SQLite.Step (DBQ) loop
               Counter := Counter + 1;
               Tio.Put ("Row " & To_VString(Integer'Image(Counter)) & " : " );
               for Index in 1..Columns loop
                  Tio.Put (SQLite.Column (DBQ, Index) & " (T" & Trim_Left(To_VString(SQLite.Datatype'Image (SQLite.Column_Type (DBQ, Index)))) & ") ");
               end loop;
               Tio.Line;
            end loop;

            SQLite.Reset (DBQ);
         end;
         SQLite.Exec (DBH, "COMMIT;");

      end;

      --  Update
      Tio.Line;
      Tio.Put_Line ("Update:");

      declare
         DBQ : constant SQLite.Statement := SQLite.Prepare ( DBH, "UPDATE test_table SET value=? WHERE key=?");
      begin

         for Index in 1..Count loop

            SQLite.Exec (DBH, "BEGIN TRANSACTION;");
            declare
               --  Primary key so keys must be unique
               Key   : aliased  String := To_String (+"key" & Trim_Left(To_VString(Integer'Image(Index))));
               Value : aliased  String := To_String (+"value"& Trim_Left(To_VString(Integer'Image(Index + 10))));
            begin
               Tio.Put_Line ("Update Key: " & Key & " with new value: " & Value);
               SQLite.Bind (DBQ, 1, Key'Access);
               SQLite.Bind (DBQ, 2, Value'Access);
               SQLite.Step (DBQ);
               SQLite.Reset (DBQ);
            end;
            SQLite.Exec (DBH, "COMMIT;");
         end loop;
      end;

      --  Delete
      Tio.Line;
      Tio.Put_Line ("Delete:");

      declare
         DBQ : constant SQLite.Statement := SQLite.Prepare ( DBH, "DELETE FROM test_table WHERE key=?");
      begin

         for Index in 1..Count / 3 loop

            SQLite.Exec (DBH, "BEGIN TRANSACTION;");
            declare
               --  Primary key so keys must be unique
               Key : aliased  String := To_String (+"key" & Trim_Left(To_VString(Integer'Image(Index))));
            begin
               Tio.Put_Line ("Delete row with Key: " & Key);
               SQLite.Bind (DBQ, 1, Key'Access);
               SQLite.Step (DBQ);
               SQLite.Reset (DBQ);
            end;
            SQLite.Exec (DBH, "COMMIT;");
         end loop;
      end;

   end Sql_Low_Level_Demo;

begin

   Sys.Set_Memory_Monitor (True);

   Prg.Set_Version (0, 6);
   Log.Set_Display(True);
   Log.Set_Debug (False);
   Tio.Cursor_Off;

   Log.Line;
   Log.Msg (+"v20 library test program - " & Prg.Get_Version);
   Log.Msg (+"Copyright (C) Sowebio SARL 2020-2021, according to GPLv3.");
   Log.Line;

   ----------------------------------------------------------------------------
   --  STEP 0
   --  Argument processing demo

   GCL.Set_Usage (Config,
                  Usage => "[switches] [arguments] overview",
                  Help =>  "This is the short help text");

   GCL.Define_Switch (Config, Cursor_Animation_1'Access,
                      Switch => "-1",
                      Help => "Enable first cursor animation");
   GCL.Define_Switch (Config, Cursor_Animation_2'Access,
                      Switch => "-2",
                      Help => "Enable second cursor animation");
   GCL.Define_Switch (Config, Memory_Reports'Access,
                      Switch => "-m",
                      Help => "Enable memory reports");
   GCL.Define_Switch (Config, String_Option'Access,
                      Switch => "-s=", --  "-s:" to avoid "=" assignment
                      Help => "Enable option -s. Arg is a string ");
   GCL.Define_Switch (Config, Long_Option'Access,
                      Switch => "-l=",
                      Long_Switch => "--long=",
                      Help => "Enable long option. Arg is an integer");

   GCL.Getopt (Config); --  Command line processing

   ----------------------------------------------------------------------------

   Log.Set_Header (True);
   Log.Set_Disk (True);

   Log.Msg (Sys.Get_Alloc_Ada);
   Log.Msg (Sys.Get_Alloc_All);
   Log.Line;

   ----------------------------------------------------------------------------

   Log.Set_Task ("STEP 0");
   Log.Title ("Get option demo");
   Log.Line;

   if String_Option.all /= "" then
      Log.Msg ("Switch -s:        : " & String_Option.all);
   end if;
   if Long_Option /= 0 then
      Log.Msg ("Switch -l --long= : " & To_VString (Long_Option));
   end if;

   if (String_Option.all = "") and (Long_Option = 0) then
      Log.Msg ("Try ./test -h or --help");
      Log.Msg ("Try ./test -badoption");
      Log.Msg ("Try ./test -s=toto --long=123456 -1 -2 (or -12 instead)");
   end if;
   Log.Line;

   ----------------------------------------------------------------------------

   Log.Set_Task ("STEP 1");
   Log.Title ("Basic informations");
   Log.Line;

   Log.Msg ("Program name   : " & Prg.Name);
   Log.Msg ("User home      : " & Sys.Get_Home);
   Log.Msg ("Library version: " & v20.Get_Version);
   Log.Msg ("A time stamp  : " & Prg.Time_Stamp);
   Log.Line;

   ----------------------------------------------------------------------------

   Log.Set_Task ("STEP 2");
   Log.Title ("Log demo");
   Log.Line;

   Log.Msg ("This is an information message");
   Log.Dbg ("This first debug message should not appears");
   Log.Set_Debug (True);
   Log.Dbg ("This is a debug message");
   Log.Set_Debug (False);
   Log.Dbg ("This last debug message should not appears");
   Log.Err ("This is an error message");
   Log.Set_Disk (False);
   Log.Msg ("This message should not be file logged (but displayed)");
   Log.Set_Disk (True);
   Log.Msg ("This message should be truncated because it is really" &
            "too long !");
   Log.Set_Task ("TASKTRUNCATED");
   Log.Title ("Task above and this title should be truncated it is" &
              "really too long !");
   Log.Line;

   ----------------------------------------------------------------------------

   Log.Set_Task ("STEP 3");
   Log.Title ("Console demo - nothing will be logged");
   Log.Line;

   Tio.Put_Line ("Put a String at current cursor position");
   Tio.Put_Line ("Put a Char, a String, a VString then put a new line");
   Tio.Put ("C"); Tio.Put (" String"); Tio.Put (" Vtring"); Tio.Line;
   Tio.Put_Line ("Move cursor forward, backward, then put a new line");
   Tio.Put_Line ("     0    1         2         ");
   Tio.Put_Line ("012345678901234567890123456789");
   Tio.Cursor_Line_Forward (10); Tio.Put ("|");
   Tio.Cursor_Line_Forward (9); Tio.Put ("|");
   Tio.Cursor_Line_Backward (16); Tio.Put ("|");
   Tio.Line;
   Tio.Put_Line ("(-16)3<  >1(+10)   >2(+9)");
   Tio.Put_Line ("Save cursor pos, print first line, restore pos, print");
   Tio.Put_Line ("current line, then put a new line. Using 'clear'");
   Tio.Put_Line ("command before running this test is recommended to");
   Tio.Put_Line ("reset the console coordinates.");
   Tio.Cursor_Save;
   Tio.Cursor_Move (0, 0);
   Tio.Put ("Command 1 : Cursor at the first line, after saving cursor");
   Tio.Put ("coordinates and move cursor...");
   Tio.Cursor_Restore;
   Tio.Put_Line ("Command 2 : Cursor at the current line, after restoring");
   Tio.Put_Line ("cursor coordinates...");

   Tio.Line;
   if Cursor_Animation_1 then
      Tio.Put_Line ("Animation for batch processing, style 1.");
      for I in 1 .. 20 loop
         Tio.Put (" *");
         Tio.Cursor_Line_Backward (1);
         delay 0.1;
      end loop;
      for I in 1 .. 20 loop
         Tio.Put ("* ");
         Tio.Cursor_Line_Backward (3);
         delay 0.1;
      end loop;
   end if;
   if Cursor_Animation_2 then
      Tio.Put_Line ("Animation for batch processing, style 2.");
      for I in 1 .. 3 loop
         Tio.Put (I * 2 * ".");
         for J in 1 .. 7 loop
            Tio.Put (Progress (J));
            Tio.Cursor_Line_Backward (1);
            delay 0.2;
         end loop;
         Tio.Cursor_Line_Backward (3);
         delay 0.2;
      end loop;
   end if;

   Tio.Put_Line ("Finally send a beep");
   Tio.Beep;
   Log.Line;

   ----------------------------------------------------------------------------

   Log.Set_Task ("STEP 4");
   Log.Title ("Configuration file demo");
   Log.Line;
   Log.Msg ("Default name creation (prog name + .cfg)");
   Fls.Delete_File ("test.cfg"); -- demo mode
   if Cfg.Open then
      Cfg.Set ("Section_1", "Parameter_11", "Value_11");
      Cfg.Set ("Section_2", "Parameter_21", "Value_21");
      Cfg.Set ("Section_3", "Parameter_31", "Value_31");
      Log.Msg (+"Get Parameter_11: " &
               Cfg.Get ("Section_1", "Parameter_11"));
      Log.Msg (+"Get Parameter_21: " &
               Cfg.Get ("Section_2", "Parameter_21"));
      Log.Msg (+"Get Parameter_31: " &
               Cfg.Get ("Section_3", "Parameter_31"));
      Cfg.Close;
   else
      Prg.Set_Exit_Status (4);
   end if;

   Log.Msg ("Create with a custom name (custom.ini)");
   Log.Msg ("with a more complex scheme (see test.adb)");
   Fls.Delete_File ("custom.ini"); -- demo mode

   if Cfg.Open ("custom.ini") then

      Cfg.Comment ("-----------------------------------------------------");
      Cfg.Comment ("A comment line is already write at the end of the");
      Cfg.Comment ("configuration file. So you can write headers and");
      Cfg.Comment ("intermediate comments to obtain a good looking");
      Cfg.Comment ("configuration file :)");
      Cfg.Comment ("-----------------------------------------------------");
      Cfg.Comment ("");
      Cfg.Comment ("Section 1 deals with blah blah...");
      Cfg.Comment ("");
      Cfg.Set ("Section_1", "Parameter_11", "Value_11");
      Cfg.Set ("Section_1", "Parameter_12", "Value_12");
      Cfg.Set ("Section_1", "Parameter_13", "Value_13");
      Cfg.Comment ("");
      Cfg.Comment ("Section 2 deals with blah blah...");
      Cfg.Comment ("");
      Cfg.Set ("Section_2", "Parameter_21", "Value_21",
               "Only this line should stay...");
      Cfg.Set ("Section_2", "Parameter_22", "Value_22");
      Cfg.Set ("Section_2", "Parameter_23", "Value_23");
      Cfg.Comment ("");
      Cfg.Comment ("Section 3 deals with blah blah...");
      Cfg.Comment ("");
      Cfg.Set ("Section_3", "Parameter_31", "Value_31");
      Cfg.Set ("Section_3", "Parameter_32", "Value_32");
      Cfg.Set ("Section_3", "Parameter_33", "Value_33");
      Cfg.Close;

      Fls.Copy_File ("custom.ini", "custom.untouched");

      if Cfg.Open ("custom.ini") then

         Log.Msg (+"Get Parameter_12: " &
                  Cfg.Get ("Section_1", "Parameter_12"));
         Log.Msg (+"Get Parameter_22: " &
                  Cfg.Get ("Section_2", "Parameter_22"));
         Log.Msg (+"Get Parameter_32: " &
                  Cfg.Get ("Section_3", "Parameter_32"));
         Cfg.Delete ("Section_1", "Parameter_11");
         Cfg.Delete ("Section_1", "Parameter_12");
         Cfg.Delete ("Section_1", "Parameter_13");
         Log.Msg ("Delete all section_1 parameters. At the");
         Log.Msg ("deleting of the last parameter, the");
         Log.Msg ("[section_1] line, now useless, is also");
         Log.Msg ("deleted");
         Cfg.Delete ("Section_3", "Parameter_31");
         Cfg.Delete ("Section_3", "Parameter_32");
         Cfg.Delete ("Section_3", "Parameter_33");
         Cfg.Delete ("Section_2", "Parameter_22");
         Cfg.Delete ("Section_2", "Parameter_23");
         Log.Msg ("At the end of the configuration file");
         Log.Msg ("demo, only Section_2 and parameter_21");
         Log.Msg ("should remain");
         Cfg.Delete ("Section_unknowed", "Parameter_22");
         Cfg.Delete ("Section_2", "Parameter_unknown");
         Cfg.Close;
      end if;

      Log.Msg ("Trailing comment preservation test");
      if Cfg.Open ("dontdelete.me") then
         Cfg.Set ("Section_1", "Parameter_11", "New_Value: " &
                  To_String (Prg.Time_Stamp));
         Cfg.Set ("Section_2", "Parameter_21", "New_Value: " &
                  To_String (Prg.Time_Stamp));
         Cfg.Set ("Section_3", "Parameter_31", "New_Value: " &
                  To_String (Prg.Time_Stamp));
         Cfg.Close;
      end if;
      Log.Line;
   else
      Prg.Set_Exit_Status (2);
   end if;
   Log.Set_Disk (False);

   ----------------------------------------------------------------------------

   Log.Set_Task ("STEP 5");
   Log.Title ("Memory reports demo");

   if Memory_Reports then
      Log.Line; Log.Title ("");
      Log.Title ("Memory: Report");
      Log.Title (""); Log.Line;
      Sys.Get_Memory_Dump (1);

      Log.Line; Log.Title ("");
      Log.Title ("Memory: Allocations_Count");
      Log.Title (""); Log.Line;
      Sys.Get_Memory_Dump (1, Sys.Allocations_Count);

      Log.Line; Log.Title ("");
      Log.Title ("Memory: Sort_Total_Allocs");
      Log.Title (""); Log.Line;
      Sys.Get_Memory_Dump (1, Sys.Sort_Total_Allocs);

      Log.Line; Log.Title ("");
      Log.Title ("Memory: Marked_Blocks");
      Log.Title (""); Log.Line;
      Sys.Get_Memory_Dump (1, Sys.Marked_Blocks);
      Log.Line;
      Log.Title ("Memory: Reporting Ada and All languages ");
      Log.Line;
      Log.Msg (Sys.Get_Alloc_Ada);
      Log.Msg (Sys.Get_Alloc_All);
      Log.Line;
   else
      Log.Msg ("Use -m to display memory reports...");
      Log.Line;
   end if;

   ----------------------------------------------------------------------------

   Log.Set_Task ("STEP 6");
   Log.Title ("Shell execute demo");

   Log.Line;
   Log.Msg ("Execute cat test.cfg and display results.");
   Log.Line;

   declare
      SE_Result : Integer := 0;
      SE_Output : VString := +"";
   begin
      Sys.Shell_Execute ("cat test.cfg", SE_Result, SE_Output);
      if SE_Result = 0 then
         Tio.Put_Line (SE_Output);
         Tio.Line;
      end if;
   end;

   declare
      SE_Result : Integer := 0;
   begin
      Sys.Shell_Execute ("find test.cfg", SE_Result);
      Tio.Put_Line (SE_Result);
      Tio.Line;
   end;

   declare
      SE_Result : Integer := 0;
   begin
      Sys.Shell_Execute ("find i.dont.exist", SE_Result);
      Tio.Put_Line (SE_Result);
      Tio.Line;
   end;

   declare
      type Money is delta 0.01 digits 10;
   begin
      Tio.Put_Line ("Money is delta 0.01 digits 10 - Money'Image (Money'Last): " & Money'Image (Money'Last));
   end;
   Log.Line;

   ----------------------------------------------------------------------------

   --Log.Set_Task ("STEP 7");
   --Log.Title ("cURL demo");

   --Log.Line;
   --Log.Msg ("Low level functions - Simple test");
   --Log.Line;

   ----------------------------------------------------------------------------

   Log.Set_Task ("STEP 8");
   Log.Title ("SQLite low level demo");

   Sql_Low_Level_Demo;
   Log.Line;

   ----------------------------------------------------------------------------

   Log.Set_Task ("STEP 9");
   Log.Title ("SQLite high level demo");

   Sql_High_Level_Demo;
   Log.Line;

   ----------------------------------------------------------------------------

   Log.Set_Task ("STEP 10");
   Log.Title ("SQLite high level Schema_Load and Schema_Update demo");
   Log.Line;

   if Sql.Schema_Need_Update (+"sqlite_update_test.db", 0, 2) then

      Sql.Schema_Load (Sql.Table_Name,        "Cluster");
      Sql.Schema_Load (Sql.Column_Name,       "Number",  "INTEGER");
      Sql.Schema_Load (Sql.Column_Constraint, "Number",  "UNIQUE");
      Sql.Schema_Load (Sql.Table_Constraint,  "Number",  "PRIMARY KEY");
      Sql.Schema_Load (Sql.Column_Name,       "Domain",  "TEXT");
      Sql.Schema_Load (Sql.Column_Name,       "Email",   "TEXT");
      Sql.Schema_Load (Sql.Column_Name,       "Bidule",   "TEXT");
      Sql.Schema_Load (Sql.Column_Name,       "Manager", "INTEGER");
      Sql.Schema_Load (Sql.Column_Name,       "API_EP",  "TEXT");
      Sql.Schema_Load (Sql.Column_Name,       "API_AK",  "TEXT");
      Sql.Schema_Load (Sql.Column_Name,       "API_AS",  "TEXT");
      Sql.Schema_Load (Sql.Column_Name,       "API_CK",  "TEXT");
      Sql.Schema_Load (Sql.Index_Name,        "Idx",     "Number");
      Sql.Schema_Load (Sql.Index_Constraint,  "Idx",     "UNIQUE");

      Log.Set_Debug (True);

      Sql.Schema_Update;

      Log.Set_Debug (False);

   end if;

   Tio.Line;
   Tio.Put("Table_Exists: ");
   Tio.Put_Line (Sql.Table_Exists (+"Cluster")); -- Existing table
   Tio.Put("Table_Exists: ");
   Tio.Put_Line (Sql.Table_Exists (+"Cluster_Void")); -- Non existing table

   Tio.Line;
   Tio.Put("Index_Exists: ");
   Tio.Put_Line (Sql.Index_Exists (+"Idx")); -- Existing index
   Tio.Put("Index_Exists: ");
   Tio.Put_Line (Sql.Index_Exists (+"Idx_Void")); -- Non existing index

   Tio.Line;
   Tio.Put("Column_Exists: ");
   Tio.Put_Line (Sql.Column_Exists (+"Cluster", +"Domain")); -- Existing column
   Tio.Put("Column_Exists: ");
   Tio.Put_Line (Sql.Column_Exists (+"Cluster", +"Domain_Void")); -- Non existing column

   Tio.Line;

   Log.Set_Debug (True);

   Sql.Insert (+"Cluster", +"Number~1234" & "^" & "Domain~genesix.org");
   Log.Dbg ("Insert_RowID: " & Trim_Left (To_VString (Integer (Sql.Last_Insert_RowID))));

   Sql.Update (+"Cluster", +"Domain~genesix2.org", +"Number = 1234");

   Sql.Insert (+"Cluster", +"Number~1" & "^" & "Domain~domain1");
   Sql.Insert (+"Cluster", +"Number~2" & "^" & "Domain~domain2");
   Sql.Insert (+"Cluster", +"Number~3" & "^" & "Domain~domain3");
   Sql.Insert (+"Cluster", +"Number~4" & "^" & "Domain~domain4");

   Log.Dbg (Sql.Read (+"Cluster", +"Number,Domain", +"WHERE Number = 1234"));
   Log.Dbg (Sql.Read (+"Cluster", +"Number,Domain"));

   Tio.Line;
   Tio.Put_Line ("Formatted display: " & "Sql.Read (+" & DQ & "Cluster" & DQ & ", +" & DQ &
   "Number,Domain" & DQ & "), CD, RD, " & DQ & "Cluster number,Domain name" & DQ & ")");
   Tio.Line;
   Field_Display (Sql.Read (+"Cluster", +"Number,Domain"), CD, RD, "Cluster number,Domain name");

   Tio.Line;
   if Sql.Search (+"Cluster",+"WHERE Number = 1234") then
      Tio.Put_Line ("Search 'Number = 1234': Found");
   end if;
   if not Sql.Search (+"Cluster",+"WHERE Number = 9999") then
      Tio.Put_Line ("Search 'Number = 9999': Not found");
   end if;
   Tio.Line;

   Sql.Delete (+"Cluster", +"Number = 1234");
   Sql.Delete (+"Cluster", +"Number < 5");

   Log.Line;

 ----------------------------------------------------------------------------

   Log.Set_Task ("STEP 11");
   Log.Title ("Local package install");
   Log.Line;

   Tio.Put (+"Is 'apt' package installed? ");
   Tio.Put_Line (Sys.Is_Package (+"apt"));

   Tio.Put (+"Is 'joe' package installed? ");
   Tio.Put_Line (Sys.Is_Package (+"joe"));

   Tio.Put (+"Is 'le' package installed? ");
   Tio.Put_Line (Sys.Is_Package (+"le"));

   Log.Line;

   if Sys.Install_Packages ("joe,le") then
      Log.Msg ("'joe' and 'le' packages has been installed.");
   else
      Log.Err ("At least one package has not been installed.");
   end if;

   Log.Line;

   Tio.Put (+"Is 'joe' package installed? ");
   Tio.Put_Line (Sys.Is_Package (+"joe"));
   Tio.Put (+"Is 'le' package installed? ");
   Tio.Put_Line (Sys.Is_Package (+"le"));

   Log.Line;

   if Sys.Install_Packages ("le") then
         Log.Msg ("'le' is already installed, so Sys.Install_Packages returns true again.");
   end if;

   Log.Line;

   if Sys.Purge_Packages ("joe,le") then
      Log.Msg ("'joe' and 'le' packages has been purged.");
   else
      Log.Err ("At least one package has not been purged.");
   end if;

   Log.Line;

   Tio.Put (+"Is 'joe' package installed? ");
   Tio.Put_Line (Sys.Is_Package (+"joe"));

   Tio.Put (+"Is 'le' package installed? ");
   Tio.Put_Line (Sys.Is_Package (+"le"));

   Log.Line;

 ----------------------------------------------------------------------------

   Log.Set_Task ("STEP 12");
   Log.Title ("Distant package install");
   Log.Line;

   Tio.Put (+"Is 'apt' package installed? ");
   Tio.Put_Line (Sys.Is_Package (+"apt",+"root@n222c1.genesix.org"));
   Tio.Put (+"Is 'joe' package installed? ");
   Tio.Put_Line (Sys.Is_Package (+"joe",+"root@n222c1.genesix.org"));
   Tio.Put (+"Is 'le' package installed? ");
   Tio.Put_Line (Sys.Is_Package (+"le",+"root@n222c1.genesix.org"));

   Log.Line;

   if Sys.Install_Packages ("joe,le",+"root@n222c1.genesix.org") then
      Log.Msg ("'joe' and 'le' packages has been installed.");
   else
      Log.Err ("At least one package has not been installed.");
   end if;

   Log.Line;

   Tio.Put (+"Is 'apt' package installed? ");
   Tio.Put_Line (Sys.Is_Package (+"apt",+"root@n222c1.genesix.org"));
   Tio.Put (+"Is 'joe' package installed? ");
   Tio.Put_Line (Sys.Is_Package (+"joe",+"root@n222c1.genesix.org"));
   Tio.Put (+"Is 'le' package installed? ");
   Tio.Put_Line (Sys.Is_Package (+"le",+"root@n222c1.genesix.org"));

   Log.Line;

   if Sys.Install_Packages ("le",+"root@n222c1.genesix.org") then
         Log.Msg ("'le' is already installed, so Sys.Install_Packages returns true again.");
   end if;

   Log.Line;

   if Sys.Purge_Packages ("joe,le",+"root@n222c1.genesix.org") then
      Log.Msg ("'joe' and 'le' packages has been purged.");
   else
      Log.Err ("At least one package has not been purged.");
   end if;

   Log.Line;

   Tio.Put (+"Is 'joe' package installed? ");
   Tio.Put_Line (Sys.Is_Package (+"joe",+"root@n222c1.genesix.org"));
   Tio.Put (+"Is 'le' package installed? ");
   Tio.Put_Line (Sys.Is_Package (+"le",+"root@n222c1.genesix.org"));

   Log.Set_Debug (False);
   Tio.Cursor_On;

   Log.Line;
   Log.Set_Task ("END");
   Log.Title ("End of demo");
   Log.Line;

   -- ------------------------------------------------------------------/\-----
   --  Raise_Exception; --  < Uncomment for trigger exception test     /!!\
   -- ----------------------------------------------------------------/-!!-\---

exception

   --  Invalid switches
   when GCL.Invalid_Switch =>
      Log.Line;
      GOL.OS_Exit (2);

   --  -h or --help switches
   when GCL.Exit_From_Command_Line =>
      Log.Line;
      GOL.OS_Exit (1);

   --  Runtime errors
   when Error : others =>
      v20.Exception_Handling (Error);

-----------------------------------------------------------------------------
end Test;
-----------------------------------------------------------------------------
