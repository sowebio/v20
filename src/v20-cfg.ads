-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20-Cfg.ads
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V20 library config file manager
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  20210329 - 0.1 - sr - v20 lib integration
-------------------------------------------------------------------------------

with v20.Vst; use v20.Vst;
with v20.Prg;
with v20.Tio;

package v20.Cfg is

   pragma Elaborate_Body;

   function Open (Cfg_File_Read_In : String := "") return Boolean;
   --  Open and load if exist a configuration file. Create blank if non
   --  existent. Default configuration file name is “program name” followed by
   --  .cnf extension and created in the program start directory.

   procedure Close;
   --  Close Cfg file. For sanity only as each setting is instantly flushed to
   --  disk.

   procedure Set (Section : String;
                  Parameter : String;
                  Value : String;
                  Comment : String := "");
   --  Create or replace an existing parameter in a section. If this latter
   --  does not exist, also creating it. New setting is persistent even program
   --  quits unexpectedly after. Avoid reserved chars [ ] = # inside
   --  parameters. If reserved chars are passed, the procedure does nothing. A
   --  optional trailing comment can also be added.

   function Get (Section : String; Parameter : String) return VString;
   --  Return parameter in section or empty string if not found. Avoid reserved
   --  chars [ ] = # inside parameters.

   procedure Delete (Section : String; Parameter : String);
   --  Delete parameter in section. If no other parameter in this section,
   --  delete section too. Avoid reserved chars [ ] = # inside parameters.
   --  If reserved chars are passed, the procedure does nothing.

   procedure Comment (Text : String);
   --  Insert a comment after the last line.

private

   Cfg_File_Read : VString := Prg.Start_Dir & "/" & Prg.Name & ".cfg";
   Cfg_File_Write : VString := Prg.Start_Dir & "/" & Prg.Name & ".tmp";

   Cfg_Open_Section : constant VString := +"[";
   Cfg_Close_Section : constant VString := +"]";
   Cfg_Assignment : constant VString := +"=";
   Cfg_Comment : constant VString := +"#";
   Cfg_Command_Delete : constant VString := Cfg_Open_Section &
                                            Cfg_Open_Section & "D";
   Cfg_Command_Add : constant VString := Cfg_Open_Section &
                                         Cfg_Open_Section & "P";

   --  Memory consumption test of an array of VString
   --  Table_Max  ram
   --  (elements) (bytes)
   --  500        1088
   --  250        488

   Table_Max : Natural := 250;
   type Table_Lines is array (1 .. Table_Max) of VString;
   Cfg_Table : Table_Lines;

   Cfg_Last : Natural := 0;
   Cfg_Section : Natural := 0;
   Cfg_Parameter : Natural := 0;

   Handle_Read : Tio.File;
   Handle_Write : Tio.File;

   --  Service functions

   function Table_Write (Line : VString) return Boolean;
   function Cfg_Read return Boolean;
   function Check_Parameters (Section : String;
                              Parameter : String;
                              Value : String)
                              return Boolean;
   procedure Cfg_Search (Section : String; Parameter : String);
   function Cfg_Write (Section : String := "";
                       Parameter : String := "";
                       Value : String := "";
                       Trailing_Comment : VString := +"")
                       return Boolean;

end v20.Cfg;
