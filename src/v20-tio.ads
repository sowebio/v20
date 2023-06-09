------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20-Tio.ads
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
------------------------------------------------------------------------------
--  @summary
--  V20 library test File_Handle
--
--  @description
--  Launch and reports validation tests
--
--  @authors
--  Gautier de Montmollin - gdm - https://github.com/zertovitch (HAC author)
--  Stéphane Rivière - sr - sriviere@soweb.io (HAC runtime integration)
--
--  @versions
--  see v20.ads
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Text_IO;

--  SQLite
with Interfaces;
--with Interfaces.C;

with v20.Vst; use v20.Vst;

package v20.Tio is

   package ASU renames Ada.Strings.Unbounded;
   package ATI renames Ada.Text_IO;

   Ansi : Boolean := True;

   --  Terminal

   Max_Row  : constant Natural := 24;
   Max_Column : constant Natural := 79;

   subtype Row is Natural range 0 .. Max_Row;
   subtype Column  is Natural range 0 .. Max_Column;
   subtype Integer_64 is Interfaces.Integer_64;

   procedure Put (B : Boolean);
   procedure Put (I : Integer);
   procedure Put (I : Long_Integer);
   procedure Put (I : Integer_64);
   procedure Put (C : Character) renames ATI.Put;
   procedure Put (S : String) renames ATI.Put;
   procedure Put (V : VString);
   --  Print to the console.

   procedure Put_Line (B : Boolean);
   procedure Put_Line (I : Integer);
   procedure Put_Line (I : Long_Integer);
   procedure Put_Line (I : Integer_64);
   procedure Put_Line (C : Character);
   procedure Put_Line (S : String) renames ATI.Put_Line;
   procedure Put_Line (V : VString);
   --  Print to the console then add a new line.

   procedure Animated_Delay (Delay_Seconds : Positive);
   -- Animated delay in seconds with markers each 5 and 10 seconds.
   -- ....!....|....!....|....!./ < animated wheel with /-\|/-| characters
   -- .1s !5s  |10s

   function Confirm_Twice (User_Prompt_1 : VString ; User_Prompt_2 : VString) return Boolean;
   -- Double check by user before action. Returns True if user has validate.

   procedure Line (Spacing : ATI.Positive_Count := 1) renames ATI.New_Line;
   --  Add a new line to the console

   procedure Get_Immediate (C : out Character)
                            renames Ada.Text_IO.Get_Immediate;
   --  Get a character validated by [Enter].

   function Get_Password return VString;
   --  Returns a password blind typed

   procedure Pause;
   --  Displays "Press any key to continue or [Ctrl-C] to abort..." waiting for
   --  user input.

   procedure Beep;
   --  Send a beep.

   procedure Clear_Screen;
   --  Clear the screen.

   procedure Cursor_Move (X : Row; Y : Column);
   --  Move the cursor at the specified X,Y coordinates.

   procedure Cursor_Line_Forward (X : Row);
   --  Move the cursor forward X rows.

   procedure Cursor_Line_Backward (X : Row);
   --  Move the cursor backward X rows.

   procedure Cursor_Line_Erase;
   --  Erase the current line from the current cursor position to the end of
   --  the line.

   procedure Cursor_Save;
   --  Save the current cursor position.

   procedure Cursor_Restore;
   --  Restore the previous saved cursor position.

   procedure Cursor_On;
   --  Display cursor.

   procedure Cursor_Off;
   --  Hide cursor.

   --  Text File_Handle

   subtype File is ATI.File_Type;

   procedure Open_Conf (Handle : in out File; Name : String ;
                       Wipe_Before_Process : Boolean := False ;
                       Permissions : VString := +"");

   procedure Open_Conf (Handle : in out File; Name : VString ;
                       Wipe_Before_Process : Boolean := False ;
                       Permissions : VString := +"");
   --  Special Open procedure for config files. Creates or Append if needed.
   --  Ensure that the complete directory tree structure exists before
   --  creating file. Creating this directory tree if needed.
   --  Allways make backup before Append. If Wipe_Before_Process is True, the
   --  file Name is backuped before beeing deleted

   procedure Open_Read (Handle : in out File; Name : String);
   procedure Open_Read (Handle : in out File; Name : VString);
   --  Open a file in read mode.

   procedure Create (Handle : in out File; Name : String);
   procedure Create (Handle : in out File; Name : VString);
   --  Create a file.

   procedure Append (Handle : in out File; Name : String);
   procedure Append (Handle : in out File; Name : VString);
   --  Append on an existing file.

   procedure Close (Handle : in out File) renames ATI.Close;
   --  Close a file.

   function Is_Open (Handle : File) return Boolean renames ATI.Is_Open;
   --  Test if a file is open.

   procedure Put (Handle  : File; C : Character) renames ATI.Put;
   procedure Put (Handle  : File; S : String) renames ATI.Put;
   procedure Put (Handle  : File; V : VString);
   --  Write to a file.

   procedure Put_Line (Handle  : File; C : Character);
   procedure Put_Line (Handle  : File; S : String) renames ATI.Put_Line;
   procedure Put_Line (Handle  : File; V : VString);
   --  Write a file and then add a new line

   procedure Get_Line (Handle : File; V : out VString);
   --  Read a line then move the file pointer to the next line.

   procedure Line (Handle : File; Spacing : ATI.Positive_Count := 1)
                   renames ATI.New_Line;
   --  Add a new line to a file.

   procedure Reset (Handle : in out File) renames ATI.Reset;
   --  Reset the file pointer to the start of the file

   procedure Flush (Handle : File) renames ATI.Flush;
   --  Flush file buffer to disk.

   function End_Of_Line (Handle : File) return Boolean renames ATI.End_Of_Line;
   --  Test if end of line reached.

   function End_Of_File (Handle : File) return Boolean renames ATI.End_Of_File;
   --  Test if enf of file reached.

   function Read_File (File_Name : VString) return VString;
   --  Read a text file File_To_Read and returning a VString buffer. LF
   --  (line feed) are preserved.

   procedure Write_File (File_Name : VString ; Content : VString ; Permissions : VString := +"");
   --  Write a text file File_To_Write with Content. LF in content are
   --  preserved and used as line feed. Read Open_Conf documentation for
   --  implementation details.

-------------------------------------------------------------------------------
end v20.Tio;
-------------------------------------------------------------------------------
