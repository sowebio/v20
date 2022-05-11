------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20-Log.ads
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
------------------------------------------------------------------------------
--  @summary
--  V20 library test file
--
--  @description
--  Launch and reports validation tests
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  see v20.ads
------------------------------------------------------------------------------

with v20.Vst; use v20.Vst;
with v20.Tio;
with v20.Prg;

package v20.Log is

   procedure Dbg (Message : String);
   procedure Dbg (Message : VString);
   --  Log a debug message.

   procedure Err (Message : String);
   procedure Err (Message : VString);
   --  Log a error message.
      
   function Get_Debug return Boolean;
   --  Return true if debug status is on.

   procedure Line;
   --  Log a blank line.
   
   function Log_Dir return VString;
   --  Returns log file directory.
   
   procedure Msg (Message : Integer);
   procedure Msg (Message : Character);
   procedure Msg (Message : String);
   procedure Msg (Message : VString);
   --  Log a message.

   procedure Set_Debug (Action : Boolean);
   --  Set debug messages status on/[off].
   
   procedure Set_Display (Action : Boolean);
   --  Log to display on/[off].

   procedure Set_Disk (Action : Boolean);
   --  Log to disk on/[off].
      
   procedure Set_Header (Action : Boolean);
   --  Line header on/[off].
   
   procedure Set_Log_Dir (Dir_In : String);
   procedure Set_Log_Dir (Dir_In : VString);
   --  Set log file directory.
   
   procedure Set_Task (New_Task : String);
   procedure Set_Task (New_Task : VString);
   --  Set new current log task.

   procedure Title (Message : String);
   procedure Title (Message : VString);
   --  Log a title.

------------------------------------------------------------------------------
private

   Task_State : VString := +"INIT";

   --  0         1         2         3  3
   --  0123456789012345678901234567890123
   --  20210327 160010 - STEP 3  - MSG -
   --  \-----15-----/  3 \--7--/ 3 \3/ 3 = Header_Length
   --   Timestamp        Task    Class
   --  Line_Max_Length : constant Natural := 79; < Declared in v20.ads

   Task_Max_Length : constant Natural := 7;
   Header_Length : constant Natural := 34;

   Header_On : Boolean := False;
   --  Line header on/[off]

   Debug_On : Boolean := False;
   --  Debug messages on/[off]
   
   Display_On : Boolean := False;
   --  Log to display on/[off]

   Disk_On : Boolean := False;
   --  Log to disk on/[off]

   Handle : Tio.File;

   Log_Dir_Store : VString := Prg.Start_Dir;

   procedure Put (Line_In : String;
                  Line_Level : String;
                  Title_On : Boolean := False);

------------------------------------------------------------------------------
end v20.Log;
------------------------------------------------------------------------------
