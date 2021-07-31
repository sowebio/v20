-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20-Prg.ads
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
------------------------------------------------------------------------------
--  @summary
--  V20 library common
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  20210317 - 0.1 - sr - initial release
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Directories;

with v20.Vst; use v20.Vst;

package v20.Prg is

   Command : constant VString := To_VString (Ada.Command_Line.Command_Name);
   --  Constant storing program command (Arg 0).

   function Duration_Stamp (Start_Time : Ada.Calendar.Time) return VString;
   --  Returns a duration as HHhMMmSSs since Start_Time.

   function Get_Version return VString;
   --  Returns program name and formatted program version :
   --  “<space>v.minor.major”.

   function Is_User_Not_Root return Boolean;
   --  Returns True if program user's not root.

   --  Name : constant String := To_String (Tail_After_Match (Command, "/"));
   function Name return VString;
   --  Program name.

   -- Path : constant String := To_String (Slice (To_VString (Name), 1, Index_Backward ( To_VString (Name), "/")));
   function Path return String;
   --  Constant storing program path.

   procedure Set_Exit_Status (Code : Natural);
   --  Set errorlevel return code. Each call is cumulative. Four calls with
   --  1, 2, 4 and 8 set 15 ie msb-00001111-lsb. Can be used everywhere in
   --  the program without special call at its end. Convention : 1 = no or bad
   --  command, 128 = runtime exception (8th bit).

   procedure Set_Version (Major : Natural; Minor : Natural);
   --  Set program version.

   Start_Dir : constant VString := To_VString (Ada.Directories.Current_Directory);
   --  Constant storing current directory at start.

   Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   --  Program start timestamp.

   function Time_Stamp return VString;
   --  Returns current timestamp as YYYYMMDD-HHMMSS.

-------------------------------------------------------------------------------
private

   function Time_Format (Input_To_Format : Integer) return VString;

   Version_Major : Natural := 0;
   Version_Minor : Natural := 0;

   Prg_Exit_Status : Natural := 0;

-------------------------------------------------------------------------------
end v20.Prg;
-------------------------------------------------------------------------------
