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
--  see v20.ads
------------------------------------------------------------------------------

with Ada.Calendar; use Ada.Calendar; -- Use for operation on Ada.Calendar.Time
with Ada.Command_Line;
with Ada.Directories;

with v20.Vst; use v20.Vst;

package v20.Prg is

   package AC renames Ada.Calendar;

   Command : constant VString := To_VString (Ada.Command_Line.Command_Name);
   --  Constant storing program command (Arg 0).

   function Current_Time_Seconds return Natural;
   --  Returns a duration as seconds since ISO date 197001010. Conforms to
   --  Unix time standard. Checked with console command "date +%s". Valid
   --  algorithm until 2070.

   function Duration_Stamp (Time : Ada.Calendar.Time) return VString;
   --  Returns a duration as HHhMMmSSs since Time.

   function Duration_Stamp_Seconds (Time : Ada.Calendar.Time) return Natural;
   --  Returns a duration as seconds since Time.

   function Duration_Stamp_Time (Time_Seconds : Integer) return VString;
   --  Returns a formatted HHhMMmSSs VString from Time_Seconds

   function Generate_Password return VString;
   --  Sowebio standard password generation with 64 charset:
   --    ([A-Z] + [a-z] + [0-9] + '_' + '-')
   --  Search space size:
   --    > 1,26 x 10^25
   --  Space exploration time:
   --    40000 centuries @ 100 billion tests per second.
   --  Command line with standard tools:
   --    < /dev/urandom tr -dc _A-Z-a-z-0-9 | head -c${1:-14};echo;
   --  Generates 14 chars long passwords like:
   --   5fx7_0Fubo-hNa

   function Get_Version return VString;
   --  Returns formatted program version :
   --  “<space>v.minor.major”.

   function Get_Version_Major return Natural;
   --  Returns major program version

   function Get_Version_Minor return Natural;
   --  Returns minor program version

   function Is_User_Not_Root return Boolean;
   --  Returns True if program user's not root.

   function Name return VString;
   --  Program name.

   function Path return String;
   --  Program path.

   procedure Set_Exit_Status (Code : Natural);
   --  Set errorlevel return code. Each call is cumulative. Four calls with
   --  1, 2, 4 and 8 set 15 ie msb-00001111-lsb. Can be used everywhere in
   --  the program without special call at its end. Convention : 1 = no or bad
   --  command, 128 = runtime exception (8th bit).

   procedure Set_Version (Major : Natural; Minor : Natural);
   --  Set program version.

   Start_Dir : constant VString := To_VString (Ada.Directories.Current_Directory);
   --  Constant storing current directory at start.

   Start_Time : constant Ada.Calendar.Time := AC.Clock;
   --  Constant storing Time at program start

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
