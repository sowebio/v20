-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20.ads
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V20 library definitions
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  20210317 - 0.1 - sr - initial release
--  20210402 - 0.2 - sr - basic cfg log prg tio vst packages coded
--  20210411 - 0.3 - sr - refactoring: fls sys created extend api too
--  20210414 - 0.4 - sr - adjust again Line_Max_Length
--  20210731 - 0.5 - sr - API consistency: All strings constants and function 
--                        only returns VString typed. All strings parameters 
--                        accept both String and VString types.
--  20210804 - 0.6 - sr - Fix a RTE when a program is launched through a 
--                        symbolic link. Add Get_Build function.
-------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Strings.Unbounded;

package v20 is

   -- Can't use v20-vst to avoid circular references
   package ASU renames Ada.Strings.Unbounded;

   function Get_Version return ASU.Unbounded_String;
   --  Returns the Library name and formatted version like:
   --  “v20 v.minor.major”.
   
   function Get_Build return ASU.Unbounded_String;
   --  Returns the formatted build date stamp like:
   --  “build YYYY-mm-dd hh:mm:ss”.

   procedure Raise_Exception;
   --  Raise an exception for reporting test and <program_Name.err> file
   --  creation. In addition to the usual trace, a v20 exception give some
   --  extra information like : exception time, program uptime, program &
   --  library names & versions, start & home directories and Ada and all
   --  languages memory allocation, current & maximum (peak) values.

   procedure Exception_Handling
   (Exception_Hook : Ada.Exceptions.Exception_Occurrence);
   --  Process exceptions.

-------------------------------------------------------------------------------
private

   Name : constant String := "v20";
   --  Library's name

   Version_Major : constant Natural := 0;
   --  Library major version number

   Version_Minor : constant Natural := 6;
   --  Library minor version number

   --  135 cols width is the max full screen standard console on a rather old,
   --  but so good, Dell UltraSharp 1907Fp 1280x1024 4:3 monitor
   --  92 cols width is the max length useable in the "Listing 7" paragraph 
   --  style of AIDE Manual with B612 font.
   --  79 is the standard width.
   
   Line_Max_Length : constant Natural := 92;
   --  Maximum line length for exceptions (.err) and log reports (.log)

   Errorlevel : Natural := 0;

------------------------------------------------------------------------------
end v20;
------------------------------------------------------------------------------
