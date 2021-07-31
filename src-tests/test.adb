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
------------------------------------------------------------------------------

with GNAT.Command_Line;
with GNAT.OS_Lib;
with GNAT.Strings;

with v20; use v20;
with v20.Cfg;
with v20.Fls;
with v20.Log;
with v20.Prg;
with v20.Tio;
with v20.Sys;
with v20.Vst; use v20.Vst;

procedure Test is

   package GCL renames GNAT.Command_Line;
   package GOL renames GNAT.OS_Lib;
   package GS renames GNAT.Strings;

   type Animation is array (1 .. 7) of Character;
   Progress : constant Animation := ('/', '-', '\', '|', '/', '-', '|');

   Config : GCL.Command_Line_Configuration;
   Cursor_Animation_1 : aliased Boolean := False;
   Cursor_Animation_2 : aliased Boolean := False;
   Memory_Reports : aliased Boolean := False;
   String_Option : aliased GS.String_Access;
   Long_Option : aliased Integer := 0;

begin

   Sys.Set_Memory_Monitor (True);

   Prg.Set_Version (0, 4);

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
   Tio.Put_Line ("     3    1         2");
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
      Tio.Put_Line (Money'Image (Money'Last));
   end;

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
