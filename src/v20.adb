-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20.adb
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V20 library
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  see .ads
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Strings.Fixed;
with GNAT.Source_Info;

with v20.Prg;
with v20.Sys;
with v20.Tio;

with v20.Vst; use v20.Vst;

package body v20 is

   procedure Raise_Exception is
      v20_Exception_Test : exception;
   begin
      raise v20_Exception_Test;
   end Raise_Exception;

   procedure Exception_Handling
                 (Exception_Hook : Ada.Exceptions.Exception_Occurrence) is

      Exception_Handle : Tio.File;
      Exception_File_Name : constant VString := Prg.Start_Dir & "/" & Prg.Name & ".err";
      Trace_Output : VString := To_VString (Ada.Exceptions.Exception_Information (Exception_Hook));
      Trace_Lines : constant Natural := Field_Count (Trace_Output, LF);
      Current_Line : VString;
      Current_Address : VString;
      Number_Position, First_Space_Position : Natural;
      Output_Processed : VString := +"";

      Trace_without_Lines : Boolean := False;

      SE_Result : Integer := 0;
      SE_Output : VString := +"";

   begin

      Tio.Line;
      Tio.Put_Line (Title_Max_Length * "-");
      Tio.Line;
      Tio.Put_Line ("Exception time         : " & Prg.Time_Stamp);
      Tio.Put_Line ("Program uptime         : " & Prg.Duration_Stamp (Prg.Start_Time));
      Tio.Put_Line ("Program build DT stamp : " & v20.Get_Build);
      Tio.Put_Line ("Program name & version : " & Prg.Get_Version);
      Tio.Put_Line ("Library name & version : " & v20.Get_Version);
      Tio.Put_Line ("Start directory        : " & Prg.Start_Dir);
      Tio.Put_Line ("Home directory         : " & Sys.Get_Home);
      Tio.Put_Line ("Ada mem. alloc. (bytes): " & Sys.Get_Alloc_Ada);
      Tio.Put_Line ("All mem. alloc. (bytes): " & Sys.Get_Alloc_All);

      --  In the GNAT Pro and CE versions AdaCore has integrated the GNU/GCC
      --  utility addr2line into the GNAT runtime. It is called to display the
      --  subroutine names and line numbers of the call stack.
      --  This feature is missing in the GNAT FSF version. This is a recurring
      --  complaint on the net, some people offer workarounds, but none of them
      --  work here (Linux, GNAT FSF 11).
      --  Since the CE versions are terminated, there were two solutions to
      --  restore this useful feature; the clean one (build a full FSF 12
      --  suite as GNAT FSF 12 gracefully returns line numbers - but lack of
      --  time) and the dirty (faster).

      --  When handling an exception, we search if the addr2line utility exists
      --  on the system and then, for each line indicating a "???", we run, on
      --  the executable itself, a command addr2line -e <executable name>
      --  <address> and analyze the return. If the latter is valid (example:
      --  /home/sr/.../debug/gnx-utl_sv.adb:1174, as opposed to "addr2line:
      --  DWARF error: can't find .debug_ranges section." and/or "??:?",
      --  ??:0"), the function replaces "???" by "gnx-utl_sv.adb:1174". If the
      --  program was compiled with GNAT Pro or CE, this is transparent and if
      --  add2line was not found, the "???" are left as they are.

      if (Trace_Lines > 0) and Sys.Is_Command ("addr2line") then

         for I in 1 .. Trace_Lines loop
            Current_Line := Field_By_Index (Trace_Output, I, LF);
            Number_Position := Index (Current_Line, "???");
            First_Space_Position := Index (Current_Line, SP);

            if (Number_Position > 2) and (First_Space_Position > 2) then
               Current_Address := Slice (Current_Line, 1, Index (Current_Line, SP) - 1);
               Sys.Shell_Execute ("addr2line -e " & Prg.Name & " " & Current_Address, SE_Result, SE_Output);
               if (Index (SE_Output, "/") > 0) then
                  Output_Processed := Output_Processed & Slice (Current_Line, 1, Number_Position - 1) & Tail_After_Match (SE_Output, "/") & LF;
               else
                  Output_Processed := Output_Processed & Current_Line & LF;
               end if;
               Trace_without_Lines := True;
            else
               Output_Processed := Output_Processed & Current_Line & LF;
            end if;
         end loop;

         if Trace_without_Lines then
            Trace_Output := Output_Processed;
         end if;

         Tio.Line;
         Tio.Put_Line (Trace_Output);
         Tio.Put_Line (Title_Max_Length * "-");
         Tio.Line;

         if Ada.Directories.Exists (To_String (Exception_File_Name)) then
            Tio.Append (Exception_Handle, Exception_File_Name);
         else
            Tio.Create (Exception_Handle, Exception_File_Name);
         end if;

         if Tio.Is_Open (Exception_Handle) then
            Tio.Put_Line (Exception_Handle, Title_Max_Length * "-");
            Tio.Line (Exception_Handle);
            Tio.Put_Line (Exception_Handle, "Exception time         : " & Prg.Time_Stamp);
            Tio.Put_Line (Exception_Handle, "Program uptime         : " & Prg.Duration_Stamp (Prg.Start_Time));
            Tio.Put_Line (Exception_Handle, "Program build DT stamp : " & v20.Get_Build);
            Tio.Put_Line (Exception_Handle, "Program name & version : " & Prg.Get_Version);
            Tio.Put_Line (Exception_Handle, "Library name & version : " & v20.Get_Version);
            Tio.Put_Line (Exception_Handle, "Start directory        : " & Prg.Start_Dir);
            Tio.Put_Line (Exception_Handle, "Home directory         : " & Sys.Get_Home);
            Tio.Put_Line (Exception_Handle, "Ada mem. alloc. (bytes): " & Sys.Get_Alloc_Ada);
            Tio.Put_Line (Exception_Handle, "All mem. alloc. (bytes): " & Sys.Get_Alloc_All);
            Tio.Line (Exception_Handle);
            Tio.Put_Line (Exception_Handle, Trace_Output);
            Tio.Put_Line (Exception_Handle, Title_Max_Length * "-");
            --Tio.Line (Exception_Handle);
            Tio.Close (Exception_Handle);
         end if;

      end if;

      Tio.Cursor_On;
      Prg.Set_Exit_Status (9);

   end Exception_Handling;

   function Get_Version return ASU.Unbounded_String is
   begin
      return (ASU.To_Unbounded_String (Name & " v" &
              Ada.Strings.Fixed.Trim (Natural'Image (Version_Major),
              Ada.Strings.Left) & "." &
              Ada.Strings.Fixed.Trim (Natural'Image (Version_Minor),
            Ada.Strings.Left)));
   end Get_Version;

   function Get_Build return ASU.Unbounded_String is
   begin
      return (ASU.To_Unbounded_String ("build " &
              GNAT.Source_Info.Compilation_ISO_Date & " " &
              GNAT.Source_Info.Compilation_Time));
   end Get_Build;

   function Get_Log_Dir return ASU.Unbounded_String is
   begin
     return ASU.To_Unbounded_String (Log_Dir);
   end Get_Log_Dir;

   function Get_Tmp_Dir return ASU.Unbounded_String is
   begin
     return ASU.To_Unbounded_String (Tmp_Dir);
   end Get_Tmp_Dir;

-------------------------------------------------------------------------------
end v20;
-------------------------------------------------------------------------------
