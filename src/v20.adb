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
      Exception_File_Name : constant VString := Prg.Start_Dir & "/" &
                                               Prg.Name & ".err";
   begin

      Tio.Line;
      Tio.Put_Line (Line_Max_Length * "-");
      Tio.Line;
      Tio.Put_Line ("Exception time         : " & Prg.Time_Stamp);
      Tio.Put_Line ("Program uptime         : " &
                     Prg.Duration_Stamp (Prg.Start_Time));
      Tio.Put_Line ("Program build DT stamp : " & v20.Get_Build);
      Tio.Put_Line ("Program name & version : " & Prg.Get_Version);
      Tio.Put_Line ("Library name & version : " & v20.Get_Version);
      Tio.Put_Line ("Start directory        : " & Prg.Start_Dir);
      Tio.Put_Line ("Home directory         : " & Sys.Get_Home);
      Tio.Put_Line ("Ada mem. alloc. (bytes): " & Sys.Get_Alloc_Ada);
      Tio.Put_Line ("All mem. alloc. (bytes): " & Sys.Get_Alloc_All);

      Tio.Line;
      Tio.Put_Line (Ada.Exceptions.Exception_Information (Exception_Hook));
      Tio.Put_Line (Line_Max_Length * "-");
      Tio.Line;

      if Ada.Directories.Exists (To_String (Exception_File_Name)) then
         Tio.Append (Exception_Handle, Exception_File_Name);
      else
         Tio.Create (Exception_Handle, Exception_File_Name);
      end if;

      if Tio.Is_Open (Exception_Handle) then
         Tio.Put_Line (Exception_Handle, Line_Max_Length * "-");
         Tio.Line (Exception_Handle);
         Tio.Put_Line (Exception_Handle, "Exception time        : " &
                                          Prg.Time_Stamp);
         Tio.Put_Line (Exception_Handle, "Program uptime        : " &
                                          Prg.Duration_Stamp (Prg.Start_Time));
         Tio.Put_Line (Exception_Handle, "Program name & version: " &
                                          Prg.Get_Version);
         Tio.Put_Line (Exception_Handle, "Library name & version: " &
                                          v20.Get_Version);
         Tio.Put_Line (Exception_Handle, "Start directory       : " &
                                          Prg.Start_Dir);
         Tio.Put_Line (Exception_Handle, "Home directory        : " &
                                          Sys.Get_Home);
         Tio.Put_Line (Exception_Handle, "Ada memory allocations: " &
                                          Sys.Get_Alloc_Ada);
         Tio.Put_Line (Exception_Handle, "All memory allocations: " &
                                          Sys.Get_Alloc_All);

         Tio.Line (Exception_Handle);
         Tio.Put_Line (Exception_Handle,
                        Ada.Exceptions.Exception_Information (Exception_Hook));
         Tio.Put_Line (Exception_Handle, Line_Max_Length * "-");
         Tio.Line (Exception_Handle);
         Tio.Close (Exception_Handle);
      end if;

      Tio.Cursor_On;
      Prg.Set_Exit_Status (128);

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

-------------------------------------------------------------------------------
end v20;
-------------------------------------------------------------------------------
