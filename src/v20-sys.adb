-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20-Sys.adb
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V20 library System package
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  see v20-Sys.ads
-------------------------------------------------------------------------------

with Ada.Environment_Variables;

with Interfaces.C;

with v20.Log;
with v20.Prg;

package body v20.Sys is

   function Get_Alloc_Ada return String is
      Watermark : constant GM.Watermark_Info := GM.Get_Ada_Allocations;
   begin
      return "Ada Cur: [" & Watermark.Current'Img & " ] Max: " &
                      "[" & Watermark.High'Img & " ]";
   end Get_Alloc_Ada;

   function Get_Alloc_All return String is
      Watermark : constant GM.Watermark_Info := GM.Get_Allocations;
   begin
      return "All Cur: [" & Watermark.Current'Img & " ] Max: " &
                      "[" & Watermark.High'Img & " ]";
   end Get_Alloc_All;

   function Get_Env (Name : String) return VString is
   begin
      if Ada.Environment_Variables.Exists (Name) then
         return To_VString (Ada.Environment_Variables.Value (Name));
      else
         return Null_VString;
      end if;
   end Get_Env;

   function Get_Env (Name : VString) return VString is
   begin
      return Get_Env (To_String (Name));
   end Get_Env;

   function Get_Home return VString is
   begin
      return Get_Env ("HOME");
   end Get_Home;

   procedure Get_Memory_Dump (Size : Positive;
                       Report_View : Report := Memory_Usage) is
   begin                          -- Report_View cast
      GNATCOLL.Memory.Dump (Size, GNATCOLL.Memory.Report_Type (Report_View));
   end Get_Memory_Dump;

   function Install_Package (Package_Name : VString) return Boolean is
      Exec_Output : VString;
      Exec_Error : Integer;
      Package_Installed : constant String := "'install ok installed'";
      type Table_Installers is array (1 .. 3) of VString;
      Installers : constant Table_Installers := (+"apt",
                                                 +"aptitude",
                                                 +"apt-get");
      Installer : VString := +"";
      Result : Boolean := True;
   begin
      --  Is package already installed
      Sys.Shell_Execute ("dpkg-query -W -f='${Status}' " &
                                Package_Name, Exec_Error, Exec_Output);
      --  Exec_Error checkin not needed (0 = found, 1 = not found)
      --  Message output inspection is preferred
      if not (Exec_Output = Package_Installed) then
         --  Select installer (no need to test again Exec_Error)
         for  I in 1 .. Installers'Last loop
            Sys.Shell_Execute ("dpkg-query -W -f='${Status}' " &
                                      Installers (I), Exec_Error, Exec_Output);
            if Exec_Output = Package_Installed then
               Installer := Installers (I);
               exit;
            end if;
         end loop;
         --  Install package
         if not (Installer = "") then
            Log.Msg ("Installing " & Package_Name);
            if Prg.Is_User_Not_Root then
               Installer := "sudo " & Installer;
            end if;
            Sys.Shell_Execute (Installer & " install -y " &
                               Package_Name, Exec_Error);
            if Exec_Error = 0 then
               Log.Msg (Package_Name & " installed successfully.");
            else
               Log.Err ("v20.Sys.Install_Package - Exec error installing: " & Package_Name);
               Result := False;
            end if;
         else
            Log.Err ("v20.Sys.Install_Package - No installer found for: " & Package_Name);
            Result := False;
         end if;
      else
         Log.Msg ("Package " & Package_Name & " already installed.");
      end if;
      return Result;
   end Install_Package;

   function Install_Packages (Packages_List : String) return Boolean is
      Packages_String : constant VString := To_VString (Packages_List);
      Package_Buffer : VString := +"";
      Result : Boolean := True;
   begin
      Log.Msg ("Check system packages dependencies.");

      for I in 1 .. Length (Packages_String) loop

         Package_Buffer := Package_Buffer & Slice (Packages_String, I, I);

         if I = Length (Packages_String) then
            if not Install_Package (Trim_Left (Package_Buffer)) then
               Result := False;
            end if;
            Package_Buffer := +"";

         elsif Ends_With (Package_Buffer, ",") then
            if not Install_Package (Trim_Left (Slice (Package_Buffer, 1,
                                           Length (Package_Buffer) - 1)))
            then
               Result := False;
            end if;
            Package_Buffer := +"";
         end if;
      end loop;
      return Result;
   end Install_Packages;

   procedure Reset_Memory_Monitor is
   begin
      GNATCOLL.Memory.Reset;
   end Reset_Memory_Monitor;

   procedure Set_Memory_Monitor (State : Boolean := True) is
   begin
      GNATCOLL.Memory.Configure (Activate_Monitor => State);
   end Set_Memory_Monitor;

   procedure Shell_Execute_Output (Command : String;
                                   Result : out Integer;
                                   Output : out VString) is
      Arguments : GOL.Argument_List_Access;
      Command_Exit_Code : aliased Integer; --  Must reside in memory (pointer)
   begin
      if Command /= "" then
         Arguments := GOL.Argument_String_To_List (Command);
         Output := To_VString (GE.Get_Command_Output
               (Command => Arguments (Arguments'First).all,
              Arguments => Arguments (Arguments'First + 1 .. Arguments'Last),
              Input => "",
              Status => Command_Exit_Code'Access));
         GOL.Free (Arguments);
      else
         Output := +"";
      end if;
      Result := Command_Exit_Code;
   end Shell_Execute_Output;

   procedure Shell_Execute_Output (Command : VString;
                                   Result : out Integer;
                                   Output : out VString) is
   begin
      Shell_Execute_Output (To_String (Command), Result, Output);
   end Shell_Execute_Output;

   --  https://rosettacode.org/wiki/Execute_a_system_command#Ada
   procedure Shell_Execute (Command : String; Result : out Integer) is
      function Sys (Arg : Interfaces.C.char_array) return Integer;
      pragma Import (C, Sys, "system");
   begin
      Result := Sys (Interfaces.C.To_C (Command));
   end Shell_Execute;

   procedure Shell_Execute (Command : VString; Result : out Integer) is
   begin
      Shell_Execute (To_String (Command), Result);
   end Shell_Execute;
   
   procedure Shell_Execute (Command : String) is
      Dummy : Integer := 0;
   begin
      Shell_Execute (Command, Dummy);
   end Shell_Execute;
   
   procedure Shell_Execute (Command : VString) is
      Dummy : Integer := 0;
   begin
      Shell_Execute (To_String (Command), Dummy);
   end Shell_Execute;
   
   procedure Shell_Execute (Command : String;
                            Result : out Integer;
                            Output : out VString) is
   begin
      Shell_Execute_Output (Command, Result, Output);
   end Shell_Execute;
   
   procedure Shell_Execute (Command : VString;
                            Result : out Integer;
                            Output : out VString) is
   begin
      Shell_Execute_Output (To_String (Command), Result, Output);
   end Shell_Execute;

-------------------------------------------------------------------------------
end v20.Sys;
-------------------------------------------------------------------------------
