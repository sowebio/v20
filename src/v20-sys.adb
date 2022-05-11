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

   package AEV renames Ada.Environment_Variables;

   ---------------------------------------------------------------------------
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

   ---------------------------------------------------------------------------
   function Get_Env (Name : String) return VString is
   begin
      if AEV.Exists (Name) then
         return To_VString (AEV.Value (Name));
      else
         return Null_VString;
      end if;
   end Get_Env;

   function Get_Env (Name : VString) return VString is
   begin
      return Get_Env (To_String (Name));
   end Get_Env;
   
   ---------------------------------------------------------------------------
   function Get_Home return VString is
   begin
      return Get_Env ("HOME");
   end Get_Home;

   procedure Get_Memory_Dump (Size : Positive;
                       Report_View : Report := Memory_Usage) is
   begin                          -- Report_View cast
      GNATCOLL.Memory.Dump (Size, GNATCOLL.Memory.Report_Type (Report_View));
   end Get_Memory_Dump;

   ---------------------------------------------------------------------------
   function Is_Package (Package_Name : VString; Host_Name : VString := +"") return Boolean is
      Exec_Error : Integer;
      Check_Command : constant VString := "dpkg -s " & Package_Name & " | grep Status:";
  --    Exec_Output_Local : VString := +"'install ok installed'";
  --    Exec_Output_Local : VString := +"install ok installed'";
   begin
      if Empty (Host_Name) then
         -- Exec_Error checking is irrelevant with dpkg-query, allways check 
         -- message output. In addition, dpkg-query -W -f='${Status}' method
         -- is not recommended as the result is not reliable. Indeed, with a 
         -- package available in, for example, two architectures, as libcurl4
         -- (libcurl4:amd64, libcurl4:i386), the response string 
         -- 'install ok installed' appears two times instead of once.
         Sys.Shell_Execute (Check_Command & STD_ERR_OUT_REDIRECT, Exec_Error); --, Exec_Output);
      else
         Sys.Shell_Execute (+"ssh -q -o StrictHostKeyChecking=no " & Host_Name & " " & DQ & 
                            Check_Command & DQ & STD_ERR_OUT_REDIRECT, Exec_Error);
      end if;
      return (Exec_Error = 0);
   end Is_Package;

   ---------------------------------------------------------------------------
   function Install_Package (Package_Name : VString; Host_Name : VString) return Boolean is
      Exec_Error : Integer;
      -- apt and aptitude are for user's end (more human friendly but 
      -- unstable CLI between versions), apt-get is for scripts (stable CLI)
      Installer : VString := +"apt-get";
      Result : Boolean := True;
   begin
      if not Is_Package (Package_Name, Host_Name) then
         Log.Msg ("Installing " & Package_Name);
         if Empty (Host_Name) then
            if Prg.Is_User_Not_Root then
               Installer := "sudo " & Installer;
            end if;
            Sys.Shell_Execute (Installer & " install -y " & Package_Name & STD_ERR_OUT_REDIRECT, Exec_Error);
         else
            -- Use of Exec_Output seems to disturb results. Definitly we should handle redirection analysis through STD and ERR files
            Sys.Shell_Execute (+"ssh -q -o StrictHostKeyChecking=no " & Host_Name & " " & 
               DQ & Installer & " install -y " & Package_Name & DQ & STD_ERR_OUT_REDIRECT, Exec_Error);
         end if;
         
         if Exec_Error = 0 then
            Log.Msg (Package_Name & " installed successfully.");
         else
            Log.Err ("v20.Sys.Install_Package: Exec error installing: " & Package_Name);
            Result := False;
         end if;
      else
         Log.Msg ("Package " & Package_Name & " already installed.");
      end if;
      return Result;
   end Install_Package;

   function Install_Packages (Packages_List : String; Host_Name : VString := +"") return Boolean is
      Packages_String : constant VString := To_VString (Packages_List);
      Packages_Count : Natural;
      Result : Boolean := True;
   begin
      Log.Msg ("Check packages to install.");
      Packages_Count := Field_Count (Packages_String, VD);
      if (Packages_Count > 0) then
         -- Install packages
         for I in 1 .. Packages_Count loop
            -- The returned value is not relevant as managed dependencies by package manager may interfere
            Result := Install_Package (Field_By_Index (Packages_String, I, VD), Host_Name);
         end loop;
         -- Check installed packages
         Result := True;
         for I in 1 .. Packages_Count loop
            if not Is_Package (Field_By_Index (Packages_String, I, VD), Host_Name) then
               Result := False;
               exit;
            end if;
         end loop;
      end if;
      return Result;
   end Install_Packages;
   
   ---------------------------------------------------------------------------
   function Purge_Package (Package_Name : VString; Host_Name : VString) return Boolean is
      Exec_Error : Integer;
      -- type Table_Installers is array (1 .. 3) of VString;
      -- apt and aptitude are for user's end (more human friendly but 
      -- unstable CLI between versions), apt-get is for scripts (stable CLI)
      Installer : VString := +"apt-get";
      Result : Boolean := True;
   begin
      if Is_Package (Package_Name, Host_Name) then
         Log.Msg ("Installing " & Package_Name);
         if Empty (Host_Name) then
            if Prg.Is_User_Not_Root then
               Installer := "sudo " & Installer;
            end if;
            Sys.Shell_Execute (Installer & " purge -y " & Package_Name & STD_ERR_OUT_REDIRECT, Exec_Error);
         else
            Sys.Shell_Execute (+"ssh -q -o StrictHostKeyChecking=no " & Host_Name & " " & 
               DQ & Installer & " purge -y " & Package_Name & DQ & STD_ERR_OUT_REDIRECT, Exec_Error); 
         end if;
         
         if Exec_Error = 0 then
            Log.Msg (Package_Name & " installed successfully.");
         else
            Log.Err ("v20.Sys.Install_Package: Exec error purging: " & Package_Name);
            Result := False;
         end if;
      else
         Log.Msg ("Package " & Package_Name & " not yet installed.");
      end if;
      return Result;
   end Purge_Package;

   function Purge_Packages (Packages_List : String; Host_Name : VString := +"") return Boolean is
      Packages_String : constant VString := To_VString (Packages_List);
         Packages_Count : Natural;
      Result : Boolean := True;
   begin
      Log.Msg ("Check packages to purge.");
      Packages_Count := Field_Count (Packages_String, VD);
      if (Packages_Count > 0) then
         -- Purge packages
         for I in 1 .. Packages_Count loop
            -- The returned value is not relevant as managed dependencies by package manager may interfere
            Result := Purge_Package (Field_By_Index (Packages_String, I, VD), Host_Name);
         end loop;
         -- Check purged packages
         Result := True;
         for I in 1 .. Packages_Count loop
            if Is_Package (Field_By_Index (Packages_String, I, VD), Host_Name) then
               Result := False;
               exit;
            end if;
         end loop;
      end if;
      return Result;
   end Purge_Packages;
   
   ---------------------------------------------------------------------------
   procedure Reset_Memory_Monitor is
   begin
      GNATCOLL.Memory.Reset;
   end Reset_Memory_Monitor;
   
   ---------------------------------------------------------------------------
   procedure Set_Env (Name : String; Value : String) is
   begin
      AEV.Set (Name, Value);
   end Set_Env;
   
   procedure Set_Env (Name : VString; Value : String) is
   begin
      Set_Env (To_String (Name), Value);
   end Set_Env;
   
   procedure Set_Env (Name : String; Value : VString) is
   begin
      Set_Env (Name, To_String (Value));
   end Set_Env;
   
   procedure Set_Env (Name : VString; Value : VString) is
   begin
     Set_Env (To_String (Name), To_String (Value));
   end Set_Env;

   ---------------------------------------------------------------------------
   procedure Set_Memory_Monitor (State : Boolean := True) is
   begin
      GNATCOLL.Memory.Configure (Activate_Monitor => State);
   end Set_Memory_Monitor;

   ---------------------------------------------------------------------------
   function Is_Command (Command : String) return Boolean is
      function Sys (Arg : Interfaces.C.char_array) return Integer;
      pragma Import (C, Sys, "system");
      -- Stript parameters if exists (or not) because "which" needs command without parameter
      Command_Processed : constant String := To_String (Field_By_Index (Trim_Both(To_VString (Command)),1,SP));
   begin
      return (Sys (Interfaces.C.To_C ("which " & Command_Processed & STD_ERR_OUT_REDIRECT)) = 0);
   end Is_Command;
   
   function Is_Command (Command : VString) return Boolean is
   begin
      return Is_Command (To_String (Command ));
   end Is_Command;
   
   ---------------------------------------------------------------------------
   procedure Shell_Execute_Output (Command : String;
                                   Result : out Integer;
                                   Output : out VString) is
      Arguments : GOL.Argument_List_Access;
      Command_Exit_Code : aliased Integer; --  Must reside in memory (pointer)
   begin
      if Is_Command (Command) then
         Arguments := GOL.Argument_String_To_List (Command);
         Output := To_VString (GE.Get_Command_Output
               (Command => Arguments (Arguments'First).all,
              Arguments => Arguments (Arguments'First + 1 .. Arguments'Last),
              Input => "",
              Status => Command_Exit_Code'Access));
         GOL.Free (Arguments);
         Result := Command_Exit_Code;
      else
         Output := +"";
         Result := 255;
      end if;
   end Shell_Execute_Output;
   
   procedure Shell_Execute_Output (Command : VString;
                                   Result : out Integer;
                                   Output : out VString) is
   begin
      Shell_Execute_Output (To_String (Command), Result, Output);
   end Shell_Execute_Output;
   
   ---------------------------------------------------------------------------
   --  https://rosettacode.org/wiki/Execute_a_system_command#Ada
   procedure Shell_Execute (Command : String; Result : out Integer) is
      function Sys (Arg : Interfaces.C.char_array) return Integer;
      pragma Import (C, Sys, "system");
   begin
      if Is_Command (Command) then
         Result := Sys (Interfaces.C.To_C (Command));
      else
         Result := 255;
      end if;
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
