-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20-Fls.adb
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V20 library File System package
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  see v20.ads
-------------------------------------------------------------------------------

with Ada.IO_Exceptions;

with v20.Log;
with v20.Sys;
with v20.Tio;

package body v20.Fls is

   --  Private functions

   ----------------------------------------------------------------------------
   procedure Log_Err (Err_Exc : Ada.Exceptions.Exception_Occurrence) is
   begin
      Log.Err ("v20.Fls.Exception: " & 
                           Ada.Exceptions.Exception_Information (Err_Exc));
   end Log_Err;

   --  Public functions
   
   ----------------------------------------------------------------------------
   procedure Backup_File (File_To_Backup : VString) is
      Backup_File : constant VString := File_To_Backup & ".bak.";
      Backup_To_Delete : Integer := 0;
   begin 
      for I in 0 .. 9 loop
         if not Exists (Backup_File & To_VString (I)) then
            Move_File (File_To_Backup, Backup_File & To_VString (I));
            if I = 9 then
               Backup_To_Delete := 0;
            else
               Backup_To_Delete := I + 1;
            end if;
            if Exists (Backup_File & To_VString (Backup_To_Delete)) then
               Delete_File (Backup_File & To_VString (Backup_To_Delete));
            end if;
            exit;
         end if; 
      end loop;
   end Backup_File;

   procedure Backup_File (File_To_Backup : String) is
   begin
      Backup_File (To_VString (File_To_Backup));
   end Backup_File;

   ----------------------------------------------------------------------------
   procedure Copy_File (Source_Name, Target_Name : String) is
      Destination_Valid : Boolean := True;
   begin
      -- Check if destination is current or contains a leading directory
      if (Index (To_VString (Target_Name), "/") > 0) then
         if not (Exists (Slice (To_VString (Source_Name), 1,
                             Index_Backward (To_VString (Source_Name), "/"))))
         then
            Destination_Valid := False;
            Log.Err ("v20.Fls.Copy_File > Destination directory does not exist: " & Target_Name);
         end if;
      end if;  
      if Destination_Valid then      
         AD.Copy_File (Source_Name, Target_Name, Copy_Form);
      end if;
   exception
      when Error : Ada.IO_Exceptions.Use_Error =>
         Log_Err (Error);
      when Error : Ada.IO_Exceptions.Name_Error =>
         Log_Err (Error);
   end Copy_File;

   procedure Copy_File (Source_Name, Target_Name : VString) is
   begin
      Copy_File (ASU.To_String (Source_Name), ASU.To_String (Target_Name));
   end Copy_File;

   procedure Copy_File (Source_Name : VString; Target_Name : String) is
   begin
      Copy_File (ASU.To_String (Source_Name), Target_Name);
   end Copy_File;

   procedure Copy_File (Source_Name : String; Target_Name : VString) is
   begin
      Copy_File (Source_Name, ASU.To_String (Target_Name));
   end Copy_File;

   ----------------------------------------------------------------------------
   function Create_Directory_Tree (Dir_Tree : String) return Boolean is
   begin
      if AD.Exists (Dir_Tree) then
         return True;
      else
         AD.Create_Path (Dir_Tree);
         return AD.Exists (Dir_Tree);
      end if;
   exception
      when Error : Ada.IO_Exceptions.Use_Error =>
         Log_Err (Error);
         return False;
      when Error : Ada.IO_Exceptions.Name_Error =>
         Log_Err (Error);
         return False;
   end Create_Directory_Tree;

   function Create_Directory_Tree (Dir_Tree : VString) return Boolean is
   begin
      return Create_Directory_Tree (To_String (Dir_Tree));
   end Create_Directory_Tree;

   ----------------------------------------------------------------------------
   function Delete_Directory_Tree (Dir_Tree : String) return Boolean is
      Result : Boolean := False;
   begin
      if not If_Root_Directory (Dir_Tree) then
         if AD.Exists (Dir_Tree) then
            AD.Delete_Tree (Dir_Tree);
            if not AD.Exists (Dir_Tree) then
               Result := True;
            end if;
         else
            Result := True;
         end if;
      else
         Log.Err ("Fls.Delete_Directory_Tree - Attempt to delete a root directory: " & Dir_Tree);
      end if;   
      return Result;
   exception
      when Error : Ada.IO_Exceptions.Use_Error =>
         Log_Err (Error);
         return False;
      when Error : Ada.IO_Exceptions.Name_Error =>
         Log_Err (Error);
         return False;
   end Delete_Directory_Tree;

   function Delete_Directory_Tree (Dir_Tree : VString) return Boolean is
   begin
      return Delete_Directory_Tree (To_String (Dir_Tree));
   end Delete_Directory_Tree;
   
   procedure Delete_Directory_Tree (Dir_Tree : String) is
      Dummy : Boolean;
   begin
      Dummy := Delete_Directory_Tree (Dir_Tree);
   end Delete_Directory_Tree;
   
   procedure Delete_Directory_Tree (Dir_Tree : VString) is
      Dummy : Boolean;
   begin
      Dummy := Delete_Directory_Tree (To_String (Dir_Tree));
   end Delete_Directory_Tree;

   ----------------------------------------------------------------------------
   procedure Delete_File (Name : String) is
   begin
      if AD.Exists (Name) then
         AD.Delete_File (Name);
      end if;
   exception
      when Error : Ada.IO_Exceptions.Use_Error =>
         Log_Err (Error);
      when Error : Ada.IO_Exceptions.Name_Error =>
         Log_Err (Error);
   end Delete_File;

   procedure Delete_File (Name : VString) is
   begin
      if AD.Exists (ASU.To_String (Name)) then
         AD.Delete_File (ASU.To_String (Name));
      end if;
   end Delete_File;

   ----------------------------------------------------------------------------
   procedure Delete_Lines (File_Name, Pattern : String) is
      File_Write : constant String := File_Name  & ".new";
      Handle_Read, Handle_Write : Tio.File;
      Line_Deleted : Boolean := False;
      Line_Buffer : VString := +"";
   begin
      if Fls.Exists (File_Name) then
         Tio.Open_Read (Handle_Read, File_Name);
         if Tio.Is_Open (Handle_Read) then
            Tio.Create (Handle_Write, File_Write);
            if Tio.Is_Open (Handle_Write) then
               while not Tio.End_Of_File (Handle_Read) loop
                  Tio.Get_Line (Handle_Read, Line_Buffer);
                  if Index (Line_Buffer, Pattern) > 0 then
                     Line_Deleted := True;
                  else
                     Tio.Put_Line (Handle_Write, Line_Buffer);
                  end if;
               end loop;
               Tio.Close (Handle_Write);
            end if;
            Tio.Close (Handle_Read);
         end if;
         if Line_Deleted then
            Rename (File_Name, File_Name & ".bak");
            Rename (File_Write, File_Name);
         end if;
      else
         Log.Err ("v20.Fls.Delete_Lines > Can't find: " & File_Name);
      end if;
   end Delete_Lines;

   procedure Delete_Lines (File_Name, Pattern : VString) is
   begin
      Delete_Lines (To_String (File_Name), To_String (Pattern));
   end Delete_Lines;

   procedure Delete_Lines (File_Name : String; Pattern : VString) is
   begin
      Delete_Lines (File_Name, To_String (Pattern));
   end Delete_Lines;

   procedure Delete_Lines (File_Name : VString; Pattern : String) is
   begin
      Delete_Lines (To_String (File_Name), Pattern);
   end Delete_Lines;

   ----------------------------------------------------------------------------
   function Download_File (Url : VString;
                           Dlfile : VString;
                           Name : VString := +"";
                           DlSize : Integer := 0) return Boolean is
      Exec_Error : Integer;
      Result : Boolean := False;
      Message_Name : VString := Name;
   begin
      if Vst.Length (Name) = 0 then
         Message_Name := Url;
      end if;
      
      --  Processing existing file
      if Fls.Exists (Dlfile) then
         if DlSize > 0 then
            --  If Size correct
            if Fls.File_Size (Dlfile) = DlSize then
               Result := True;
               Log.Msg ("Keep existing and valid file: " & Dlfile);
            end if;
         end if;
         if not Result then
            Log.Msg ("Delete old file: " & Dlfile);
            Fls.Delete_File (Dlfile);
         end if;
      end if;
      --  Proceed to download if needed
      if not Result then
         Log.Msg ("Download file: " & Message_Name);
         
         -- http1.1 to avoid curl error 'HTTP/2 stream 0 was not closed cleanly'
         Sys.Shell_Execute ("curl --http1.1 --location --output " &
                         Dlfile & " " & Url, Exec_Error);
         if (Exec_Error = 0) and Fls.Exists (Dlfile) then
            if DlSize > 0 then
               if Fls.File_Size (Dlfile) = DlSize then
                  Result := True;
               end if;
            else
               Result := True;
            end if;
         end if;
         if not Result then
            Log.Err ("Fls.Download_File > Download file failed: " & Message_Name);
         end if;
      end if;
      return Result;
   end Download_File;

   ----------------------------------------------------------------------------
   function Exists (Name : VString) return Boolean is
   begin
      return Exists (To_String (Name));
   end Exists;

   ----------------------------------------------------------------------------
   function Extract_Directory (Name : VString) return VString is
      Slash : constant VString := +"/";
      Dir_End : Natural;
      Dir_Result : VString := +"";
   begin
      Dir_End := Index_Backward (Name, Slash);
      if (Dir_End > 1) then 
         Dir_Result := Slice (Name, 1, Dir_End - 1);
      end if;
      return Dir_Result;
   end Extract_Directory;
      
   function Extract_Directory (Name : String) return VString is
   begin
      return Extract_Directory (To_VString (Name));
   end Extract_Directory;
   
   ----------------------------------------------------------------------------   
   function Extract_Filename (Name : VString) return VString is
   begin
      return Tail_After_Match (Name, '/');
   end Extract_Filename;
      
   function Extract_Filename (Name : String) return VString is
   begin
      return Extract_Filename (To_VString (Name));
   end Extract_Filename;

   ----------------------------------------------------------------------------
   function File_Size (Name : String) return Integer is
   begin
      return To_Integer (AD.File_Size'Image (AD.Size (Name)));
   exception
      when Error : Ada.IO_Exceptions.Use_Error =>
         Log_Err (Error);
         return 0;
      when Error : Ada.IO_Exceptions.Name_Error =>
         Log_Err (Error);
         return 0;
   end File_Size;

   function File_Size (Name : VString) return Integer is
   begin
      return File_Size (To_String (Name));
   end File_Size;

   ----------------------------------------------------------------------------
   function Get_Directory return VString is
   begin
      return To_VString (AD.Current_Directory);
   end Get_Directory;
   
   ----------------------------------------------------------------------------
   function If_Root_Directory (Dir_Tree : VString) return Boolean is
      Test_Dir_Tree : VString := Dir_Tree;
      Slash : constant VString := +"/";
      Root_Dirs : constant VString := +"bin,boot,dev,etc,home,lib,lib32,lib64," & 
         "libx32,lost+found,media,mnt,opt,proc,root,run,sbin,srv,sys," & 
         "tmp,usr,var,Test_Delete_Directory_Tree";
      Result : Boolean := False;
   begin
      -- Dir_Tree must be fully qualified, ie starting with a slash (/)
      if Starts_With (Dir_Tree, Slash) then
         --  Add a slash if Dir_Tree does not ends with it
         if not Ends_With (Dir_Tree, Slash) then
            Test_Dir_Tree := Test_Dir_Tree & Slash;
         end if;
         --  If Dir_Tree counts only two slashes and at least one char between
         if (Char_Count (Test_Dir_Tree, Slash) = 2) and
            (Length (Dir_Tree) > 2) then
            Test_Dir_Tree := Slice (Test_Dir_Tree, 2, Length (Test_Dir_Tree) - 1);
            if not Empty (Field_By_Name (Root_Dirs, Test_Dir_Tree, To_String (Slash))) then
               Result := True;
            end if;
         end if;
      end if;
      return Result;
   end If_Root_Directory;
   
   function If_Root_Directory (Dir_Tree : String) return Boolean is
   begin
      return If_Root_Directory (To_VString (Dir_Tree));
   end If_Root_Directory;
    
   ----------------------------------------------------------------------------
   procedure Move_File (Source_Name, Target_Name : String) is
   begin
      Copy_File (Source_Name, Target_Name);
      Delete_File (Source_Name);
   end Move_File;

   procedure Move_File (Source_Name, Target_Name : VString) is
   begin
      Move_File (ASU.To_String (Source_Name), ASU.To_String (Target_Name));
   end Move_File;

   procedure Move_File (Source_Name : VString; Target_Name : String) is
   begin
      Move_File (ASU.To_String (Source_Name), Target_Name);
   end Move_File;

   procedure Move_File (Source_Name : String; Target_Name : VString) is
   begin
      Move_File (Source_Name, ASU.To_String (Target_Name));
   end Move_File;

   ----------------------------------------------------------------------------
   procedure Rename (Old_Name, New_Name : String) is
      use Ada.Directories;  --  For File_Kind.Directory visibility
   begin
      if Exists (New_Name) then
         if not (Kind (New_Name) = Directory) then
            AD.Delete_File (New_Name);
         end if;
      end if;
      AD.Rename (Old_Name, New_Name);
   exception
      when Error : Ada.IO_Exceptions.Use_Error =>
         Log_Err (Error);
      when Error : Ada.IO_Exceptions.Name_Error =>
         Log_Err (Error);
   end Rename;

   procedure Rename (Old_Name, New_Name : VString) is
   begin
      Rename (To_String (Old_Name), To_String (New_Name));
   end Rename;

   procedure Rename (Old_Name : String; New_Name : VString) is
   begin
      Rename (Old_Name, To_String (New_Name));
   end Rename;

   procedure Rename (Old_Name : VString; New_Name : String) is
   begin
      Rename (To_String (Old_Name), New_Name);
   end Rename;

   ----------------------------------------------------------------------------
   function Search_Lines (File_Name, Pattern : String) return Boolean is
      Handle_Read : Tio.File;
      Line_Found : Boolean := False;
      Line_Buffer : VString := +"";
   begin
      Tio.Open_Read (Handle_Read, File_Name);
      if Tio.Is_Open (Handle_Read) then
         while not Tio.End_Of_File (Handle_Read) loop
            Tio.Get_Line (Handle_Read, Line_Buffer);
            if Index (Line_Buffer, Pattern) > 0 then
               Line_Found := True;
               exit;
            end if;
         end loop;
         Tio.Close (Handle_Read);
      end if;
      return Line_Found;
   end Search_Lines;

   function Search_Lines (File_Name, Pattern : VString) return Boolean is
   begin
      return Search_Lines (To_String (File_Name), To_String (Pattern));
   end Search_Lines;

   function Search_Lines (File_Name : String; Pattern : VString)
                                                             return Boolean is
   begin
      return Search_Lines (File_Name, To_String (Pattern));
   end Search_Lines;

   function Search_Lines (File_Name : VString; Pattern : String)
                                                             return Boolean is
   begin
      return Search_Lines (To_String (File_Name), Pattern);
   end Search_Lines;

   ----------------------------------------------------------------------------
   function Set_Directory (Directory : String) return Boolean is
      Result : Boolean := False;
   begin
      if AD.Exists (Directory) then
         AD.Set_Directory (Directory);
         Result := True;
      end if;
      return Result;
   exception
      when Error : Ada.IO_Exceptions.Use_Error =>
         Log_Err (Error);
         return False;
      when Error : Ada.IO_Exceptions.Name_Error =>
         Log_Err (Error);
         return False;
   end Set_Directory;

   function Set_Directory (Directory : VString) return Boolean is
   begin
      return Set_Directory (To_String (Directory));
   end Set_Directory;

-------------------------------------------------------------------------------
end v20.Fls;
-------------------------------------------------------------------------------
