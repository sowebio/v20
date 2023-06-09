-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20-Tio.ads
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V20 library test file
--
--  @description
--  Launch and reports validation tests
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io (HAC runtime integration)
--
--  @versions
--  see .ads
-------------------------------------------------------------------------------

with Ada.Integer_Text_IO;

with v20.Fls;
with v20.Log;
with v20.Sys;
with v20.Tio;

package body v20.Tio is

   ----------------------------------------------------------------------------
   --  Terminal
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Animated_Delay (Delay_Seconds : Positive) is
      type Animation is array (1 .. 7) of Character;
      Progress : constant Animation := ('/', '-', '\', '|', '/', '-', '|');
   begin
      for I in 1 .. Delay_Seconds loop
         if I mod 10 = 0 then
            Put ("|");
         elsif I mod 5 = 0 then
            Put ("!");
         else
            Put (".");
         end if;
         for J in 1 .. 7 loop
            Put (Progress (J));
            Cursor_Line_Backward (1);
            delay 0.143; -- 0.143 x 7 = 1001 ms ~ 1s
         end loop;
      end loop;
      Line;
   end Animated_Delay;

   ----------------------------------------------------------------------------
   function Confirm_Twice (User_Prompt_1 : VString ; User_Prompt_2 : VString) return Boolean is
      Answer : Character;
      Result : Boolean := False;
   begin
      Put_Line (User_Prompt_1 & " 'y/n' ?");
      Tio.Get_Immediate (Answer);
      if (Answer = 'y' or Answer = 'Y') then
         Put_Line (User_Prompt_2 & " by pressing 'c'");
         Tio.Get_Immediate (Answer);
         Result := (Answer = 'c' or Answer = 'C');
      end if;
      Line;
      return Result;
   end Confirm_Twice;

   ----------------------------------------------------------------------------
   procedure Beep is
   begin
      ATI.Put (Item => ASCII.BEL);
      ATI.Flush;
   end Beep;

   ----------------------------------------------------------------------------
   procedure Clear_Screen is
   begin
      ATI.Put (ASCII.ESC & "[2J");
      ATI.Flush;
   end Clear_Screen;

   ----------------------------------------------------------------------------
   procedure Cursor_Move (X : Row; Y : Column) is
   begin
      ATI.Flush;
      ATI.Put (ASCII.ESC & "[");
      Ada.Integer_Text_IO.Put (Item => X, Width => 1);
      ATI.Put (Item => ';');
      Ada.Integer_Text_IO.Put (Item => Y, Width => 1);
      ATI.Put (Item => 'f');
   end Cursor_Move;

   ----------------------------------------------------------------------------
   procedure Cursor_Line_Forward (X : Row) is
   begin
      ATI.Put (Item => ASCII.ESC);
      ATI.Put ("[");
      Ada.Integer_Text_IO.Put (Item => X, Width => 1);
      ATI.Put (Item => 'C');
   end Cursor_Line_Forward;

   ----------------------------------------------------------------------------
   procedure Cursor_Line_Backward (X : Row) is
   begin
      ATI.Put (ASCII.ESC & "[");
      Ada.Integer_Text_IO.Put (Item => X, Width => 1);
      ATI.Put (Item => 'D');
   end Cursor_Line_Backward;

   ----------------------------------------------------------------------------
   procedure Cursor_Line_Erase is
   begin
      ATI.Put (ASCII.ESC & "[K");
   end Cursor_Line_Erase;

   ----------------------------------------------------------------------------
   procedure Cursor_Save is
   begin
      ATI.Put (ASCII.ESC & "[s");
   end Cursor_Save;

   ----------------------------------------------------------------------------
   procedure Cursor_Restore is
   begin
      ATI.Put (ASCII.ESC & "[u");
   end Cursor_Restore;

   ----------------------------------------------------------------------------
   procedure Cursor_On is
   begin
      ATI.Put (ASCII.ESC & "[?25h");
   end Cursor_On;

   ----------------------------------------------------------------------------
   procedure Cursor_Off is
   begin
      ATI.Put (ASCII.ESC & "[?25l");
   end Cursor_Off;

   ----------------------------------------------------------------------------
   function Get_Password return VString is
      Exit_Char : constant Character := Character'Val(10); -- LF (return code key)
      Current_Char : Character;
      Result : VString := +"";
   begin
      Tio.Put ("Password:");
      while True loop
         Get_Immediate (Current_Char);
         --Put (To_Val(To_VString(Current_Char)));
         if Current_Char = Exit_Char then
            exit;
         else
            Result := Result & To_VString (Current_Char);
         end if;
      end loop;
      Tio.Line;
      return Result;
   end Get_Password;

   ----------------------------------------------------------------------------
   procedure Pause is
      Dummy : Character;
   begin
      Put ("Press any key to continue or [Ctrl-C] to abort...");
      Get_Immediate (Dummy);
   end Pause;

   ----------------------------------------------------------------------------
   procedure Put (B : Boolean) is
   begin
      if B then
         Put ("True");
      else
         Put ("False");
      end if;
   end Put;

   procedure Put (I : Integer) is
   begin
      Put (To_VString (I));
   end Put;

   procedure Put (I : Long_Integer) is
   begin
      Put (To_VString (Integer (I)));
   end Put;

   procedure Put (I : Integer_64) is
   begin
      Put (To_VString (Integer (I)));
   end Put;

   procedure Put (V : VString) is
   begin
      Put (ASU.To_String (V));
   end Put;

   ----------------------------------------------------------------------------
   procedure Put_Line (B : Boolean) is
   begin
      if B then
         Put ("True");
      else
         Put ("False");
      end if;
      Line;
   end Put_Line;

   procedure Put_Line (I : Integer) is
   begin
      Put (To_VString (I));
      Line;
   end Put_Line;

   procedure Put_Line (I : Long_Integer) is
   begin
      Put (To_VString (Integer (I)));
      Line;
   end Put_Line;

   procedure Put_Line (I : Integer_64) is
   begin
      Put (To_VString (Integer (I)));
      Line;
   end Put_Line;

   procedure Put_Line (C : Character) is
   begin
      Put (C);
      Line;
   end Put_Line;

   procedure Put_Line (V : VString) is
   begin
      Put_Line (ASU.To_String (V));
   end Put_Line;

   ----------------------------------------------------------------------------
   --  File
   ----------------------------------------------------------------------------

   procedure Open_Conf (Handle : in out File; Name : String ;
                       Wipe_Before_Process : Boolean := False ;
                       Permissions : VString := +"") is
      Dir_File : constant VString := Fls.Extract_Directory (Name);
      SE_Result : Integer := 0;
   begin
      --  Preventive house keeping
      if Fls.Exists (Name) then
         Fls.Backup_File (Name);
         if Wipe_Before_Process then
            Fls.Delete_File (Name);
         end if;
      end if;

      if Fls.Exists (Name) then
         Append (Handle, Name);
      else
         -- Ensure that the complete tree structure exists before creating file
         if Fls.Create_Directory_Tree (Dir_File) then
            Create (Handle, Name);
         else
            Log.Err ("Tio.Open_Conf > Can't create directory: " & Dir_File);
         end if;
      end if;

      -- Apply optional permissions
      if Fls.Exists (Name) and not Empty (Permissions) then
         Sys.Shell_Execute (+"chmod 0600 " & Name, SE_Result);
         if SE_Result /= 0 then
            Log.Err  (+"Tio.Open_Conf > Can't apply permissions to: " & Name);
          end if;
      end if;

   end Open_Conf;

   procedure Open_Conf (Handle : in out File; Name : VString ;
                       Wipe_Before_Process : Boolean := False ;
                       Permissions : VString := +"") is
   begin
      Open_Conf (Handle, Vst.To_String (Name), Wipe_Before_Process, Permissions);
   end Open_Conf;

   ----------------------------------------------------------------------------
   procedure Open_Read (Handle : in out File; Name : String) is
   begin
      ATI.Open (Handle, ATI.In_File, Name);
   end Open_Read;

   procedure Open_Read (Handle : in out File; Name : VString) is
   begin
      Open_Read (Handle, Vst.To_String (Name));
   end Open_Read;

   ----------------------------------------------------------------------------
   procedure Create (Handle : in out File; Name : String) is
   begin
      ATI.Create (Handle, ATI.Out_File, Name);
   end Create;

   procedure Create (Handle : in out File; Name : VString) is
   begin
      Create (Handle, Vst.To_String (Name));
   end Create;

   ----------------------------------------------------------------------------
   procedure Append (Handle : in out File; Name : String) is
   begin
      ATI.Open (Handle, ATI.Append_File, Name);
   end Append;

   procedure Append (Handle : in out File; Name : VString) is
   begin
      Append (Handle, Vst.To_String (Name));
   end Append;

   ----------------------------------------------------------------------------
   procedure Put (Handle : File; V : VString) is
   begin
      Put (Handle, Vst.To_String (V));
   end Put;

   ----------------------------------------------------------------------------
   procedure Put_Line (Handle : File; C : Character) is
   begin
      Put (Handle, C);
      Line (Handle);
   end Put_Line;

   procedure Put_Line (Handle : File; V : VString) is
   begin
      Put_Line (Handle, ASU.To_String (V));
   end Put_Line;

   ----------------------------------------------------------------------------
   function Read_File (File_Name : VString) return VString is
      File_Handle : File;
      Line_Buffer, Result_Buffer : VString := +"";
   begin
      if Fls.Exists (File_Name) then
         Tio.Open_Read (File_Handle, File_Name);
         while not (End_Of_File (File_Handle)) loop
            Get_Line (File_Handle, Line_Buffer);
            Result_Buffer := Result_Buffer & Line_Buffer & LF;
         end loop;
         Close (File_Handle);
      else
         Log.Err (+"Tio.Read_File > File does not exist: " & File_Name);
      end if;
      return Result_Buffer;
   end Read_File;

   ----------------------------------------------------------------------------
   procedure Write_File (File_Name : VString ; Content : VString ; Permissions : VString := +"") is
      Index_Next : Integer := 1;
      String_To_Process : VString := +"";
      Dir_File : constant VString := Fls.Extract_Directory (File_Name);
      File_Handle : File;
      SE_Result : Integer := 0;
   begin

      -- Preventive housekeeping
      Fls.Delete_File (File_Name);

      -- Ensure that the full tree structure exists before creating file
      if Fls.Create_Directory_Tree (Dir_File) then
         Create (File_Handle, File_Name);

         -- Apply optional permissions
         if Fls.Exists (File_Name) and not Empty (Permissions) then
            Sys.Shell_Execute (+"chmod 0600 " & File_Name, SE_Result);
            if SE_Result /= 0 then
               Log.Err  (+"Tio.Open_Conf > Can't apply permissions to: " & File_Name);
            end if;
         end if;

         -- Adding trailing LF if missing
         String_To_Process := Content & (if Slice (Content, Length (Content)) = LF then +"" else To_VString (LF));
         while True loop
            Index_Next := Index (String_To_Process, LF);
            Put_Line (File_Handle, Slice (String_To_Process, 1, Index_Next - 1));
            String_To_Process := Slice (String_To_Process, Index_Next + 1, Length (String_To_Process));
            if Length (String_To_Process) <= 1 then -- Last field reached
               exit;
            end if;
         end loop;

         Close (File_Handle);

      else
         Log.Err ("Tio.Write_File > Can't create directory: " & Dir_File);
      end if;

   end Write_File;

   ----------------------------------------------------------------------------
   procedure Get_Line (Handle : File; V : out VString) is
   begin
      V := To_VString (ATI.Get_Line (Handle));
   end Get_Line;

------------------------------------------------------------------------------
end v20.Tio;
------------------------------------------------------------------------------
