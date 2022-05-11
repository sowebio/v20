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

package body v20.Tio is

   --  Terminal

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

   --  File

   procedure Open_Conf (Handle : in out File; Name : String ;
                       Wipe_Before_Process : Boolean := False) is
      Dir_File : constant VString := Fls.Extract_Directory (Name);
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
            Log.Err ("Tio.Open_Conf: Can't create directory: " & Dir_File);
         end if;
      end if;

   end Open_Conf;

   procedure Open_Conf (Handle : in out File; Name : VString ;
                       Wipe_Before_Process : Boolean := False) is
   begin
      Open_Conf (Handle, Vst.To_String (Name), Wipe_Before_Process);
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
   function Read_File (File_To_Read : VString) return VString is
      -- Read a text file File_To_Read and returning a VString buffer. LF
      -- (line feed) are preserved.
      File_Handle : File;
      Line_Buffer, Result_Buffer : VString := +"";
   begin
      if Fls.Exists (File_To_Read) then
         Tio.Open_Read (File_Handle, File_To_Read);
         while not (End_Of_File (File_Handle)) loop
            Get_Line (File_Handle, Line_Buffer);
            Result_Buffer := Result_Buffer & Line_Buffer & LF;
         end loop;
         Close (File_Handle);
      else
         Log.Err (+"Tio.Read_File: File does not exist: " & File_To_Read);
      end if;
      return Result_Buffer;
   end Read_File;

   ----------------------------------------------------------------------------
   procedure Write_File (File_To_Write : VString ; Content : VString) is
      -- Write a text file File_To_Write with Content. LF in content are
      -- preserved and used as line feed
      Index_Next : Integer := 1;
      String_To_Process : VString := +"";
      File_Handle : File;
   begin
      -- Adding trailing LF if missing
      String_To_Process := Content & (if Slice (Content, Length (Content)) = LF then +"" else To_VString (LF));

      Open_Conf (File_Handle, File_To_Write, Wipe_Before_Process => True);
      while True loop
         Index_Next := Index (String_To_Process, LF);
         Put_Line (Slice (String_To_Process, 1, Index_Next - 1));
         String_To_Process := Slice (String_To_Process, Index_Next + 1, Length (String_To_Process));
         if Length (String_To_Process) <= 1 then -- Last field reached
            exit;
         end if;
      end loop;
      Close (File_Handle);
   end Write_File;

   ----------------------------------------------------------------------------
   procedure Get_Line (Handle : File; V : out VString) is
   begin
      V := To_VString (ATI.Get_Line (Handle));
   end Get_Line;

------------------------------------------------------------------------------
end v20.Tio;
------------------------------------------------------------------------------
