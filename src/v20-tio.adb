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

package body v20.Tio is

   --  Terminal

   ----------------------------------------------------------------------------
   procedure Pause is
      Dummy : Character;
   begin
      Put ("Press any key to continue or [Ctrl-C] to abort...");
      Get_Immediate (Dummy);
   end Pause;

   ----------------------------------------------------------------------------
   procedure Put (V : VString) is
   begin
      Put (ASU.To_String (V));
   end Put;

   procedure Put_Line (I : Integer) is
   begin
      Put (To_VString (I));
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
      ATI.Put (Item => ASCII.ESC);
      ATI.Put (Item => "[2J");
      ATI.Flush;
   end Clear_Screen;

   ----------------------------------------------------------------------------
   procedure Cursor_Move (X : Row; Y : Column) is
   begin
      ATI.Flush;
      ATI.Put (Item => ASCII.ESC);
      ATI.Put ("[");
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
      ATI.Put (Item => ASCII.ESC);
      ATI.Put ("[");
      Ada.Integer_Text_IO.Put (Item => X, Width => 1);
      ATI.Put (Item => 'D');
   end Cursor_Line_Backward;

   ----------------------------------------------------------------------------
   procedure Cursor_Line_Erase is
   begin
      ATI.Put (Item => ASCII.ESC);
      ATI.Put ("[K");
   end Cursor_Line_Erase;

   ----------------------------------------------------------------------------
   procedure Cursor_Save is
   begin
      ATI.Put (Item => ASCII.ESC);
      ATI.Put ("[s");
   end Cursor_Save;

   ----------------------------------------------------------------------------
   procedure Cursor_Restore is
   begin
      ATI.Put (Item => ASCII.ESC);
      ATI.Put ("[u");
   end Cursor_Restore;

   --  File

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
   procedure Get_Line (Handle : File; V : out VString) is
   begin
      V := To_VString (ATI.Get_Line (Handle));
   end Get_Line;

------------------------------------------------------------------------------
end v20.Tio;
------------------------------------------------------------------------------
