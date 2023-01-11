------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20-Log.adb
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
------------------------------------------------------------------------------
--  @summary
--  v20 library Log package
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  see .ads
------------------------------------------------------------------------------

with Ada.Characters.Handling;

with v20.Fls;

package body v20.Log is

   --  Private functions

   ----------------------------------------------------------------------------
   procedure Put (Line_In : String;
                  Line_Level : String;
                  Title_On : Boolean := False) is
      Line : VString := To_VString (Line_In);
      Line_Disk : VString := Line;
      Line_Task : VString := +"";
      Ansi_Begin : VString := +"";
      Ansi_End : VString := +"";
   begin

      if Display_On and Tio.Ansi then
         if Line_Level = "DBG" then
            Ansi_Begin := To_VString (CONSOLE_COLOR_YELLOW);
         elsif Line_Level = "ERR" then
            Ansi_Begin := To_VString (CONSOLE_COLOR_RED);
         end if;
         Ansi_End := To_VString (CONSOLE_COLOR_RESET);
      end if;

      --  Task length control
      if Length (Task_State) > Task_Max_Length then
         --                  these two numbers must be equals v     v
         Line_Task := Slice (Task_State, 1, Task_Max_Length - 1) & (1 * "*");
      elsif  Length (Task_State) < Task_Max_Length then
         Line_Task :=  Task_State &
                      (Task_Max_Length - Length (Task_State)) * " ";
      else
         Line_Task :=  Task_State;
      end if;

      --  Header console and disk mode
      if Header_On then

         --  Line Length control
         if (Header_Length + Length (Line) + 1) > Line_Max_Length then
            --                                      these two numbers v
            Line := Slice (Line, 1, Line_Max_Length - Header_Length - 1) & (1 * "*");
            --                                               must be equals ^                                                        
         end if;

         if Title_On then
            if (Header_Length + Length (Line) + 1) < Title_Max_Length then
               Line := Line &
                      (Title_Max_Length - Header_Length - Length (Line)) * "-";
            end if;
         end if;
         
         if Display_On then
            --  Console write with limited length line and ansi fancy
            Tio.Put_Line (To_String (Prg.Time_Stamp & " - " &
                            Line_Task & " - " &
                            Ansi_Begin & Line_Level & Ansi_End & " - " &
                            Line));
         end if;
         
      --  Free console mode
      else
         if Display_On then
            Tio.Put_Line (Ansi_Begin & To_String (Line) & Ansi_End);
         end if;
      end if;
      
      --  Disk write with unlimited length line
      if Disk_On then
         if Title_On then
            if (Header_Length + Length (Line_Disk) + 1) < Title_Max_Length then
               Line_Disk := Line_Disk &
                (Title_Max_Length - Header_Length - Length (Line_Disk)) * "-";
            end if;
         end if;
         Tio.Put_Line (Handle, To_String (Prg.Time_Stamp & " - " &
                                          Line_Task & " - " &
                                          Line_Level & " - " &
                                          Line_Disk));
      end if;

   end Put;

   --  Public Functions
   
   ----------------------------------------------------------------------------
   procedure Dbg (Message : String) is
   begin
      if Debug_On then
         Put (Message, "DBG");
      end if;
   end Dbg;

   procedure Dbg (Message : VString) is
   begin
      if Debug_On then
         Put (To_String (Message), "DBG");
      end if;
   end Dbg;
   
   ----------------------------------------------------------------------------
   procedure Err (Message : String) is
   begin
      Put (Message, "ERR");
   end Err;

   procedure Err (Message : VString) is
   begin
      Put (To_String (Message), "ERR");
   end Err;

   ----------------------------------------------------------------------------
   function Get_Debug return Boolean is
   begin
      return Debug_On;
   end Get_Debug;
   
   ----------------------------------------------------------------------------
   procedure Line is
   begin
      Tio.Line;
      if Disk_On then
         Tio.Line (Handle);
      end if;
   end Line;

   ----------------------------------------------------------------------------
   procedure Msg (Message : Boolean) is
   begin
      if Message then
         Put ("True", "MSG");
      else
         Put ("False", "MSG");
      end if;
   end Msg;
   
   procedure Msg (Message : String) is
   begin
      Put (Message, "MSG");
   end Msg;
   
   procedure Msg (Message : Character) is
   begin
      Put (To_String (To_VString (Message)), "MSG");
   end Msg;
   
   procedure Msg (Message : Integer) is
   begin
      Put (To_String (To_VString (Message)), "MSG");
   end Msg;

   procedure Msg (Message : Long_Integer) is
   begin
      Put (Long_Integer'Image (Message), "MSG");
   end Msg;

   procedure Msg (Message : VString) is
   begin
      Put (To_String (Message), "MSG");
   end Msg;

   ----------------------------------------------------------------------------
   procedure Set_Debug (Action : Boolean)  is
   begin
      Debug_On := Action;
   end Set_Debug;
   
   ----------------------------------------------------------------------------
   procedure Set_Display (Action : Boolean)  is
   begin
      Display_On := Action;
   end Set_Display;
   
   ----------------------------------------------------------------------------
   procedure Set_Header (Action : Boolean) is
   begin
      Header_On := Action;
   end Set_Header;

   ----------------------------------------------------------------------------
   procedure Set_Dir (Dir_In : String) is
   begin
      Log_Dir_Store := To_VString (Dir_In);
   end Set_Dir;

   procedure Set_Dir (Dir_In : VString) is
   begin
      Log_Dir_Store := Dir_In;
   end Set_Dir;

   function Get_Dir return VString is
   begin
      return Log_Dir_Store;
   end Get_Dir;
   
   ----------------------------------------------------------------------------
   procedure Set_Task (New_Task : String) is
   begin
      Task_State := To_VString (New_Task);
   end Set_Task;

   procedure Set_Task (New_Task : VString) is
   begin
      Task_State := New_Task;
   end Set_Task;

   ----------------------------------------------------------------------------
   procedure Set_Disk (Action : Boolean) is
      Log_File_Name : constant VString := Log_Dir_Store & Prg.Name & ".log";
      Log_Dir_Name : constant VString := Fls.Extract_Directory (Log_File_Name);
   begin
      Disk_On := Action;
      if Disk_On then
         if Fls.Exists (Log_File_Name) then
            Tio.Append (Handle, Log_File_Name);
         else
            -- Ensure that the complete tree structure exists before creating file
            if Fls.Create_Directory_Tree (Log_Dir_Name) then
               Tio.Create (Handle, Log_File_Name);
            else
               Disk_On := False;
               Err ("Log.Set_Disk > Can't create directory: " & Log_File_Name);
            end if;
         end if;
         if not Tio.Is_Open (Handle) then
            Disk_On := False;
            Err ("Log.Set_Disk > can't log on disk to: " & Log_File_Name);
         end if;
      else
         if Tio.Is_Open (Handle) then
            Tio.Close (Handle);
         end if;
      end if;
   end Set_Disk;
   
   ----------------------------------------------------------------------------
   procedure Title (Message : String) is
   begin
      Put (Ada.Characters.Handling.To_Upper (Message), "MSG", True);
   end Title;

   procedure Title (Message : VString) is
   begin
      Put (To_String (To_Upper (Message)), "MSG", True);
   end Title;
   
------------------------------------------------------------------------------
end v20.Log;
------------------------------------------------------------------------------
