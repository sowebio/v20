-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20-Prg.adb
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V20 library Log package
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  see .ads
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;

with v20.Sys;

package body v20.Prg is

   --  Program related --------------------------------------------------------

   function Duration_Stamp (Start_Time : Ada.Calendar.Time) return VString is
      Current_Time : constant Integer :=
                         Integer (Ada.Calendar.Seconds (Ada.Calendar.Clock));
      Day_Secs : constant Integer := Current_Time -
                         Integer (Ada.Calendar.Seconds (Start_Time));
   begin
      --  Unlimited hours counter for long uptimes
      return To_VString ((Day_Secs / 3600)) & "h" &
           Time_Format ((Day_Secs / 60) mod 60) & "m" &
           Time_Format (Day_Secs mod 60) & "s";
   end Duration_Stamp;

   function Get_Version return VString is
   begin
      return (Name & " v" &
              Ada.Strings.Fixed.Trim (Natural'Image (Version_Major),
              Ada.Strings.Left) & "." &
              Ada.Strings.Fixed.Trim (Natural'Image (Version_Minor),
          Ada.Strings.Left));
   end Get_Version;

   function Is_User_Not_Root return Boolean is
   begin
      return (not (Sys.Get_Env ("USER") = +"root"));
   end Is_User_Not_Root;

   function Name return VString is
      Result : VString := Tail_After_Match (Command, "/");
   begin
      if (Length (Result) = 0) then
         Result := Command;
      end if;
     return Result;
   end Name;

   function Path return String is
      Result_Code : Natural := 0;
      Result_String : VString := +"";
   begin
      Sys.Shell_Execute ("pwd", Result_Code, Result_String);
      -- pwd at root returns /
      -- pwn elsewhere returns a path without a trailing slash
      if (Slice (Result_String, Length (Result_String), Length (Result_String)) /= +"/") then
         Result_String := Result_String & "/";
      end if;
      return To_String (Result_String);
   end Path;

   procedure Set_Exit_Status (Code : Natural) is
   begin
      Prg_Exit_Status := Prg_Exit_Status + Code;
      Ada.Command_Line.Set_Exit_Status
                             (Ada.Command_Line.Exit_Status (Prg_Exit_Status));
   end Set_Exit_Status;

   procedure Set_Version (Major : Natural; Minor : Natural) is
   begin
      Version_Major := Major;
      Version_Minor := Minor;
   end Set_Version;

   function Time_Format (Input_To_Format : Integer) return VString is
      Two_Digits_Output : VString;
   begin
      Two_Digits_Output := To_VString (Input_To_Format);
      if Input_To_Format < 10 then
         Two_Digits_Output := +"0" & Two_Digits_Output;
      end if;
      return Two_Digits_Output;
   end Time_Format;

   function Time_Stamp return VString is
      Current_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Day_Secs : constant Integer :=
                               Integer (Ada.Calendar.Seconds (Current_Time));
   begin
      return To_VString (Ada.Calendar.Year (Current_Time)) &
      Time_Format (Ada.Calendar.Month (Current_Time)) &
      Time_Format (Ada.Calendar.Day (Current_Time)) &
      " " &
      Time_Format (Day_Secs / 3660) &
      Time_Format ((Day_Secs / 60) mod 60) &
      Time_Format (Day_Secs mod 60);
   end Time_Stamp;

-------------------------------------------------------------------------------
end v20.Prg;
-------------------------------------------------------------------------------
