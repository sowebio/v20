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

with Ada.Calendar.Formatting;
--with Ada.Calendar.Time_Zones;
with Ada.Strings.Fixed;

with v20.Log;
with v20.Sys;

package body v20.Prg is

   package ACF renames Ada.Calendar.Formatting;

   --  Program related --------------------------------------------------------

   function Current_Time_Seconds return Natural is
      Time_Base :    constant Ada.Calendar.Time := ACF.Time_Of (1970, 1, 1, 0.0);
      Time_Current : constant Ada.Calendar.Time := AC.Clock;
      --type Time_Seconds is range 0 .. 100*365*24*60*60; -- 100 years of seconds...
   begin
      -- Tio.Put_Line (Time_Seconds'Image (Time_Seconds (Time_Current - Time_Base)));
      --return Integer (Time_Seconds (Time_Current - Time_Base));
      return Integer (Time_Current - Time_Base);
   end Current_Time_Seconds;

   function Duration_Stamp (Time : Ada.Calendar.Time) return VString is
      Current_Time : constant Integer := Integer (AC.Seconds (AC.Clock));
      Day_Secs : constant Integer := Current_Time - Integer (AC.Seconds (Time));
   begin
      --  Unlimited hours counter for long uptimes
      return To_VString ((Day_Secs / 3600)) & "h" &
            Time_Format ((Day_Secs / 60) mod 60) & "m" &
             Time_Format (Day_Secs mod 60) & "s";
   end Duration_Stamp;

   function Duration_Stamp_Seconds (Time : Ada.Calendar.Time) return Natural is
      Current_Time : constant Natural := Integer (AC.Seconds (AC.Clock));
      Day_Secs : constant Natural := Current_Time - Integer (AC.Seconds (Time));
   begin
      --  Seconds
      return Day_Secs;
   end Duration_Stamp_Seconds;

   function Duration_Stamp_Time (Time_Seconds : Integer) return VString is
   begin
      --  Unlimited hours counter for long uptimes
      return To_VString ((Time_Seconds / 3600)) & "h" &
            Time_Format ((Time_Seconds / 60) mod 60) & "m" &
             Time_Format (Time_Seconds mod 60) & "s";
   end Duration_Stamp_Time;

   function Generate_Password return VString is
      --  Sowebio password generation with 64 chars:
      --    ([A-Z] + [a-z] + [0-9] + '_' + '-')
      --  Search space size:
      --    > 1,26 x 10^25
      --  Space exploration time:
      --    40000 centuries @ 100 billion tests per second.
      --  Command line with standard tools:
      --    < /dev/urandom tr -dc _A-Z-a-z-0-9 | head -c${1:-14};echo;
      --  Generates 14 chars long passwords like:
      --    x2U5hhIKX7IJt6
      SE_Result : Integer := 0;
      SE_Output : VString := +"";
   begin
      Sys.Shell_Execute ("< /dev/urandom tr -dc _A-Z-a-z-0-9 | head -c${1:-14};echo;", SE_Result, SE_Output);
      if (SE_Result /= 0) then
         Log.Err ("v20.Prg.Generate_Password - Password generation command failed");
      end if;
      return SE_Output;
   end Generate_Password;

   function Get_Version return VString is
   begin
      return (Name & " v" &
              Ada.Strings.Fixed.Trim (Natural'Image (Version_Major),
              Ada.Strings.Left) & "." &
              Ada.Strings.Fixed.Trim (Natural'Image (Version_Minor),
          Ada.Strings.Left));
   end Get_Version;

   function Get_Version_Major return Natural is
   begin
      return Version_Major;
   end Get_Version_Major;

   function Get_Version_Minor return Natural is
   begin
      return Version_Minor;
   end Get_Version_Minor;

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

      --Current_Time : constant AC.Time := AC.Clock;
      Current_Time : constant AC.Time := AC.Clock;
      Day_Secs : constant Integer := Integer (AC.Seconds (Current_Time));
   begin
      return To_VString (AC.Year (Current_Time)) &
      Time_Format (AC.Month (Current_Time)) &
      Time_Format (AC.Day (Current_Time)) &
      " " &
      Time_Format (Day_Secs / 3660) &
      Time_Format ((Day_Secs / 60) mod 60) &
      Time_Format (Day_Secs mod 60);
   end Time_Stamp;

-------------------------------------------------------------------------------
end v20.Prg;
-------------------------------------------------------------------------------
