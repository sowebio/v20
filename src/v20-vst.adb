------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20-vst.adb
--  @copyright See authors list below and v20.copyrights file
--  @licence   EUPL 1.2
--  @encoding  UTF-8
------------------------------------------------------------------------------
--  @summary
--  V20 library VStrings from HAC runtime by Gautier de Montmollin
--
--  @description
--
--  @authors
--  Gautier de Montmollin - gdm - https://github.com/zertovitch (author)
--  Stéphane Rivière - sr - sriviere@soweb.io (integration)
--
--  @versions
--  see v20.ads
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;

with v20.Log;
with v20.Tio;

package body v20.Vst is

   use Interfaces;

   package AS renames Ada.Strings;
   package ASF renames Ada.Strings.Fixed;

   --  Service functions ------------------------------------------------------

   function HAC_Image (I : Integer) return String is
      Im : constant String := Integer'Image (I);
   begin
      if I < 0 then
         return Im;
      else
         return Im (Im'First + 1 .. Im'Last);
      end if;
   end HAC_Image;

   function HAC_Generic_Image (I : Abstract_Integer) return String is
      Im : constant String := Abstract_Integer'Image (I);
   begin
      if I < 0 then
         return Im;
      else
         return Im (Im'First + 1 .. Im'Last);  --  Remove the leading ' '.
      end if;
   end HAC_Generic_Image;

   --  Types conversion -------------------------------------------------------

   function To_VString (B : Boolean) return VString is
   begin
      if B then
         return +"True";
      else
         return +"False";
      end if;
   end To_VString;

   function To_VString (I : Integer) return VString is
      function HAC_Image_for_Integer is
      new v20.Vst.HAC_Generic_Image (Abstract_Integer => Integer);
   begin
      return +HAC_Image_for_Integer (I);
   end To_VString;

   function To_VString (C : Character) return VString is
   begin
      return To_VString ((1 => C));
   end To_VString;

   function To_Integer (V : VString) return Integer is
   begin
      if Length (V) > 0 then
         return Integer'Value (ASU.To_String (V));
      else
         return 0;
      end if;
   end To_Integer;

   function To_Integer (V : String) return Integer is
   begin
      return To_Integer (To_VString (V));
      -- return Integer'Value (V);
   end To_Integer;

   function To_String (V : VString) return String is
   begin
      return ASU.To_String (V);
   end To_String;

   function To_Hex (Byte : Unsigned8) return String is
      Hex_Chars : constant array (Unsigned8 range 0 .. 15)
        of Character := ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
      Half_Byte_1 : constant Unsigned8 := Byte mod 16;
      Half_Byte_2 : constant Unsigned8 := Byte / 16;
   begin
      return Hex_Chars (Half_Byte_2) & Hex_Chars (Half_Byte_1);
   end To_Hex;

   function To_Hex (String_To_Convert : VString) return VString is
      String_Extracted : String := " ";
      String_Converted : VString := +"";
   begin
      for I in 1 .. Length (String_To_Convert) loop
         String_Extracted := ASU.Slice (Source => String_To_Convert, Low => I, High => I);
         ASU.Append (String_Converted, To_Hex ( Character'Pos (String_Extracted(1))) & " ");
      end loop;
      return String_Converted;
   end To_Hex;

   function To_Val (S_In : VString) return VString is
      S_Out : VString := +"";
   begin
      if Length (S_In) > 0 then
         for I in 1 .. Length (S_In) loop
            S_Out := S_Out & To_VString ( Character'Pos ((Element (S_In, I))) ) & " ";
         end loop;
      end if;
      return S_Out;
   end To_Val;


   --  Types tests ------------------------------------------------------------

   --  https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Ada
   function Is_Numeric (Item : in String) return Boolean is
      Dummy : Float;
   begin
      Dummy := Float'Value (Item);
      return True;
   exception
      when others =>
         return False;
   end Is_Numeric;

   function Is_Numeric (Item : in VString) return Boolean is
   begin
      return Is_Numeric (To_String (Item));
   end Is_Numeric;

   --  Basics -----------------------------------------------------------------

   function Empty (Source : String) return Boolean is
   begin
      return (Length (To_VString (Source)) = 0);
   end Empty;

   function Empty (Source : VString) return Boolean is
   begin
      return (Length (Source) = 0);
   end Empty;

   function Length (Source : String) return Natural is
   begin
      return Length (To_VString (Source));
   end Length;

   function Trim_Left  (Source : VString) return VString is
   begin
      return ASU.Trim (Source, AS.Left);
   end Trim_Left;

   function Trim_Right (Source : VString) return VString is
   begin
      return ASU.Trim (Source, AS.Right);
   end Trim_Right;

   function Trim_Both  (Source : VString) return VString is
   begin
      return ASU.Trim (Source, AS.Both);
   end Trim_Both;

   function Trim_Slashes (Source : VString) return VString is
      Src : VString := Source;
   begin
      --  Specific case
      if Src = "/" then
         Src := +"";
      end if;
      --  Suppress leading slash(es)
      while Index (Src, "/") = 1 and Length (Src) > 1 loop
         Src := Slice (Src, 2, Length (Src));
      end loop;
      --  Suppress trailing slash(es)
      while Index_Backward (Src, "/") = Length (Src) and Length (Src) > 1 loop
         Src := Slice (Src, 1, Length (Src) - 1);
      end loop;
      return Src;
   end Trim_Slashes;

   function To_Lower (Item : String) return VString is
   begin
      return To_VString (ACH.To_Lower (Item));
   end To_Lower;

   function To_Lower (Item : VString) return VString is
   begin
      return +ACH.To_Lower (ASU.To_String (Item));
   end To_Lower;

   function To_Upper (Item : String) return VString is
   begin
      return To_VString (ACH.To_Upper (Item));
   end To_Upper;

   function To_Upper (Item : VString) return VString is
   begin
      return +ACH.To_Upper (ASU.To_String (Item));
   end To_Upper;

   --  Detecting if pattern exist

   function Starts_With (Item : VString; Pattern : Character) return Boolean is
   begin
      return 1 <= Length (Item) and then Element (Item, 1) = Pattern;
   end Starts_With;

   function Starts_With (Item : VString; Pattern : String) return Boolean is
   begin
      return Pattern'Length <= Length (Item)
      and then ASU.To_String (ASU.Head (Item, Pattern'Length)) = Pattern;
   end Starts_With;

   function Starts_With (Item : VString; Pattern : VString) return Boolean is
   begin
      return Length (Pattern) <= Length (Item)
      and then ASU.Head (Item, Length (Pattern)) = Pattern;
   end Starts_With;

   function Ends_With (Item : VString; Pattern : Character) return Boolean is
   begin
      return 1 <= Length (Item) and
      then Element (Item, Length (Item)) = Pattern;
   end Ends_With;

   function Ends_With (Item : VString; Pattern : String) return Boolean is
   begin
      return Pattern'Length <= Length (Item)
      and then ASU.To_String (ASU.Tail (Item, Pattern'Length)) = Pattern;
   end Ends_With;

   function Ends_With (Item : VString; Pattern : VString) return Boolean is
   begin
      return Length (Pattern) <= Length (Item)
      and then ASU.Tail (Item, Length (Pattern)) = Pattern;
   end Ends_With;

   -- Counting

   function Char_Count (String_To_Process : VString ; Char_Set_Pattern : VString) return Integer is
      Result_Count : Integer := 0;
      String_To_Process_Length : constant Natural := Length (String_To_Process);
      Char_Set_Pattern_Length : constant Natural := Length (Char_Set_Pattern);
   begin
      if (String_To_Process_Length > 0) and (Char_Set_Pattern_Length > 0) then
         for I in 1 .. String_To_Process_Length loop
            for J in 1 .. Char_Set_Pattern_Length loop
               if Slice (String_To_Process, I, I) = Slice (Char_Set_Pattern, J, J) then
                  Result_Count := Result_Count + 1;
                  exit;
               end if;
            end loop;
         end loop;
      end if;
      return Result_Count;
   end Char_Count;

   function Char_Count (String_To_Process : VString ; Char_Set_Pattern : String) return Integer is
   begin
      return Char_Count (String_To_Process, To_VString (Char_Set_Pattern));
   end Char_Count;

   --  Returning a pattern position -------------------------------------------

   function Index (Source : VString; Pattern : Character) return Natural is
   begin
      return ASU.Index (Source, (1 => Pattern));
   end Index;

   function Index (Source : VString; Pattern : String) return Natural is
   begin
      return ASU.Index (Source, Pattern);
   end Index;

   function Index (Source : VString; Pattern : VString) return Natural is
   begin
      return ASU.Index (Source, ASU.To_String (Pattern));
   end Index;

   function Index (Source : VString; Pattern : Character; From : Positive)
                   return Natural is
   begin
      return ASU.Index (Source, (1 => Pattern), From);
   end Index;

   function Index (Source : VString; Pattern : String; From : Positive)
                   return Natural is
   begin
      return ASU.Index (Source, Pattern, From);
   end Index;

   function Index (Source : VString; Pattern : VString; From : Positive)
                   return Natural is
   begin
      return ASU.Index (Source, ASU.To_String (Pattern), From);
   end Index;

   function Index_Backward (Source : VString; Pattern : Character)
                            return Natural is
   begin
      return ASU.Index (Source, (1 => Pattern), Ada.Strings.Backward);
   end Index_Backward;

   function Index_Backward (Source : VString; Pattern : String)
                           return Natural is
   begin
      return ASU.Index (Source, Pattern, Ada.Strings.Backward);
   end Index_Backward;

   function Index_Backward (Source : VString; Pattern : VString)
                           return Natural is
   begin
      return ASU.Index (Source, ASU.To_String (Pattern), Ada.Strings.Backward);
   end Index_Backward;

   function Index_Backward (Source : String; Pattern : String)
                            return Natural is
   begin
      return ASU.Index (To_VString (Source), Pattern, Ada.Strings.Backward);
   end Index_Backward;

   function Index_Backward (Source : VString;
                            Pattern : Character;
                            From : Positive)
                            return Natural is
   begin
      return ASU.Index (Source, (1 => Pattern), From, Ada.Strings.Backward);
   end Index_Backward;

   function Index_Backward (Source : VString;
                            Pattern : String;
                            From : Positive)
                            return Natural is
   begin
      return ASU.Index (Source, Pattern, From, Ada.Strings.Backward);
   end Index_Backward;

   function Index_Backward (Source : VString;
                            Pattern : VString;
                            From : Positive)
                            return Natural is
   begin
      return ASU.Index (Source,
                         ASU.To_String (Pattern), From, Ada.Strings.Backward);
   end Index_Backward;

   --  Extracting substring form position -------------------------------------

   function Head (Source : VString; Count : Natural) return VString is
   begin
      return ASU.Head (Source, Count);  --  We use the default padding: ' '.
   end Head;

   function Tail (Source : VString; Count : Natural) return VString is
   begin
      return ASU.Tail (Source, Count);  --  We use the default padding: ' '.
   end Tail;

   function Slice (Source : String; Low : Positive; High : Natural := 0)
                  return VString is
      Loc_High : constant Natural := (if High = 0 then Length (Source) else High);
   begin
      return +ASU.Slice (To_VString (Source), Low, Loc_High);
   end Slice;

   function Slice (Source : VString; Low : Positive; High : Natural := 0)
                  return VString is
   begin
      return +ASU.Slice (Source, Low, High);
   end Slice;

   --  Extracting substring from pattern --------------------------------------

   function Tail_After_Match (Source : VString; Pattern : VString)
                              return VString is
      Result : VString := +"";
      Source_Length : constant Natural := Length (Source);
      Pattern_Length : constant Natural := Length (Pattern);
   begin
      if (Source_Length > 0) and (Pattern_Length > 0) then
         for I in reverse 1 .. Source_Length loop
            if I <= Source_Length - Pattern_Length then
               if Slice (Source, I, I + Pattern_Length - 1) = Pattern then
                  Result := Slice (Source, I + Pattern_Length, Source_Length);
                  exit;
               end if;
            end if;
         end loop;
      end if;
      return Result;
   end Tail_After_Match;

   function Tail_After_Match (Source : String; Pattern : String)
                              return VString is
   begin
      return Tail_After_Match (To_VString (Source), +Pattern);
   end Tail_After_Match;

   function Tail_After_Match (Source : VString; Pattern : Character)
                              return VString is
   begin
      return Tail_After_Match (Source, (1 => Pattern));
   end Tail_After_Match;

   function Tail_After_Match (Source : VString; Pattern : String)
                              return VString is
   begin
      return Tail_After_Match (Source, +Pattern);
   end Tail_After_Match;

   -- Stript and replace -----------------------------------------------------

   function Stript_Accents (String_To_Process : VString ) return VString is
      Result_String : VString := +"";
      Result_Char : Character;
      String_To_Process_Length : constant Natural := Length (String_To_Process);
      UTF8_To_Process  : Boolean := False;
      UTF8_ESC          : constant Character := Character'Val(195);
      UTF8_A_Acute      : constant Character := Character'Val(160); -- à
      UTF8_A_Umlaut     : constant Character := Character'Val(162); -- ä
      UTF8_C_Cedilla    : constant Character := Character'Val(167); -- ç
      UTF8_E_Grave      : constant Character := Character'Val(168); -- è
      UTF8_E_Acute      : constant Character := Character'Val(169); -- é
      UTF8_E_Circumflex : constant Character := Character'Val(170); -- ê
      UTF8_E_Umlaut     : constant Character := Character'Val(171); -- ë
      UTF8_I_Circumflex : constant Character := Character'Val(174); -- î
      UTF8_I_Umlaut     : constant Character := Character'Val(175); -- ï
      UTF8_O_Circumflex : constant Character := Character'Val(180); -- î
      UTF8_U_Grave      : constant Character := Character'Val(185); -- ï
   begin
      for I in 1 .. String_To_Process_Length loop
         Result_Char := Element (String_To_Process, I);
         if UTF8_To_Process then
            -- UTF-8 to ASCII
            if    Result_Char = UTF8_A_Acute      then
               Result_Char:= 'a';
            elsif Result_Char = UTF8_A_Umlaut     then
               Result_Char:= 'a';
            elsif Result_Char = UTF8_E_Grave      then
               Result_Char:= 'e';
            elsif Result_Char = UTF8_E_Acute      then
               Result_Char:= 'e';
            elsif Result_Char = UTF8_E_Circumflex then
               Result_Char:= 'e';
            elsif Result_Char = UTF8_E_Umlaut     then
               Result_Char:= 'e';
            elsif Result_Char = UTF8_I_Circumflex then
               Result_Char:= 'i';
            elsif Result_Char = UTF8_I_Umlaut     then
               Result_Char:= 'i';
            elsif Result_Char = UTF8_O_Circumflex then
               Result_Char:= 'o';
            elsif Result_Char = UTF8_U_Grave      then
               Result_Char:= 'u';
            elsif Result_Char = UTF8_C_Cedilla    then
               Result_Char:= 'c';
            else
               Result_Char:= '?';
            end if;
            UTF8_To_Process := False;
            Result_String := Result_String & To_VString (Result_Char);
         else
            if Result_Char = UTF8_ESC then
               UTF8_To_Process := True;
            else
               -- Latin_1 to ASCII
               if    Result_Char = 'à' then
                  Result_Char:= 'a';
               elsif Result_Char = 'â' then
                  Result_Char:= 'a';
               elsif Result_Char = 'é' then
                  Result_Char:= 'e';
               elsif Result_Char = 'è' then
                  Result_Char:= 'e';
               elsif Result_Char = 'ê' then
                  Result_Char:= 'e';
               elsif Result_Char = 'ë' then
                  Result_Char:= 'e';
               elsif Result_Char = 'î' then
                  Result_Char:= 'i';
               elsif Result_Char = 'ï' then
                  Result_Char:= 'i';
               elsif Result_Char = 'ô' then
                  Result_Char:= 'o';
               elsif Result_Char = 'ù' then
                  Result_Char:= 'u';
               elsif Result_Char = 'ç' then
                  Result_Char:= 'c';
               elsif Character'Pos (Result_Char) > 127 then
                  Result_Char:= '?';
               end if;
               Result_String := Result_String & To_VString (Result_Char);
            end if;
         end if;
      end loop;
      return Result_String;
   end Stript_Accents;

   function Stript_Chars (String_To_Process : VString ; Char_List : VString) return VString is
      Result_String, Result_Char : VString := +"";
      String_To_Process_Length : constant Natural := Length (String_To_Process);
      Char_Set_Pattern_Length : constant Natural := Length (Char_List);
   begin
      for I in 1 .. String_To_Process_Length loop
         Result_Char := Slice (String_To_Process, I, I);
         for J in 1 .. Char_Set_Pattern_Length loop
            if Result_Char = Slice (Char_List, J, J) then
               Result_Char := + "";
               exit;
            end if;
         end loop;
         Result_String := Result_String & Result_Char;
      end loop;
      return Result_String;
   end Stript_Chars;

   ---------------------------------------------------------------------------
   function Replace_Char (String_To_Process : VString ; Char_In : Character ; Char_Out : Character) return VString is
      Result_String : VString := +"";
      Result_Char : Character;
      String_To_Process_Length : constant Natural := Length (String_To_Process);
   begin
      for I in 1 .. String_To_Process_Length loop
         Result_Char := Element (String_To_Process, I);
         if (Result_Char = Char_In) then
            Result_Char := Char_Out;
         end if;
         Result_String := Result_String & To_VString (Result_Char);
      end loop;
      return Result_String;
   end Replace_Char;

   ---------------------------------------------------------------------------
   function Replace_Pattern (String_To_Process : VString ;
              Pattern_In : VString ; Pattern_Out : VString) return VString is

      Result_String : VString := +"";
      Result_Char : VString;

      Pattern_In_Length : constant Natural := Length (Pattern_In);
      Pattern_Buffer : VString := +"";

      String_To_Process_Length : constant Natural := Length (String_To_Process);

   begin

      for I in 1 .. String_To_Process_Length loop
         Result_Char := Slice (String_To_Process, I, I);
         Pattern_Buffer := Pattern_Buffer & Result_Char;

         -- Sliding window
         if (Length (Pattern_Buffer) > Pattern_In_Length) then
            Pattern_Buffer := Slice (Pattern_Buffer, 2, Pattern_In_Length + 1);
         end if;

         if (Pattern_Buffer = Pattern_In) then
            Result_String := Slice (Result_String, 1, Length (Result_String) - Pattern_In_Length + 1) & Pattern_Out;
            Pattern_Buffer := +"";
         else
            Result_String := Result_String & Result_Char;
         end if;

      end loop;

      return Result_String;

   end Replace_Pattern;

   --  Fields processing ------------------------------------------------------

   function Field_By_Index (String_Input : VString ; Index_Field : Integer ; Field_Delimiter : String) return VString is
      Index_Count, Index_Next : Integer := 1;
      Result_String : VString := +"";
      String_To_Process : VString := String_Input & Field_Delimiter;
   begin
      --  Slow algo - gnx db info = 12s
      --  Index_Count : Integer := 1;
      --  Result_String, Result_Char: VString := +"";
      --  String_To_Process : constant VString := String_Input & Field_Delimiter;
      --  String_To_Process_Length : constant Natural := Length (String_To_Process);
      -- begin
      --  for I in 1 .. String_To_Process_Length loop
      --     Result_Char := Slice (String_To_Process, I, I);
      --     if Result_Char = Field_Delimiter then
      --        if Index_Count = Index_Field then
      --           exit;
      --        else
      --           Index_Count := Index_Count + 1;
      --           Result_String := +"";
      --        end if;
      --     else
      --        Result_String := Result_String & Result_Char;
      --     end if;
      --  end loop;
      --  return Trim_Both (Result_String);

      --  Fast algo (100 x fastest) - gnx db info = 0,12s
      while True loop
         Index_Next := Index (String_To_Process, Field_Delimiter);
         if Index_Count = Index_Field then
            Result_String := Slice (String_To_Process, 1, Index_Next - 1);
            exit;
         else
            Index_Count := Index_Count + 1;
            String_To_Process := Slice (String_To_Process, Index_Next + 1, Length (String_To_Process));
            if Length (String_To_Process) <= 1 then -- Last field reached
               Result_String := +"";
               exit;
            end if;
         end if;
      end loop;

      return Result_String;

   end Field_By_Index;

   ---------------------------------------------------------------------------
   function Field_By_Name (String_Input : VString ; Field_To_Search : VString ; Field_Delimiter : String) return VString is
      Index_Count : Integer := 1;
      Result_String, Result_Char: VString := +"";
      String_To_Process : constant VString := String_Input & Field_Delimiter;
      String_To_Process_Length : constant Natural := Length (String_To_Process);
   begin
      if ((not Empty (String_Input)) and
          (not Empty (Field_To_Search)) and
          (not Empty (Field_Delimiter))
         ) then
         for I in 1 .. String_To_Process_Length loop
            Result_Char := Slice (String_To_Process, I, I);
            if Result_Char = Field_Delimiter then
               if (Index (Result_String, Field_To_Search) > 0) then
                  exit;
               else
                  Index_Count := Index_Count + 1;
                  Result_String := +"";
               end if;
            else
               Result_String := Result_String & Result_Char;
            end if;
         end loop;
      end if;
      return Trim_Both (Result_String);
   end Field_By_Name ;

   ----------------------------------------------------------------------------
   function Field_Search (String_To_Process : VString ; Field_To_Search : VString ; Field_Delimiter : String) return Boolean is
      Result : Boolean := False;
   begin
      -- "a" should not be found in "span" so search "a & delimiter"
      if (Index (String_To_Process & Field_Delimiter, Field_To_Search & Field_Delimiter) > 0) then
         Result := True;
      end if;
      return Result;
   end Field_Search;

   ---------------------------------------------------------------------------
   function Field_Count (String_To_Process : VString ; Field_Delimiter : String) return Integer is
   begin
      if Empty (String_To_Process) then
         return 0;
      else
         return Char_Count (String_To_Process, Field_Delimiter) + 1;
      end if;
   end Field_Count;

   ----------------------------------------------------------------------------
   procedure Field_Display (String_To_Process : VString; Column_Delimiter: String ; Row_Delimiter: String; Custom_Header : String := "") is
      Columns : constant Natural := Field_Count (Field_By_Index (String_To_Process, 1, Row_Delimiter), Column_Delimiter);
      Rows : constant Natural := Field_Count (String_To_Process, Row_Delimiter);
      Header : constant VString := To_VString (Custom_Header);
      Max_Width : Natural := 0;
      Current_Column : VString := +"";

      type T_Display is array (Integer range <>, Integer range <>) of VString;
      Display : T_Display (0..Rows+1, 0..Columns+1);

   begin

      --Log.Dbg ("Columns " & To_VString (Columns));
      --Log.Dbg ("Rows " & To_VString (Rows));

      -- Format
      if Columns > 0 then
         for Index_Columns in 1..Columns loop

            -- Get max header width
            if Length (Header) > 0 then
               Max_Width := Length (Field_By_Index (Header, Index_Columns, ","));
            end if;

            -- Get max column width
            for Index_Rows in 1..Rows loop
               Current_Column := Field_By_Index (Field_By_Index (String_To_Process, Index_Rows, Row_Delimiter), Index_Columns, Column_Delimiter);
               if Length (Current_Column) > Max_Width then
                  Max_Width := Length (Current_Column);
               end if;
            end loop;

            --Log.Dbg ("Max_Width for column " & Current_Column & ": " & To_VString (Max_Width));

            -- Format header
            if Length (Header) > 0 then
               Current_Column := Field_By_Index (Header, Index_Columns, ",");
               Display (0, Index_Columns) := Current_Column & (Max_Width - Length (Current_Column)) * " " ;
            end if;

            -- Format column
            for Index_Rows in 1..Rows loop
               Current_Column := Field_By_Index (Field_By_Index (String_To_Process, Index_Rows, Row_Delimiter), Index_Columns, Column_Delimiter);
               Display (Index_Rows, Index_Columns) := Current_Column & (Max_Width - Length (Current_Column)) * " " ;
            end loop;

         end loop;

         -- Display header
         if Length (Header) > 0 then
            Max_Width := 0;
            for Index_Columns in 1..Columns loop
               Current_Column := Display (0, Index_Columns) & "  ";
               Max_Width := Max_Width + Length (Current_Column);
               Tio.Put (Current_Column);
            end loop;
            Tio.Line;
            Tio.Put_Line ( (Max_Width - 2) * "-");
         end if;

         -- Display rows
         for Index_Rows in 1..Rows loop
            for Index_Columns in 1..Columns loop
               Tio.Put (Display (Index_Rows, Index_Columns) & "  ");
            end loop;
            Tio.Line;
         end loop;
      end if;

   end Field_Display;

   --  Operators --------------------------------------------------------------

   function "&" (I : Integer; V : VString) return VString is
   begin
      return HAC_Image (I) & V;
   end "&";

   function "&" (V : VString; I : Integer) return VString is
   begin
      return V & HAC_Image (I);
   end "&";

   function "*" (Num : Natural; Pattern : String) return VString is
   begin
      return +ASF."*" (Num, Pattern);
   end "*";

-------------------------------------------------------------------------------
end v20.Vst;
-------------------------------------------------------------------------------
