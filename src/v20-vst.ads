-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20-vst.ads
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V20 library VStrings from HAC runtime by Gautier de Montmollin
--
--  @description
--
--  @authors
--  Gautier de Montmollin - gdm - https://github.com/zertovitch (author)
--  Stéphane Rivière - sr - sriviere@soweb.io (integration and additions)
--
--  @versions
--  see v20.ads
-------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Interfaces;

package v20.Vst is

   package ACH renames Ada.Characters.Handling;
   package ASU renames Ada.Strings.Unbounded;

   subtype VString is ASU.Unbounded_String;
   subtype Unsigned8 is Interfaces.Unsigned_8;

   Null_VString : VString renames ASU.Null_Unbounded_String;

   --  Types conversion

   function To_VString (B : Boolean) return VString;
   function To_VString (I : Integer) return VString;
   function To_VString (C : Character) return VString;
   function To_VString (S : String) return VString renames ASU.To_Unbounded_String;

   --  Convert an Integer, a Char or a String type into VString type.

   function To_Integer (V : String) return Integer;
   function To_Integer (V : VString) return Integer;
   --  Convert a String or VString to an Integer.

   function To_String (V : VString) return String;
   --  Convert a VString to a String.

   function Ascii_Value_To_Hex (Input : VString) return VString;
   --  Convert an ASCII VString value ranging 0..127 to a VString hexadecimal
   --  output. For example, with input is +"61" dec, result is +"3D" hex.

   function To_Hex (Byte : Interfaces.Unsigned_8) return String;
   --  Convert a Byte to a String hexadecimal output.

   function To_Hex (String_To_Convert : VString) return VString;
   -- Convert a VString to a VString hexadecimal formatted output.

   function To_Val (S_In : VString) return VString;
   -- Convert a VString to VString ASCII decimal formatted output.

   --  Types tests

   function Is_Numeric (Item : in String) return Boolean;
   function Is_Numeric (Item : in VString) return Boolean;
   --  Return True if Item string is numeric

   --  Basics

   function Empty (Source : String) return Boolean;
   function Empty (Source : VString) return Boolean;
   --  Return True if VString is empty.

   function Length (Source : String) return Natural;
   function Length (Source : VString) return Natural renames ASU.Length;
   --  Returns the length of the String or VString represented by Source.

   function Element (Source : VString; Index : Positive) return Character
                                                          renames ASU.Element;
   --  Return the Character in Index position of the Vstring argument.
   --  Index starts at one.

   function Trim_Left (Source : VString) return VString;
   --  Returns a trimmed leading spaces VString of VString Source.

   function Trim_Right (Source : VString) return VString;
   --  Returns a trimmed trailing spaces VString of VString Source.

   function Trim_Both  (Source : VString) return VString;
   --  Returns an all trimmed spaces VString of VString Source.

   function Trim_Slashes (Source : VString) return VString;
   --  Returns an all trimmed slahes VString of VString Source.
   --
   --  Examples : Trim_Slashes (
   --    "/") returns ""
   --    "i") returns "i"
   --    "/i") returns "i"
   --    "//////i/////") returns "i"

   function To_Lower (Item : Character) return Character renames ACH.To_Lower;
   function To_Lower (Item : String) return VString;
   function To_Lower (Item : VString) return VString;
   --  Convert a Character or a VString to lower case.

   function To_Upper (Item : Character) return Character renames ACH.To_Upper;
   function To_Upper (Item : String) return VString;
   function To_Upper (Item : VString) return VString;
   --  Convert a Character or a VString to upper case.

   --  Detecting if pattern exist

   function Starts_With (Item : VString; Pattern : Character) return Boolean;
   function Starts_With (Item : VString; Pattern : String) return Boolean;
   function Starts_With (Item : VString; Pattern : VString) return Boolean;
   --  Check if Vstring Item starts with another VString or String Pattern.

   function Ends_With (Item : VString; Pattern : Character) return Boolean;
   function Ends_With (Item : VString; Pattern : String) return Boolean;
   function Ends_With (Item : VString; Pattern : VString) return Boolean;
   --  Check if VString Item ends with another VString or String Pattern.

   --  Counting

   function Char_Count (String_To_Process : VString ; Char_Set_Pattern : VString) return Integer;
   function Char_Count (String_To_Process : VString ; Char_Set_Pattern : String) return Integer;
   --  Count each char in String_To_Process relative to Char_Set_Pattern.

   --  Returning a pattern position

   function Index (Source : VString; Pattern : Character) return Natural;
   function Index (Source : VString; Pattern : String) return Natural;
   function Index (Source : VString; Pattern : VString) return Natural;
   --  Returns Natural start position of String or VString Pattern in the
   --  target Vstring Source. Natural starts at one. Natural is zero if not
   --  found.

   function Index (Source : VString;
                   Pattern : Character;
                   From : Positive) return Natural;
   function Index (Source : VString;
                   Pattern : String;
                   From : Positive) return Natural;
   function Index (Source : VString;
                   Pattern : VString;
                   From : Positive) return Natural;
   --  Returns Natural start position of String or VString Pattern in the
   --  target Vstring Source, From a starting index. Natural starts at one.
   --  Natural is zero if not found.

   function Index_Backward (Source : VString;
                            Pattern : Character) return Natural;
   function Index_Backward (Source : String;
                            Pattern : String) return Natural;
   function Index_Backward (Source : VString;
                            Pattern : String) return Natural;
   function Index_Backward (Source : VString;
                            Pattern : VString) return Natural;
   --  From the end of the target Vstring Source, returns Natural start
   --  position of String or VString Pattern in the target Vstring Source.
   --  Natural is zero if not found. Natural starts at one.

   function Index_Backward (Source : VString;
                            Pattern : Character;
                            From : Positive) return Natural;
   function Index_Backward (Source : VString;
                            Pattern : String;
                            From : Positive) return Natural;
   function Index_Backward (Source : VString;
                            Pattern : VString;
                            From : Positive) return Natural;
   --  From the end of the target Vstring Source, returns Natural start
   --  position of String or VString Pattern in the target Vstring Source,
   --  From a backward starting index. Natural is zero if not found. Natural
   --  starts at one.

   --  Extracting substring form position

   function Head (Source : VString; Count : Natural) return VString;
   --  Extract a VString between the beginning to Count Value to a VString.
   --  Count starts at one.

   function Tail (Source : VString; Count : Natural) return VString;
   --  Extract a VString from Source between its end to backward Count Value.
   --  Count starts at one (backward).

   function Slice (Source : String;
                   Low : Positive;
                   High : Natural := 0) return VString;
   function Slice (Source : VString;
                   Low : Positive;
                   High : Natural := 0) return VString;
   --  Returns a Vstring portion of the Vstring represented by Source
   --  delimited Low and High. Low and High start at one. Omitting High stands
   --  for High equal to length of source

   --  Extracting substring form pattern

   function Tail_After_Match (Source : VString;
                              Pattern : Character) return VString;
   function Tail_After_Match (Source : String;
                              Pattern : String) return VString;
   function Tail_After_Match (Source : VString;
                              Pattern : String) return VString;
   function Tail_After_Match (Source : VString;
                              Pattern : VString) return VString;
   --  Extract a VString from Source starting from Pattern+1 position to the
   --  end. Returns a VString
   --
   --  Examples : Tail_After_Match (+"/etc/genesix/gnx-startup",
   --    "/")) returns "gnx-startup"
   --    "ix")) returns "/gnx-startup"
   --    "gene")) returns "six/gnx-startup"
   --    "etc/genesix/gnx-startu")) returns "p"
   --    "/etc/genesix/gnx-startu")) returns "p"
   --    "/etc/genesix/gnx-startup")) returns empty string
   --    "/etc/genesix/gnx-startupp")) returns empty string

   --  Stript and replace -----------------------------------------------------

   function Stript_Accents (String_To_Process : VString ) return VString;
   --  Replace common accented characters with their lower ASCII equivalent.
   --  Encoding processed are Latin_1, UTF-8 and character handled are
   --  à â é è ê ë î ï ô ù ç À Â É È Ê Ë Î Ï Ô Ù Ç.

   function Stript_Chars (String_To_Process : VString ; Char_List : VString) return VString;
   --  Stript each char in String_To_Process relative to Char_List

   function Replace_Char (String_To_Process : VString ; Char_In : Character ; Char_Out : Character) return VString;
   --  Replace all Char_In by Char_Out in String_To_Process

   function Replace_Pattern (String_To_Process : VString ;
                Pattern_In : VString ; Pattern_Out : VString) return VString;
   -- Replace Pattern_In by Pattern_Out in String_To_Process. Returns a
   -- VString with Pattern_In replaced by Pattern_Out

   --  Fields processing ------------------------------------------------------

   --  /!\ Use only Field_Delimiter characters between 0dec and 127dec, due to
   --  /!\ some keyboard available characters encoding with 2 chars. Some
   --  /!\ recommended keyboards Field_Delimiter characters are listed in
   --  /!\ v20.ads but also in the v20 documentation

   function Field_By_Index (String_Input : VString ; Index_Field : Integer ; Field_Delimiter : String) return VString;
   --  Return a field indexed by Index_Field and delimited by Field_Delimiter
   --  from String_To_Process.

   function Field_By_Name (String_Input : VString ; Field_To_Search : VString ; Field_Delimiter : String) return VString;
   --  Return a field from a search string and delimited by Field_Delimiter.

   function Field_Count (String_To_Process : VString ; Field_Delimiter : String) return Integer;
   --  Count fields in String_To_Process and return fields number.

   procedure Field_Display (String_To_Process : VString; Column_Delimiter : String; Row_Delimiter : String; Custom_Header : String := "");
   --  Formatted display of a string fields structured in rows and columns

   function Field_Included (String_To_Process : VString ; Items_List  : VString ; Field_Delimiter : String) return Boolean;
   --  Returns True if all Items_List are included in String_To_Process list, which is delimited by Field_Delimiter.

   function Field_Search (String_To_Process : VString ; Field_To_Search : VString ; Field_Delimiter : String) return Boolean;
   --  Search Field_To_Search in String_To_Process and return True if found.

   --  Operators --------------------------------------------------------------

   function "+" (C : Character) return VString renames To_VString;
   function "+" (S : String) return VString renames To_VString;

   --   function "+"(input : in String) return Unbounded_String renames To_Unbounded_String;
   --function "+"(input : in Unbounded_String) return String renames To_String;

   function "+" (S : VString) return String renames To_String;

   function "*" (Num : Natural;
                 Pattern : Character) return VString renames ASU."*";
   function "*" (Num : Natural;
                 Pattern : String) return VString;
   function "*" (Num : Natural;
                 Pattern : VString) return VString renames ASU."*";
   --  TBD...
   --  function "*" (Pattern : Character; Num : Natural) return VString;
   --  function "*" (Pattern : String; Num : Natural) return VString;
   --  function "*" (Pattern : VString; Num : Natural) return VString;

   function "&" (V1, V2 : VString) return VString renames ASU."&";
   function "&" (V : VString; S : String) return VString renames ASU."&";
   function "&" (S : String; V : VString) return VString renames ASU."&";
   function "&" (V : VString; C : Character) return VString renames ASU."&";
   function "&" (C : Character; V : VString) return VString renames ASU."&";
   function "&" (I : Integer; V : VString) return VString;
   function "&" (V : VString; I : Integer) return VString;
   --  function "&" (V1, V2 : String) return VString;

   function "="  (Left, Right : VString) return Boolean renames ASU."=";
   function "<"  (Left, Right : VString) return Boolean renames ASU."<";
   function "<=" (Left, Right : VString) return Boolean renames ASU."<=";
   function ">"  (Left, Right : VString) return Boolean renames ASU.">";
   function ">=" (Left, Right : VString) return Boolean renames ASU.">=";
   --
   function "="  (Left : VString;
                  Right : String)
                  return Boolean renames ASU."=";
   function "<"  (Left : VString;
                  Right : String)
                  return Boolean renames ASU."<";
   function "<=" (Left : VString;
                  Right : String)
                  return Boolean renames ASU."<=";
   function ">"  (Left : VString;
                  Right : String)
                  return Boolean renames ASU.">";
   function ">=" (Left : VString;
                  Right : String)
                  return Boolean renames ASU.">=";

   function "="  (Left : String;
                  Right : VString)
                  return Boolean renames ASU."=";
   function "<"  (Left : String;
                  Right : VString)
                  return Boolean renames ASU."<";
   function "<=" (Left : String;
                  Right : VString)
                  return Boolean renames ASU."<=";
   function ">"  (Left : String;
                  Right : VString)
                  return Boolean renames ASU.">";
   function ">=" (Left : String;
                  Right : VString)
                  return Boolean renames ASU.">=";

-------------------------------------------------------------------------------
private

   generic type Abstract_Integer is range <>;
   function HAC_Generic_Image (I : Abstract_Integer) return String;
   function HAC_Image (I : Integer) return String;

------------------------------------------------------------------------------
end v20.Vst;
------------------------------------------------------------------------------
