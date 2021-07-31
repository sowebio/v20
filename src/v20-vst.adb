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
--  see .ads
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;

package body v20.Vst is

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

   function To_VString (C : Character) return VString is
   begin
      return To_VString ((1 => C));
   end To_VString;

   function To_VString (I : Integer) return VString is
      function HAC_Image_for_Integer is
      new v20.Vst.HAC_Generic_Image (Abstract_Integer => Integer);
   begin
      return +HAC_Image_for_Integer (I);
   end To_VString;

   function To_Integer (V : VString) return Integer is
   begin
      return Integer'Value (ASU.To_String (V));
   end To_Integer;

   function To_Integer (V : String) return Integer is
   begin
      return Integer'Value (V);
   end To_Integer;

   function To_String (V : VString) return String is
   begin
      return ASU.To_String (V);
   end To_String;

   --  Basics -----------------------------------------------------------------

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

   function Slice (Source : String; Low : Positive; High : Natural)
                  return VString is
   begin
      return +ASU.Slice (To_VString (Source), Low, High);
   end Slice;

   function Slice (Source : VString; Low : Positive; High : Natural)
                  return VString is
   begin
      return +ASU.Slice (Source, Low, High);
   end Slice;

   --  Extracting substring form pattern --------------------------------------

   function Tail_After_Match (Source : VString; Pattern : VString)
                              return VString is
      Result : VString := +"";
      Source_Length : constant Natural := Length (Source);
      Pattern_Length : constant Natural := Length (Pattern);
   begin
      for I in reverse 1 .. Source_Length loop
         if I <= Source_Length - Pattern_Length then
            if Slice (Source, I, I + Pattern_Length - 1) = Pattern then
               Result := Slice (Source, I + Pattern_Length, Source_Length);
               exit;
            end if;
         end if;
      end loop;
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
