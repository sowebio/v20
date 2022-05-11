-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20-net.adb
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

with v20.Log;
with v20.Prg;
with v20.Sys;

package body v20.Net is

   ---------------------------------------------------------------------------
   function Ip_Check (Ip : in VString) return Boolean is
      Ip_Part : VString := +"";
      Ip_Num : Natural;
      Result : Boolean := False;
   begin
      if not Empty (Ip) then
         if (Field_Count (Ip, ".") = 4) then
            for I in 1..4 loop
               Ip_Part := Field_By_Index (Ip, I, ".");
               if Is_Numeric (Ip_Part) then
                  Ip_Num := To_Integer (Ip_Part);
                  if not ((Ip_Num >= 0) and (Ip_Num <= 255)) then
                     exit;
                  end if;
                  if (I = 4) then
                     Result := True;
                  end if;
               else
                  exit;
               end if;
            end loop;
         end if;
      end if;
      return Result;
   end Ip_Check;

   function Ip_Check (Ip : in String) return Boolean is
   begin
      return Ip_Check (To_VString (Ip));
   end Ip_Check;

   ---------------------------------------------------------------------------
   function Get_Network_From_Ip (Ip : in VString) return VString is
      Result : VString := +"";
   begin
      if Ip_Check (Ip) then
         Result := Field_By_Index (Ip, 1, ".") & "." &
                   Field_By_Index (Ip, 2, ".") & "." &
                   Field_By_Index (Ip, 3, ".");
      end if;
      return Result;
   end Get_Network_From_Ip;

   function Get_Network_From_Ip (Ip : in String) return VString is
   begin
      return Get_Network_From_Ip (To_VString (Ip));
   end Get_Network_From_Ip;

   ---------------------------------------------------------------------------
   procedure Mount (Target : VString) is
      Mount_Point : constant VString := (if Prg.Is_User_Not_Root then Sys.Get_Home else +"") & "/mnt/" & Target;
      Exec_Error : Integer;
   begin

      -- Default creation for safety
      Sys.Shell_Execute (+"mkdir --parents " & Mount_Point & STD_ERR_OUT_REDIRECT, Exec_Error);
      if (Exec_Error = 0) then
         -- Mount
         Sys.Shell_Execute (+"sshfs " & Target & ":/ " & Mount_Point & STD_ERR_OUT_REDIRECT, Exec_Error);
         --Sys.Shell_Execute (+"sshfs root@" & Target & ".genesix.org:/ " & Mount_Point & STD_ERR_OUT_REDIRECT, Exec_Error);
         if (Exec_Error = 0) then
            Log.Msg (Target & " mounted in " & Mount_Point);
         else
            Log.Err ("Srv.Mount: Error mounting " & Target & " in " & Mount_Point & " Error: " & To_VString (Exec_Error));
            raise Error_Mount;
         end if;
      else
         Log.Err ("Srv.Mount: Error creating local mount point " & Mount_Point & " Error: " & To_VString (Exec_Error));
         raise Error_Mount;
      end if;

   end Mount;

   ---------------------------------------------------------------------------
   procedure Send_Command (Target : in VString ; Command : in VString) is
      Exec_Error : Integer;
   begin

      -- -q : Quiet
      -- -o StrictHostKeyChecking=no : avoid 'The authenticity...can't be established. Are you sure...connecting (yes/no)?'
      -- ssh -q -o StrictHostKeyChecking=no $file1 $file2 $fileN root@$host:$dir/

      -- Command must be between quotation marks to keep the redirects on the distant host
      Sys.Shell_Execute (+"ssh -q -o StrictHostKeyChecking=no " & Target & " " & DQ & Command & DQ & STD_ERR_OUT_REDIRECT, Exec_Error); -- & STD_ERR_OUT_REDIRECT, Exec_Error);
      if (Exec_Error = 0) then
         Log.Msg ("Distant command " & Command & " on " & Target);
      else
         Log.Err ("Srv.Send_Command: Command error with " & Command & " on " & Target & " Error: " & To_VString (Exec_Error));
         raise Error_Send_Command;
      end if;

   end Send_Command;

   ---------------------------------------------------------------------------
   procedure Send_File (Target : in VString ; File_Tx : in VString; Directory_Rx : in VString) is
      Command_String : constant VString := +"scp " & File_Tx & " " & Target & ":" & Directory_Rx & STD_ERR_OUT_REDIRECT;
      Exec_Error : Integer;
   begin

      -- Default creation for safety
      Send_Command (Target, +"mkdir --parents " & Directory_Rx);

      Sys.Shell_Execute (Command_String, Exec_Error);
      if (Exec_Error = 0) then
         Log.Msg ("Distant copy " & Command_String & " on " & Target);
      else
         Log.Err ("Srv.Send_File: Copy error with " & Command_String & " to " & Target & " Error: " & To_VString (Exec_Error));
         raise Error_Send_File;
      end if;

   end Send_File;

   ---------------------------------------------------------------------------
   procedure Unmount (Target : VString) is
      Mount_Point : constant VString := (if Prg.Is_User_Not_Root then Sys.Get_Home else +"") & "/mnt/" & Target;
      Exec_Error : Integer;
   begin

      Sys.Shell_Execute (+"umount " & Mount_Point & STD_ERR_OUT_REDIRECT, Exec_Error);
      if (Exec_Error = 0) then
         Log.Msg (Target & " unmounted from " & Mount_Point);
      else
         Log.Err ("Srv.Unmount: Error unmounting " & Target & " from " & Mount_Point & " Error: " & To_VString (Exec_Error));
         raise Error_Unmount;
      end if;

   end Unmount;

------------------------------------------------------------------------------
--- PUBLIC VARIABLES AND CONSTANTS
------------------------------------------------------------------------------

-- Tor     = Tor IP + Random download delay + Random download rate + Shuffle products
-- not Tor = Raw IP + 1s download delay + 200K download rate

--  Sys_Tor : Boolean := False;
--
--  Sys_Tor_Command : VString := +"";
--  Sys_Tor_Status : VString := +"";  -- RAW|TOR
--  Sys_Tor_Get_Counter : Integer := 0; -- Get counter
--  Sys_Tor_IP_Rotate : Integer := 50;  -- IP rotation each 50 Get
--
--  Sys_Random_Range : constant Integer := 5;  -- Rand (5) => 0..5 (6 values)
--  Sys_Random_Offset : constant Integer := 3; -- + 3 -> 3 to 8 seconds delay
--
--  Sys_Output   : constant VString := Script_Temp & "sys.out";
--  Sys_Error    : constant VString := Script_Temp & "sys.err";
--  Sys_Redirect : constant VString := +" 2>" & Sys_Error & " 1>" & Sys_Output; --2>/dev/null

------------------------------------------------------------------------------

--  Web_Get_File_Command : constant VString := +"curl";
--  Web_Get_File_Random_Range : constant Integer := 90;  -- Rand (90) => 0..90 (91 values)
--  Web_Get_File_Random_Offset : constant Integer := 90; -- + 90 -> 90 to 170 kbps
--
--  Web_Output   : constant VString := Script_Temp & "web.out";
--  Web_Error    : constant VString := Script_Temp & "web.err";
--  Web_Redirect : constant VString := +" 2>" & Web_Error & " 1>" & Web_Output; --2>/dev/null

------------------------------------------------------------------------------

--  TAB : constant VString := To_Vstring(Chr(9)); -- Tab
--  LF : constant VString := To_Vstring(Chr(12)); -- Line Feed
--  DQ : constant VString := To_Vstring(Chr(34)); -- Double quote
--
--  CD : constant VString := +","; -- Comma delimiter
--  FD : constant VString := +":"; -- Field delimiter
--  TD : constant VString := +"~"; -- Tilde delimiter
--  AD : constant VString := +"^"; -- Accent delimiter
--
--  -- Element - <p>blahblah</p>
--  TS : constant VString := +"<";  -- HTML start TAG of ELEMENT
--  TE : constant VString := +"</"; -- HTML end TAG of ELEMENT
--  TC : constant VString := +">";  -- HTML close TAG of ELEMENT
--
--  -- Attribute - href="blahblah"
--  AS : constant VString := +"="; -- HTML start TAG of ATTRIBUTE
--  AE : constant VString := DQ;   -- HTML end TAG of ATTRIBUTE
--  AC : constant VString := DQ;   -- HTML close TAG of ATTRIBUTE

------------------------------------------------------------------------------
-- SYS FUNCTIONS
------------------------------------------------------------------------------

-- Sys_Tor_IP_Change : Tor IP change
-- Sys_Delay         : Get a file after a random delay (between 3..8s by default)
-- Sys_Exec          : Execute shell Command_String and return result

------------------------------------------------------------------------------
--procedure Sys_Tor_IP_Change is
------------------------------------------------------------------------------
-- Description : Tor IP change
-- Arguments   : None
-- Return      : None
------------------------------------------------------------------------------

--  Exec_Error : Integer := 0;
--
--  begin
--
--    if Exists (Web_Output) then
--      Delete_File (Web_Output);
--    end if;
--
--    if Exists (Web_Error) then
--      Delete_File (Web_Error);
--    end if;
--
--    Log_Msg (+"Changing Tor IP");
--
--    -- NEWNYM : Switch to clean circuits, so new application requests don't share any circuits with old ones.
--    -- Also clears the client-side DNS cache. (Tor MAY rate-limit its response to this signal.)
--    --
--    -- ANSWER : The server responds with "250 OK" if the signal is recognized (or simply closes the socket
--    -- if it was asked to close immediately), or "552 Unrecognized signal" if the signal is unrecognized.
--    --
--    -- Netcat EOL is CRLF. CR suppression by sed $'s/\r$//'
--    -- web.out :
--    -- 250 OK                 <--- fake passwd
--    -- 250 OK                 <--- IP change command
--    -- 250 closing connection <--- quit
--
--    Shell_Execute ("(echo authenticate '" & DQ & DQ & "'; echo signal newnym; echo quit) | nc localhost 9051 | sed $'s/\r$//'" & Web_Redirect, Exec_Error);
--    if (Exec_Error = 0) then
--      Log_Msg (+"Tor IP changed");
--    else
--      Log_Err (+"Sys_Tor_IP_Change - Error: " & Image (Exec_Error));
--    end if;
--
--  end Sys_Tor_IP_Change;

------------------------------------------------------------------------------
--procedure Sys_Delay is
------------------------------------------------------------------------------
-- Description : Random delay between 3..8s (by default)
-- Arguments   : See public variables Sys_Random_Range and Sys_Random_Offset
-- Return      : None
------------------------------------------------------------------------------

--  Exec_Error : Integer := 0;
--  Random_Value : VString := +"";
--  Random_Seed, Random_Result : Integer := 0;
--
--  begin
--
--    if Sys_Tor then
--      -- Training
--      Random_Seed := Rand (10) + 10;
--        for I in 1 .. Random_Seed loop
--         Random_Result := Rand (I);
--      end loop;
--
--      Random_Value := Image (Rand (Sys_Random_Range) + Sys_Random_Offset);
--      Log_Msg ("Random wait: " & Random_Value & "s");
--    else
--      Random_Value := +"2";
--      Log_Msg ("Fixed wait: " & Random_Value & "s");
--    end if;
--
--    Shell_Execute ("sleep " & Random_Value, Exec_Error);
--    if (Exec_Error > 0) then
--      Log_Err (+"Sys_Delay - Error: " & Image (Exec_Error));
--    end if;
--
--  end Sys_Delay;


------------------------------------------------------------------------------
--- WEB FUNCTIONS
------------------------------------------------------------------------------

-- Web_Get_Buffer          : Get a Buffer from Url without delay
-- Web_Get_File            : Get a file from Url after a random delay (between 3..8s by default)
-- Web_Read_File           : Read a file and returning it in a buffer without EOL character
-- Web_Get_Html_Value      : Return a Keyword value in a Line starting at a strict Trigger. Handle HTML tags & attributes
-- Web_Get_Html_Value_List : Iterative multiple values handler through a buffer starting at a strict. Handle HTML tags & attributes

-- HTML Vocabulary -----------------------------------------------------------
--
-- ELEMENT :
-- <p>This is the content of the paragraph element</p> (could be on more than one line)
-- TAGS of element : <p> and </p>
-- VALUE of element : This is the content of the paragraph element.
--
-- ATRIBUTES of ELEMENT
-- <a href="/this/is/a/link/">Learn about the a href attribute</a>
-- TAGS of element : <a href="/this/is/a/link/" and </a>
-- VALUE of element : Learn about the a href attribute
-- ATTRIBUTE of element : href
-- ATRIBUTE VALUE of element : /this/is/a/link/

------------------------------------------------------------------------------
--function Web_Get_Buffer (Url : VString ; EOL_Char : VString) return VString is
------------------------------------------------------------------------------
-- Description : Get a Buffer from Url without delay
-- Arguments   : Url
-- Return      : Buffer
------------------------------------------------------------------------------

--  Exec_Error : Integer := 0;
--  File_Handle : File_Type;
--  Line_Buffer : VString := +"";
--  Result_Buffer : VString := +"";
--
--  begin
--
--    Shell_Execute (Sys_Tor_Command &
--                   Web_Get_File_Command & " " &
--                   "--location " & -- follow redirections
--                   "--silent " &   -- follow redirections
--                   "--output " &   -- file name output
--                   DQ & Script_Temp & "get_buffer" & DQ & " " &
--                   Url, Exec_Error);
--
--    if (Exec_Error = 0) then
--      if Exists (Script_Temp & "get_buffer") then
--
--        Open (File_Handle, Script_Temp & "get_buffer");
--        while not (End_Of_File (File_Handle)) loop
--          Get_Line (File_Handle, Line_Buffer);
--          Result_Buffer := Result_Buffer & Line_Buffer & EOL_Char;
--        end loop;
--        Close (File_Handle);
--
--      else
--        Log_Err (+"Web_Get_Buffer - Can't open Get buffer file");
--      end if;
--    else
--      Log_Err (+"Web_Get_Buffer - Error: " & Image (Exec_Error));
--    end if;
--
--    return Result_Buffer;
--
--  end Web_Get_Buffer;

------------------------------------------------------------------------------
--function Web_Get_File (Url : VString ; File_Output : VString) return Boolean is
------------------------------------------------------------------------------
-- Description : Get a file from Url after a random delay (between 3..8s by default)
-- Arguments   : Url
-- Return      : None
------------------------------------------------------------------------------

--  Exec_Error : Integer := 0;
--  Result : Boolean := True;
--  IP_Value : VString := Web_Get_Buffer (+"https://api.ipify.org",+"");
--  Random_Value : VString := +"";
--  Random_Seed, Random_Result : Integer := 0;
--
--  begin
--
--    -- File backup
--
--    Fls_Backup_Name (File_Output);
--
--    -- IP Rotation
--    if Sys_Tor then
--      Sys_Tor_Get_Counter := Sys_Tor_Get_Counter + 1;
--      if ((Sys_Tor_Get_Counter mod Sys_Tor_IP_Rotate) = 0) then
--        Sys_Tor_Get_Counter := 1;
--        Sys_Tor_IP_Change;
--      end if;
--    end if;
--
--    -- Random delay before getting file
--    if not Web_Test_No_Download then
--      Sys_Delay;
--    end if;
--
--    if Sys_Tor then
--      -- Compute download rate with preliminary training
--      Random_Seed := Rand (10) + 10;
--        for I in 1 .. Random_Seed loop
--         Random_Result := Rand (I);
--      end loop;
--      Random_Value := Image (Rand (Web_Get_File_Random_Range) + Web_Get_File_Random_Offset);
--    else
--      Random_Value := +"200";
--    end if;
--
--    Log_Msg (Sys_Tor_Status & ":" & IP_Value & "@" & Random_Value & "kbps - Get " & Url & " > " & File_Output);
--
--    -- Tor Ip rotation each Sys_Tor_IP_Rotate rounds
--    if Sys_Tor then
--      Log_Msg ("Next Ip rotation in: " & Image(Sys_Tor_IP_Rotate - Sys_Tor_Get_Counter));
--    end if;
--
--    -- Download
--
--    Shell_Execute (Sys_Tor_Command &
--                   Web_Get_File_Command & " " &
--                   "--location " &                          -- follow redirections
--                   "--limit-rate " & Random_Value & "k " &  -- random rate limit
--                   "--output " &                            -- file name output
--                   DQ & File_Output & DQ & " " &
--                   DQ & Url & DQ, Exec_Error);
--
--    if (Exec_Error > 0) then
--      Log_Err (+"Web_Get_File - Error: " & Image (Exec_Error));
--      Result := False;
--    end if;
--
--    return (Result and Exists (File_Output));
--
--  end Web_Get_File;

------------------------------------------------------------------------------
--function Web_Read_File (File_To_Read : VString) return VString is
------------------------------------------------------------------------------
-- Description : Read a file and returning it in a buffer without EOL character
-- Arguments   : File_To_Read
-- Return      : Buffer's file
------------------------------------------------------------------------------

--  File_Handle : File_Type;
--  Line_Buffer : VString := +"";
--  Result_Buffer : VString := +"";
--
--  begin
--
--    if Exists (File_To_Read) then
--      Open (File_Handle, File_To_Read);
--      while not (End_Of_File (File_Handle)) loop
--        Get_Line (File_Handle, Line_Buffer);
--        Result_Buffer := Result_Buffer & Line_Buffer;
--      end loop;
--      Close (File_Handle);
--    else
--      Log_Err (+"Web_Read_File - File does not exist: " & File_To_Read);
--    end if;
--
--    return Result_Buffer;
--
--  end Web_Read_File;

------------------------------------------------------------------------------
--function Web_Get_Html_Value (Line : VString; Trigger : VString; Keyword : VString) return VString is
------------------------------------------------------------------------------
-- Description : Return a Keyword value in a Line containing strict Trigger. Handle HTML tags & attributes
-- Arguments   : Line, Trigger, Keyword
-- Return      : Tag value
------------------------------------------------------------------------------

--  Buffer_String : VString := Line;
--  Buffer_length : Natural := Length (Buffer_String);
--  Trigger_length : Natural := Length (Trigger);
--  Keyword_length : Natural := Length (Keyword);
--  Result_String, Result_Char : VString := +"";
--
--  Start_Index, Start_Trigger, TE_Index : Natural := 0;
--
--  Element_List : VString := +"a:li:p:span";
--  Attribute_List : VString := +"alt:class:href:title";
--
--  type States is (S_Idle, S_Wait_For_TC, S_Wait_For_TE);
--  State : States := S_Idle;
--
--  begin
--
--  -- TS : "<"  HTML start TAG of ELEMENT
--  -- TE : "</" HTML end TAG of ELEMENT
--  -- TC : ">"  HTML close TAG of ELEMENT
--
--    --Log_Dbg ("Buffer: " & Buffer_String);
--    Log_Dbg ("Buffer length: " & Image (Buffer_Length));
--    Log_Dbg ("Trigger: " & DQ & Trigger & DQ);
--    Log_Dbg ("Keyword: " & Keyword);
--
--    Start_Trigger := Index (Buffer_String, DQ & Trigger & DQ);
--    if (Start_Trigger > 0) then
--
--      Buffer_String := Slice (Buffer_String, Start_Trigger + Trigger_length + 1, Buffer_Length);
--      Buffer_Length := Length (Buffer_String);
--
--      Log_Dbg ("Trigger Index: " & Image (Start_Trigger));
--      Log_Dbg ("Trigger Output: " & Slice (Buffer_String, 1, Trigger_length + 1));
--
--      if Vst_Search_Field (Element_List, Keyword, FD) then
--
--        -- Element value
--
--        Log_Dbg (+"Keyword: Element");
--
--        Start_Index := Index (Buffer_String, TS & Keyword);
--        if (Start_Index > 0) then
--
--          Log_Dbg ("Keyword Index: " & Image (Start_Index));
--
--          State := S_Wait_For_TC;
--
--          for I in (Start_Index + length (TS) + Keyword_length) .. Buffer_Length loop
--            Result_Char := Slice (Buffer_String, I, I);
--            if State = S_Wait_For_TC then
--              if Result_Char = TC then
--                State := S_Wait_For_TE;
--              end if;
--            elsif State = S_Wait_For_TE then
--              Result_String := Result_String & Result_Char;
--              TE_Index := Index_Backward (Result_String, TE & Keyword & TC);
--              if (TE_Index > 1) then
--                Result_String := Slice (Result_String, 1, TE_Index - 1);
--                exit;
--              end if;
--            end if;
--          end loop;
--
--        end if;
--
--      else
--
--        -- Attribute value
--
--        Log_Dbg (+"Keyword: Attribute");
--
--        Start_Index := Index (Buffer_String, Keyword & "=" & DQ);
--        if (Start_Index > 0) then
--
--          -- + 2 because 2 DQ (double quote) around Keyword in Buffer_String...
--          if ((Start_Index + Keyword_length + 2) < Buffer_Length) then
--            for I in (Start_Index + Keyword_length + 2) .. Buffer_Length loop
--              Result_Char := Slice (Buffer_String, I, I);
--              if Result_Char = DQ then
--                exit;
--              else
--                Result_String := Result_String & Result_Char;
--              end if;
--            end loop;
--          end if;
--        end if;
--
--        if Result_Char /= DQ then
--           Result_String := +"";
--        end if;
--
--      end if;
--    end if;
--
--    Log_Dbg (Result_String);
--
--    return Result_String;
--
--  end Web_Get_Html_Value;

-- <p class="show-for-sr">MEREROUKA 2 o40</p>

------------------------------------------------------------------------------
--  procedure Web_Get_Html_Value_List (Buffer : VString;              -- Page to process
--                                     Trigger : VString;             -- Identifier where to start
--                                     Keyword : VString;             -- Element or Attribute
--                                     Div_Counter : in out Natural;  -- <div ...> </div> counter for parse break
--                                     Buffer_Index : in out Natural; -- Current buffer index to resume at next call
--                                     Result_String : out VString)   -- Value found
--                                     is
------------------------------------------------------------------------------
-- Description : Iterative multiple values handler through a buffer starting at a strict. Handle HTML tags & attributes
-- Arguments : See above
-- Return : n/a
--
-- Todo : replace Keyword by Element and Attribute to process string like  :
-- <p class="show-for-sr">MEREROUKA 2 o40</p>
--  ^        ^            ^
--  |        +- Attribute +- Value
--  +- Element
--
-- Element - <p>blabla</p>
-- TS := "<"  HTML start TAG of ELEMENT
-- TE := "</" HTML end TAG of ELEMENT
-- TC := ">"  HTML close TAG of ELEMENT
--
-- Attribute - href="blabla"
-- AS : "=" HTML start TAG of ATTRIBUTE
-- AE : DQ  HTML end TAG of ATTRIBUTE
-- AC : DQ  HTML close TAG of ATTRIBUTE
------------------------------------------------------------------------------

--  Buffer_String : VString := Buffer;
--  Result_Char, Trigger_Keyword, Trigger_Keyword_End, Trigger_Value_End : VString := +"";
--
--  Buffer_Length : Natural := Length (Buffer_String);
--  Keyword_Length : Natural := Length (Keyword);
--
--  Start_Index, Start_Trigger, Start_Iterate, End_Index : Natural := 0;
--  Element_List : VString := +"a:li:p:span";
--  Attribute_List : VString := +"alt:class:href:title";
--
--  type States is (Search_TS_AS, Search_TE_AE, Search_TC_AC);
--  State : States := Search_TS_AS;
--
--  Pattern_Buffer : VString := +"";
--  Pattern_Div_Open : VString := +"<div ";
--  Pattern_Div_Close : VString := +"</div>";
--
--  Keyword_Element : Boolean := Vst_Search_Field (Element_List, Keyword, FD);
--
--  begin
--
--    --Log_Dbg ("Buffer: " & Buffer_String);
--    Log_Dbg ("Buffer length: " & Image (Buffer_Length));
--    Log_Dbg ("Trigger: " & DQ & Trigger & DQ);
--    Log_Dbg ("Keyword: " & Keyword);
--
--    Log_Dbg ("Buffer_Index: " & Image (Buffer_Index));
--
--    if (Buffer_Index = 0) then
--      -- First call : seek to Trigger
--      Start_Trigger := Index (Buffer_String, DQ & Trigger & DQ);
--      Start_Iterate := Start_Trigger + Length (DQ & Trigger & DQ) + 1;
--    else
--      -- Next calls : resume at the previous exit
--      Start_Trigger := Buffer_Index;
--      Start_Iterate := Start_Trigger;
--    end if;
--
--    Log_Dbg ("Start Trigger/Index: " & Image (Start_Trigger));
--
--    -- Reach Trigger in page
--
--    if (Start_Trigger > 0) then
--
--      if (Start_Iterate > Buffer_Length) then
--        Start_Iterate := Buffer_Length;
--      end if;
--
--      if (Buffer_Index = 0) then
--        Log_Dbg ("TRIGGER FOUND: " & DQ & Trigger & DQ);
--      end if;
--
--      -- Element or Attribute presets
--
--      if Keyword_Element then
--        Log_Dbg (+"Element");
--        Trigger_Keyword := TS & Keyword & " ";
--        Trigger_Keyword_End := TC;
--        Trigger_Value_End := TE & Keyword & TC;
--      else
--        Log_Dbg (+"Attribute");
--        Trigger_Keyword := Keyword & AS;
--        Trigger_Keyword_End := AE;
--        Trigger_Value_End := AC;
--      end if;
--
--      Result_String := +"";
--
--      -- Iterate
--
--      for I in Start_Iterate .. Buffer_Length loop
--
--        Result_String := Result_String & Slice (Buffer_String, I, I);
--
--        -- <div>.. </div> counting/decounting
--
--        if (Length (Result_String) >= Length (Pattern_Div_Open)) then
--          Pattern_Buffer := Slice (Result_String, Length (Result_String) - Length (Pattern_Div_Open) + 1, Length (Result_String));
--          if (Pattern_Buffer = Pattern_Div_Open) then
--            Div_Counter := Div_Counter + 1;
--            Log_Dbg ("Div_Counter incremented: " & Image (Div_Counter));
--          end if;
--        end if;
--
--        if (Length (Result_String) >= Length (Pattern_Div_Close)) then
--          Pattern_Buffer := Slice (Result_String, Length (Result_String) - Length (Pattern_Div_Close) + 1, Length (Result_String));
--          if (Pattern_Buffer = Pattern_Div_Close) then
--            Div_Counter := Div_Counter - 1;
--            Log_Dbg ("Div_Counter decremented: " & Image (Div_Counter));
--            if Div_Counter = 0 then
--              Log_Dbg (+"Exit by <div> break");
--              exit;
--            end if;
--          end if;
--        end if;
--
--        -- Processing string
--
--        if State = Search_TS_AS then
--          End_Index := Index_Backward (Result_String, Trigger_Keyword);
--          if (End_Index > 0) then
--            State := Search_TE_AE;
--            Result_String := +"";
--            Log_Dbg ("KEYWORD FOUND: " & Trigger_Keyword);
--          end if;
--        elsif State = Search_TE_AE then
--          End_Index := Index_Backward (Result_String, Trigger_Keyword_End);
--          if (End_Index > 0) then
--            State := Search_TC_AC;
--            Result_String := +"";
--            Log_Dbg ("VALUE REACHED: " & Trigger_Keyword_End);
--            Log_Dbg (+"Start recording 'value'");
--          end if;
--        elsif State = Search_TC_AC then
--          End_Index := Index_Backward (Result_String, Trigger_Value_End);
--          if (End_Index > 0) then
--            Result_String := Slice (Result_String, 1, End_Index - 1);
--            if (Length (Result_String) > 0) then
--              Buffer_Index := I + 1;
--              Log_Dbg ("VALUE FOUND: " & Result_String);
--              Log_Dbg (+"End recording 'value'");
--              Log_Dbg (+"Exit by 'value' returned");
--              Log_Dbg (+"Exit Index: " & Image (Buffer_Index));
--              exit;
--            else
--              State := Search_TS_AS;
--              Result_String := +"";
--              Log_Dbg (+"VALUE EMPTY and DISCARDED, resume KEYWORD SEARCH");
--            end if;
--          end if;
--
--        end if;
--
--      end loop;
--
--    end if;
--
--  -- pause
--
--  end Web_Get_Html_Value_List;


   ---------------------------------------------------------------------------
   procedure Exception_Termination (Message : String) is
   begin
      Log.Err (Message & ": exception raised, program end");
      Log.Title ("");
      GOL.OS_Exit (100);
   end Exception_Termination;

-------------------------------------------------------------------------------
end v20.Net;
-------------------------------------------------------------------------------
