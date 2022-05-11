-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▛▌
--  ▚▘▙▖█▌
--
--  @file      v20-net.ads
--  @copyright See authors list below and v20.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
------------------------------------------------------------------------------
--  @summary
--  V20 library common
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  see v20.ads
------------------------------------------------------------------------------

--  with Ada.Calendar;
--  with Ada.Command_Line;
--  with Ada.Directories;

with GNAT.OS_Lib;

with v20.Vst; use v20.Vst;

package v20.Net is

   package GOL renames GNAT.OS_Lib;

   function Ip_Check (Ip : in String) return Boolean;
   function Ip_Check (Ip : in VString) return Boolean;
   --  Ip validation

   function Get_Network_From_Ip (Ip : in String) return VString;
   function Get_Network_From_Ip (Ip : in VString) return VString;
   --  Returns the network part of a /32 classless IP address.

   procedure Mount (Target : VString);
   -- Mount a host

   procedure Send_Command (Target : in VString ; Command : in VString);
   -- Send distant command to host

   procedure Send_File (Target : in VString ; File_Tx : in VString; Directory_Rx : in VString);
   -- Copy to distant host

   procedure Unmount (Target : VString);
   -- Unmount a host

   Error_Mount : exception;
   --  Raised when mount error

   Error_Send_Command : exception;
   --  Raised when send command error

   Error_Send_File : exception;
   --  Raised when send file error

   Error_Unmount : exception;
   --  Raised when unmount error

   procedure Exception_Termination (Message : String);
   -- Program termination handling

-------------------------------------------------------------------------------
end v20.Net;
-------------------------------------------------------------------------------
