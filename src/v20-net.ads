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

   function Command (Target : in VString ; Command_In : in VString ; SE_Output : out VString) return Boolean;
   function Command (Target : in VString ; Command_In : in VString) return Boolean;
   procedure Command (Target : VString ; Command_In : VString);
   --  Send remote command to host. Returns True if command successful (remote
   --  exitcode equal to 0).

   function Copy_File (Target : in VString ; File_Tx : in VString; Directory_Rx : in VString ; Options : in VString := +"") return Boolean;
   procedure Copy_File (Target : in VString ; File_Tx : in VString; Directory_Rx : in VString ; Options : in VString := +"");
   -- Copy to distant host. Returns True if copy successful.

   function Delete_Directory_Tree (Target : in VString ; Dir_Tree : VString) return Boolean;
   --  Delete a directory tree Dir_Tree. The directory and all of its contents
   --  (possibly including other directories) are deleted but adding a '*' at
   --  the end of the path preserve the last directory of the path (/one/two/
   --  deletes two but /on/two/* preserve two.
   --
   --  Return True if Dir_Tree is successfully deleted or was already deleted.
   --  Return False if operation is unsuccessful (i.e. if base directory tree
   --  was non existent or still exists after the deleting attempt).
   --
   --  Dir_Tree must be fully qualified, ie starting with a slash (/).
   --  This function prevents deletion of the following root directories (see
   --  Is_Root_Directory for further details). Pay close attention, you can't
   --  delete /etc but you are allowed to delete /etc/network !

   function Delete_File (Target : in VString ; File_To_Delete : in VString) return Boolean;
   procedure Delete_File (Target : in VString ; File_To_Delete : in VString);
   -- Remove File_To_Delete in remote host Target. Returns True if delete successful.

   function Directory_Exists (Target : in VString ; Name : String) return Boolean;
   function Directory_Exists (Target : in VString ; Name : VString) return Boolean;
   --  Returns True if distant directory Name exists.

   function File_Exists (Target : in VString ; Name : String) return Boolean;
   function File_Exists (Target : in VString ; Name : VString) return Boolean;
   --  Returns True if distant file Name exists.

   function Get_Network_From_Ip (Ip : in String) return VString;
   function Get_Network_From_Ip (Ip : in VString) return VString;
   --  Returns the network part of a /32 classless IP address.

   function Is_Ip_Ok (Ip : in String) return Boolean;
   function Is_Ip_Ok (Ip : in VString) return Boolean;
   --  Ip validation.

   function Is_Ping_Ok (Target : in VString) return Boolean;
   -- Return true if target answer to a ping.

   function Is_Root_Directory (Dir_Tree : VString) return Boolean;
   --  This function checks the following root directories: bin, boot, dev,
   --  etc, home, lib, lib32, lib64, libx32, lost+found, media, mnt, opt,
   --  proc, root, run, sbin, srv, sys, tmp, usr, var. Returns True if
   --  Dir_Tree is a root directory. Dir_Tree must be fully qualified, ie
   --  starting with a slash (/).

   function Is_Ssh_Ok (Target : in VString) return Boolean;
   --  Return true if target accepts a valid SSH connexion.

   procedure Mount (Target : VString);
   --  Mount a host

   function Mount_Remote (Remote_Host : VString ; Target_To_Mount : VString ; Mount_Point : VString ; Mount_Options : in VString := +"") return Boolean;
   procedure Mount_Remote (Remote_Host : VString ; Target_To_Mount : VString ; Mount_Point : VString ; Mount_Options : in VString := +"");
   --  Mount a Mount_Point targetting Target_To_Mount on Remote_Host with
   --  options Mount_Options. All mount options are accepted. Returns true if
   --  operation is successful.

   procedure Set_Exception (Set_Unset : Boolean := True);
   function Set_Exception return Boolean;
   -- Enable Exception processing, which is disabled by default. A call without
   -- parameter returns the Exception status (enable or disabled).

   function Set_Hostname (Target : VString ; Hostname : VString) return Boolean;
   -- Set Hostname for a Target host. Returns true if command ok.

   function Set_Key (Key : VString := +"") return Boolean;
   -- Set SSH private key used to log in distant hosts with commands like
   -- Send_Command and Send_File. Key validity is checked. Returns true if
   -- Key is properly set.

   procedure Set_Key;
   -- Delete the key previously set.

   procedure Set_Message (Msg : Boolean := True);
   -- Control console message when using commands like Send_Command and
   -- Send_File. Default is console message enable. A call without parameter
   -- enable console output.

   procedure Set_Output (Output : Boolean := True);
   -- Control console output when using commands like Send_Command and
   -- Send_File. Default is console output enable. A call without parameter
   -- enable console output.

   procedure Unmount (Target : VString);
   -- Unmount a host.

   function Unmount_Remote (Remote_Host : VString ; Mount_Point : VString) return Boolean;
   procedure Unmount_Remote (Remote_Host : VString ; Mount_Point : VString);
   --  Unmount a Mount_Point on a Remote_Host. Mount_Point is then deleted.
   --  Returns true if the whole operation is successful.

   Error_Mount : exception;
   --  Raised when mount error.

   Error_Command : exception;
   --  Raised when send command error.

   Error_Copy_File : exception;
   --  Raised when send file error.

   Error_Unmount : exception;
   --  Raised when unmount error.

   -- Exception handling example

   -- procedure Exception_Termination (Message : String);
   -- Program termination handling

   -------------------------------------------------------------------------
   --  procedure xxx is
   --
   --  begin
   --
   --     Net.Command (+"",+"");
   --
   --  exception
   --
   --     when Net.Error_Mount =>
   --        Net.Exception_Termination ("Instance.Mount");
   --     when Net.Error_Unmount =>
   --        Net.Exception_Termination ("Instance.Unmount");
   --     when Net.Error_Command =>
   --        Net.Exception_Termination ("Instance.Send_Command");
   --     when Net.Error_Copy =>
   --        Net.Exception_Termination ("Instance.Send_File");
   --     --when Net.Error : others =>
   --     --   Net.Exception_Termination ("Instance.Unregistered");
   --
   --  end xxx;

------------------------------------------------------------------------------
private

   SSH_Default_Options : constant VString := +" -q -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null ";
   SSH_Exception : Boolean := False;
   SSH_Key : VString := +"";
   SSH_Message : Boolean := True;
   SSH_Output : Boolean := True;

-------------------------------------------------------------------------------
end v20.Net;
-------------------------------------------------------------------------------
