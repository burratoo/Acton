------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                            OAK.AGENT.STORAGE                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2013-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

package body Oak.Agent.Storage is

   procedure Allocate_An_Agent (Agent : out Agent_Id_Type) is
   begin
      if not Has_Space then
         raise Agent_Pool_Capacity_Error;
      end if;

      Agent     := Free_Cell;
      Free_Cell := Free_Cell + 1;
   end Allocate_An_Agent;

   procedure Setup_Storage is
   begin
      Storage_Ready := True;
   end Setup_Storage;

end Oak.Agent.Storage;
