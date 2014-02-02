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

   -----------------------
   -- Allocate_An_Agent --
   -----------------------

   procedure Allocate_An_Agent (Agent : out Agent_Id_Type) is
   begin
      if not Has_Space then
         raise Agent_Pool_Capacity_Error;
      end if;

      Agent     := Free_Cell;
      Free_Cell := Free_Cell + 1;
   end Allocate_An_Agent;

   -------------------------------
   -- Allocate_An_Agent_With_Id --
   -------------------------------

   procedure Allocate_An_Agent_With_Id (Id : in Agent_Id_Type) is null;
   --  This procedure should only be used for Oak_Agent_Records, where for all
   --  except the Sleep task, Ids and and their respective storage location is
   --  determined by the primary storage pool for that Agent type. Thus there
   --  is no free list to consider for Oak_Agents.

   ----------------------
   -- Deallocate_Agent --
   ----------------------

   procedure Deallocate_Agent (Id : in Agent_Id_Type) is null;
   --  In this implementation of Agent storage, agents are not deallocated.

   -------------------
   -- Setup_Storage --
   -------------------

   procedure Setup_Storage is
   begin
      Storage_Ready := True;
   end Setup_Storage;

end Oak.Agent.Storage;
