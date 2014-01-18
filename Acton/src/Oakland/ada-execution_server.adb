------------------------------------------------------------------------------
--                                                                          --
--                            OAKLAND COMPONENTS                            --
--                                                                          --
--                           ADA.EXECUTION_SERVER                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2013-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

package body Ada.Execution_Server is

   procedure Remove_Execution_Server (Server : in out Execution_Server) is
   begin
      raise Program_Error;
--        Remove_Agent_From_Scheduler (Server.Server_Object'Unchecked_Access);
   end Remove_Execution_Server;
end Ada.Execution_Server;
