------------------------------------------------------------------------------
--                                                                          --
--                            OAKLAND COMPONENTS                            --
--                                                                          --
--                      ADA.EXECUTION_SERVER.OPERATIONS                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2014-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

package Ada.Execution_Server.Operations is
   function Scheduler_Id_For_Server
     (Server : in Execution_Server'Class)
      return Oak.Agent.Scheduler_Id_With_No;

private
   function Scheduler_Id_For_Server
     (Server : in Execution_Server'Class)
      return Oak.Agent.Scheduler_Id_With_No is (Server.Scheduler);

end Ada.Execution_Server.Operations;
