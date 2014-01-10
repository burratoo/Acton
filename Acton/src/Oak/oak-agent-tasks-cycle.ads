--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                           OAK.AGENT.TASKS.CYCLE                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2013-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package handles the cyclic aspect of tasks in Oak.

package Oak.Agent.Tasks.Cycle with Preelaborate is

   procedure New_Cycle
     (For_Task         : in  Task_Id;
      Next_Task_To_Run : out Oak_Agent_Id);
   --  Causes the Agent to commence a new cycle. Recalculates new timing
   --  values for cycle dependent Agent components and holds the task till its
   --  next release event.

   procedure Release_Task
     (Task_To_Release  : in Task_Id;
      Releasing_Task   : in Oak_Agent_Id;
      Next_Task_To_Run : out Oak_Agent_Id);
   --  Releases the specified aperiodic or sporadic task that may be held by
   --  Oak.

   procedure Setup_Cyclic_Section (For_Task : in Task_Id);
   --  Setup the cyclic section for a task. This prepares the Agent structures
   --  for cyclic operations.

end Oak.Agent.Tasks.Cycle;
