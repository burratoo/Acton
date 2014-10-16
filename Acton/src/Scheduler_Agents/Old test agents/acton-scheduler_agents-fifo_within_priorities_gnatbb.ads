------------------------------------------------------------------------------
--                                                                          --
--                           ACTON SCHEDULER AGENT                          --
--                                                                          --
--               ACTON.SCHEDULER_AGENTS.FIFO_WITHIN_PRIORITIES              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);

with Oak.Agent;           use Oak.Agent;
with System;              use System;

package Acton.Scheduler_Agents.FIFO_Within_Priorities_GNATBB
  with Preelaborate is

   procedure New_Scheduler_Agent
     (Agent        : out Scheduler_Id;
      Min_Priority : in  Any_Priority;
      Max_Priority : in  Any_Priority);

   Agent_Name : constant String := "Fixed_Priority_Scheduler_Basic";
   Stack_Size : constant := 1 * 1024;

private

end Acton.Scheduler_Agents.FIFO_Within_Priorities_GNATBB;
