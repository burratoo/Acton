with Oak.Agent.Scheduler; use Oak.Agent.Scheduler;
with System;              use System;

package Acton.Scheduler_Agents.FIFO_Within_Priorities with Preelaborate is

   procedure Create_Agent
     (Agent        : in out Scheduler_Agent'Class;
      Min_Priority : in Any_Priority;
      Max_Priority : in Any_Priority);
   pragma Export
     (Ada,
      Create_Agent,
      "__acton_scheduler_agent_fifo_within_priorities");

   procedure Run_Loop;
   procedure Remove_Task;
   procedure Change_Task_Priority;

   Stack_Size : constant := 1 * 1024;
   Agent_Name : constant String := "Fixed_Priority_Scheduler";

end Acton.Scheduler_Agents.FIFO_Within_Priorities;
