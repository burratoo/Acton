with System; use System;
with Oak.Oak_Task; use Oak.Oak_Task;

package Acton.Scheduler_Agent.FIFO_Within_Priorities is

   --  pragma Pure;

   procedure Create_Agent (Agent                      : Oak_Task_Handler;
                           Min_Priority, Max_Priority : Priority);
   pragma Export
     (Ada,
      Create_Agent,
      "__acton_scheduler_agent_fifo_within_priorities");

   procedure Run_Loop;
   procedure Remove_Task;
   procedure Change_Task_Priority;

   Stack_Size : constant :=  1 * 1024;
   Agent_Name : constant String := "Fixed_Priority_Scheduler";

end Acton.Scheduler_Agent.FIFO_Within_Priorities;
