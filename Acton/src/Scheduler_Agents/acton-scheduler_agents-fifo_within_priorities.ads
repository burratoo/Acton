with Oak.Agent.Schedulers; use Oak.Agent.Schedulers;
with System;               use System;

with Oak.Agent;

package Acton.Scheduler_Agents.FIFO_Within_Priorities with Preelaborate is

   type FIFO_Within_Priorities (Min_Priority, Max_Priority : Any_Priority)
     is new Scheduler_Agent with private
     with Preelaborable_Initialization;

   procedure Initialise_Scheduler_Agent
     (Agent : in out FIFO_Within_Priorities);

   procedure Run_Loop (Self : in out FIFO_Within_Priorities) with No_Return;

   Stack_Size : constant := 1 * 1024;
   Agent_Name : constant String := "Fixed_Priority_Scheduler";

private
   type Agent_Array is
     array (System.Any_Priority range <>) of access
     Oak.Agent.Oak_Agent'Class;

   type FIFO_Within_Priorities (Min_Priority, Max_Priority : Any_Priority)
     is new Scheduler_Agent (Min_Priority, Max_Priority) with record
      Runnable_Queues : Agent_Array (Min_Priority .. Max_Priority);
      Sleeping_Queues : Agent_Array (Min_Priority .. Max_Priority);
   end record;

end Acton.Scheduler_Agents.FIFO_Within_Priorities;
