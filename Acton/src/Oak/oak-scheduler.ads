------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                              OAK.SCHEDULER                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Agent;            use Oak.Agent;
with Oak.Message;          use Oak.Message;

with System;               use System;
with System.Multiprocessors;

package Oak.Scheduler with Preelaborate is

   -----------------
   -- Subprograms --
   -----------------

   procedure Add_Agent_To_Scheduler (Agent : in Oak_Agent_Id);
   --  Adds the specified Agent to its scheduler agent (which the Agent
   --  contains a reference to).

   procedure Check_Sechduler_Agents_For_Next_Agent_To_Run
     (Next_Agent_To_Run : out Oak_Agent_Id);
   --  Queries the system's scheduler agents for the next task to run. Does not
   --  run the scheduler agents themselves, instead it relies on the cached
   --  results of the last run. Checks the scheduler agents for the next agent
   --  to run from the specified agent onwards. If the head of the Scheduler
   --  Table is provided, then the search begins with the highest priority
   --  scheduler agent.

   function Find_Scheduler_For_System_Priority
     (Priority : Any_Priority;
      CPU      : System.Multiprocessors.CPU_Range)
      return Scheduler_Id_With_No;
   --  Finds the scheduler agent responsible for the givin Ada priority.

   procedure Inform_Scheduler_Agent_Has_Changed_State
     (Changed_Agent     : in  Oak_Agent_Id;
      Next_Agent_To_Run : out Oak_Agent_Id);
   --  Notifies the scheduler responsible for the given task that the task has
   --  changed state.

   procedure New_Scheduler_Cycle
     (Scheduler         : in  Scheduler_Id;
      Next_Agent_To_Run : out Oak_Agent_Id);
   --  Starts a new cycle for the scheduler agent.

   procedure Remove_Agent_From_Scheduler (Agent : in Oak_Agent_Id);
   --  Removes the agent from its scheduler.

   procedure Run_The_Bloody_Scheduler_Agent_That_Wanted_To_Be_Woken
     (Scheduler         : in  Scheduler_Id;
      Current_Agent     : in  Oak_Agent_Id;
      Next_Agent_To_Run : out Oak_Agent_Id);
   --  Runs the scheduler that requested to be run through a Scheduler Timer.

   procedure Switch_To_Scheduler_Agent
     (Scheduler_Agent : in     Scheduler_Id;
      Message         : in out Oak_Message);
   --  Switch to the specified scheduler agent.

private

   procedure Run_Scheduler_Agent
     (Agent  : in Scheduler_Id;
      Reason : in Oak_Message);
   --  Run the specified scheduler agent, passing it the given Oak_Message.

   procedure Run_Scheduler_Agent
     (Agent             : in  Scheduler_Id;
      Reason            : in  Oak_Message;
      Next_Agent_To_Run : out Oak_Agent_Id);
   --  Like above, except it returns the next agent to run as a result of
   --  runing the scheduler agent.

end Oak.Scheduler;
