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

   procedure Add_Agents_To_Scheduler (Agents : in Oak_Agent_Id);
   --  Adds a list of Agents to their respective scheduler agents. List is
   --  linked through agents' Next_Agent link.

   procedure Check_Sechduler_Agents_For_Next_Agent_To_Run
     (Next_Agent_To_Run : out Oak_Agent_Id;
      Top_Priority      : out Any_Priority);
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
     (Changed_Agent : in Oak_Agent_Id);
   --  Notifies the scheduler responsible for the given task that the task has
   --  changed state.

   procedure New_Scheduler_Cycle (Scheduler : in Scheduler_Id);
   --  Starts a new cycle for the scheduler agent.

   procedure Post_Run_Scheduler_Agent
     (Agent   : in Scheduler_Id;
      Message : in Oak_Message);
   --  Perform kernel-level scheduler operations as a result of running a
   --  scheduler agent.

   procedure Remove_Agent_From_Scheduler (Agent : in Oak_Agent_Id);
   --  Removes the agent from its scheduler.

   procedure Service_Scheduler_Agent_Timer
     (Scheduler : in Scheduler_Id);
   --  Runs the scheduler that requested to be run through a Scheduler Timer.

end Oak.Scheduler;
