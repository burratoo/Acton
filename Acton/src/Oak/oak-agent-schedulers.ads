------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                           OAK.AGENT.SCHEDULERS                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package provides Oak's Scheduler Agents, the agents that that provide
--  Oak's scheduling services. The Agent extends the Oak Agent data structure
--  to include scheduler related components to allow Oak to interact and manage
--  these Agents and the scheduling services they provide.

with Oak.Agent.Storage;

with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;
with Oak.Oak_Time;        use Oak.Oak_Time;
with Oak.Timers;          use Oak.Timers;

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package Oak.Agent.Schedulers with Preelaborate is

   -----------
   -- Types --
   -----------

   type Active_Till is (Always_Active, Deadline, Budget_Exhaustion);
   --  A type to represent whether the scheduler agent runs until its deadline
   --  or budget expires.

   type No_Agent_Interpretation is (As_Is, Sleep_Agent);
   --  The No_Agent can mean No_Agent or Sleep_Agent. Generally this does not
   --  cause any issue except for when a scheduler agent wants to explicitly
   --  schedule the sleep agent (for example a polling server)

   -----------------
   -- Subprograms --
   -----------------

   function Agent_To_Run (Agent : in Scheduler_Id) return Oak_Agent_Id;
   --  Get the Agent that the Scheduler Agent has nominated to run.

   function Charge_While_No_Agent (Agent : in Scheduler_Id) return Boolean;
   --  Returns true if the scheduler agent remains in the charge list while the
   --  agent No_Agent is selected.

   function Highest_Resposible_Priority
     (Agent : in Scheduler_Id)
      return Any_Priority;
   --  Returns the highest priority that the Scheduler Agent is responsible
   --  for.

   function Interpret_No_Agent_As (Agent : in Scheduler_Id)
                                   return No_Agent_Interpretation;
   --  Returns how a No_Agent Agent_To_Run value is to be interpretated, either
   --  as a No_Agent or as a Sleep_Agent.

   function Is_Scheduler_Active (Scheduler : in Scheduler_Id) return Boolean;
   --  Returns true if the scheduler agent is active and is able to dispatch
   --  tasks.

   function Lowest_Resposible_Priority
     (Agent : in Scheduler_Id)
      return Any_Priority;
   --  Returns the lowest priority that the Scheduler Agent is responsible for.

   procedure New_Scheduler_Agent
     (Agent                 : out Scheduler_Id;
      Name                  : in  String;
      Call_Stack_Size       : in  Storage_Count;
      Run_Loop              : in  Address;
      Lowest_Priority       : in  Any_Priority;
      Highest_Priority      : in  Any_Priority;
      Scheduler_Agent       : in  Scheduler_Id_With_No := No_Agent;
      When_To_Charge_Agent  : in  Charge_Occurrence := Only_While_Running;
      Interpret_No_Agent_As : in  No_Agent_Interpretation := As_Is;
      Charge_While_No_Agent : in  Boolean := False;
      Agent_Active_Till     : in  Active_Till := Always_Active;
      Cycle_Period          : in  Oak_Time.Time_Span := Time_Span_Last;
      Cycle_Phase           : in  Oak_Time.Time_Span := Time_Span_Zero;
      Relative_Deadline     : in  Oak_Time.Time_Span := Time_Span_Last;
      Execution_Budget      : in  Oak_Time.Time_Span := Time_Span_Last);
   --  Creates a new Scheduler Agent with the given parameters. Allocates the
   --  the storage for the Scheduler Agent data structure and any dependents.

   procedure Set_Agent_To_Run
     (For_Agent    : in Scheduler_Id;
      Agent_To_Run : in Oak_Agent_Id);
   --  Stores the Agent that the Scheduler Agent's has nominated to run.

   procedure Set_Next_Cycle_Start_Time
     (Scheduler  : in Scheduler_Id;
      Start_Time : in Oak_Time.Time);
   --  Sets the time that the next cycle will commmence.

   procedure Set_Priority_Range
     (Agent : in Scheduler_Id;
      From  : in Any_Priority;
      To    : in Any_Priority);
   --  Stores the priority range that the Scheduler Agent is responisble for.

   procedure Set_Is_Scheduler_Active
     (Scheduler : in Scheduler_Id;
      Active    : in Boolean);
   --  Sets whether the scheduler agent is active and is able to dispatch
   --  tasks or not.

   function Scheduler_Active_Till
     (Scheduler : in Scheduler_Id) return Active_Till;
   --  Returns the event that causes the scheduler agent to inactivate and go
   --  to sleep.

   function Scheduler_Cycle_Period
     (Scheduler : in Scheduler_Id) return Oak_Time.Time_Span;
   --  The cycle period of the scheduler.

   function Scheduler_Cycle_Phase
     (Scheduler : in Scheduler_Id) return Oak_Time.Time_Span;
   --  The cycle phase of the scheduler.

   function Scheduler_Relative_Deadline
     (Scheduler : in Scheduler_Id) return Oak_Time.Time_Span;
   --  The cycle relative deadline of the scheduler agent.

   function Scheduler_Execution_Budget
     (Scheduler : in Scheduler_Id) return Oak_Time.Time_Span;
   --  The cycle execution budget of the scheduler agent.

   function Time_Next_Cycle_Commences (Agent : in Scheduler_Id)
                                       return Oak_Time.Time;
   --  The time that the scheduler agent's next cycle is due to commence.

   function Timer_For_Scheduler_Agent
     (Agent : in Scheduler_Id)
      return Oak_Timer_Id;
   --  Returns the timer associated with the Scheduler Agent, used to run the
   --  agent when it needs to manage it queues and another agent is running.

private

   -------------------
   -- Private Types --
   -------------------

   type Scheduler_Agent_Record is record
      --  Core Scheduler Agent Components

      Lowest_Priority   : Any_Priority;
      --  The lowest priority that the Agent is responsible for.

      Highest_Priority  : Any_Priority;
      --  The highest priority that the Agent is responsible for.

      Agent_To_Run      : Oak_Agent_Id;
      --  The agent selected to run by this Scheduler Agent.

      Run_Timer         : Oak_Timer_Id;
      --  An Oak Timer is used to wake the Scheduler Agent when it needs to
      --  manage its queues.

      --  The following is only used for nested scheduler agents

      Interpret_No_Agent_As : No_Agent_Interpretation;
      --  When the scheduler agent returns No_Agent, what it is interpreted as
      --  (either as a No_Agent or as the Sleep_Agent).

      Charge_While_No_Agent : Boolean;
      --  Does the scheduler agent's budget get charged while No_Agent is
      --  selected by the agent.

      Agent_Active_Till : Active_Till;
      --  Whether the scheduler agent runs till the expiry of its deadline or
      --  budget. This allows the user to allow the scheduler agent to run
      --  against actual processor usage or the wall clock.

      Scheduler_Active  : Boolean;
      --  Specifies whether the agent is active – that is able to dispatch
      --  agents – or not.

      Next_Run_Cycle    : Oak_Time.Time;
      --  The time of the next run cycle is meant to commence for periodic
      --  tasks or the earliest time a spfor the scheduler agent.

      Period            : Oak_Time.Time_Span;
      --  The period of which the scheduler agent run at.

      Phase             : Oak_Time.Time_Span;
      --  The phase of the scheduler agent.

      Relative_Deadline : Oak_Time.Time_Span;
      --  The relative deadline of the scheduler agent.

      Execution_Budget  : Oak_Time.Time_Span;
      --  The amount of processor time the agent may execute for in a given
      --  cycle.

   end record;

   -----------------------------
   -- Scheduler Agent Storage --
   -----------------------------

   package Scheduler_Pool is new Oak.Agent.Storage
     (Agent_Record_Type => Scheduler_Agent_Record,
      Agent_Id_Type     => Scheduler_Id);

   use Scheduler_Pool;

   --------------------------
   -- Function Expressions --
   --------------------------

   function Agent_To_Run (Agent : in Scheduler_Id) return Oak_Agent_Id is
     (Agent_Pool (Agent).Agent_To_Run);

   function Charge_While_No_Agent (Agent : in Scheduler_Id) return Boolean is
      (Agent_Pool (Agent).Charge_While_No_Agent);

   function Highest_Resposible_Priority
     (Agent : in Scheduler_Id)
      return Any_Priority is (Agent_Pool (Agent).Highest_Priority);

   function Interpret_No_Agent_As
     (Agent : in Scheduler_Id)
      return No_Agent_Interpretation is
     (Agent_Pool (Agent).Interpret_No_Agent_As);

   function Is_Scheduler_Active (Scheduler : in Scheduler_Id) return Boolean is
     (Agent_Pool (Scheduler).Scheduler_Active);

   function Lowest_Resposible_Priority
     (Agent : in Scheduler_Id)
      return Any_Priority is (Agent_Pool (Agent).Lowest_Priority);

   function Scheduler_Active_Till
     (Scheduler : in Scheduler_Id) return Active_Till is
     (Agent_Pool (Scheduler).Agent_Active_Till);

   function Scheduler_Cycle_Period
     (Scheduler : in Scheduler_Id) return Oak_Time.Time_Span is
     (Agent_Pool (Scheduler).Period);

   function Scheduler_Cycle_Phase
     (Scheduler : in Scheduler_Id) return Oak_Time.Time_Span is
      (Agent_Pool (Scheduler).Phase);

   function Scheduler_Relative_Deadline
     (Scheduler : in Scheduler_Id) return Oak_Time.Time_Span is
     (Agent_Pool (Scheduler).Relative_Deadline);

   function Scheduler_Execution_Budget
     (Scheduler : in Scheduler_Id) return Oak_Time.Time_Span is
     (Agent_Pool (Scheduler).Execution_Budget);

   function Time_Next_Cycle_Commences
     (Agent : in Scheduler_Id)
      return Oak_Time.Time is
      (Agent_Pool (Agent).Next_Run_Cycle);

   function Timer_For_Scheduler_Agent
     (Agent : in Scheduler_Id)
      return Oak_Timer_Id
      is (Agent_Pool (Agent).Run_Timer);
end Oak.Agent.Schedulers;
