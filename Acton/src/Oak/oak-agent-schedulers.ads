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

with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;
with Oak.Agent.Storage;
with Oak.Timers;          use Oak.Timers;

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package Oak.Agent.Schedulers with Preelaborate is

   ----------------
   -- Subprograms --
   -----------------

   function Agent_To_Run (Agent : in Scheduler_Id) return Oak_Agent_Id;
   --  Get the Agent that the Scheduler Agent has nominated to run.

   function Highest_Resposible_Priority
     (Agent : in Scheduler_Id)
      return Any_Priority;
   --  Returns the highest priority that the Scheduler Agent is responsible
   --  for.

   function Lowest_Resposible_Priority
     (Agent : in Scheduler_Id)
      return Any_Priority;
   --  Returns the lowest priority that the Scheduler Agent is responsible for.

   procedure New_Scheduler_Agent
     (Agent                : out Scheduler_Id;
      Name                 : in  String;
      Call_Stack_Size      : in  Storage_Count;
      Run_Loop             : in  Address;
      Lowest_Priority      : in  Any_Priority;
      Highest_Priority     : in  Any_Priority;
      When_To_Charge_Agent : in  Charge_Occurrence := All_Priorities);
   --  Creates a new Scheduler Agent with the given parameters. Allocates the
   --  the storage for the Scheduler Agent data structure and any dependents.

   procedure Set_Agent_To_Run
     (For_Agent    : in Scheduler_Id;
      Agent_To_Run : in Oak_Agent_Id);
   --  Stores the Agent that the Scheduler Agent's has nominated to run.

   procedure Set_Priority_Range
     (Agent : in Scheduler_Id;
      From  : in Any_Priority;
      To    : in Any_Priority);
   --  Stores the priority range that the Scheduler Agent is responisble for.

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
   --  Scheduler Agent Components

      Lowest_Priority        : Any_Priority;
      --  The lowest priority that the Agent is responsible for.

      Highest_Priority       : Any_Priority;
      --  The highest priority that the Agent is responsible for.

      Agent_To_Run           : Oak_Agent_Id;
      --  The agent selected to run by this Scheduler Agent.

      Run_Timer              : Oak_Timer_Id;
      --  An Oak Timer is used to wake the Scheduler Agent when it needs to
      --  manage its queues.
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

   function Highest_Resposible_Priority
     (Agent : in Scheduler_Id)
      return Any_Priority is (Agent_Pool (Agent).Highest_Priority);

   function Lowest_Resposible_Priority
     (Agent : in Scheduler_Id)
      return Any_Priority is (Agent_Pool (Agent).Lowest_Priority);

   function Timer_For_Scheduler_Agent
     (Agent : in Scheduler_Id)
      return Oak_Timer_Id
      is (Agent_Pool (Agent).Run_Timer);
end Oak.Agent.Schedulers;
