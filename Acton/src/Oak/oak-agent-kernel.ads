------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                             OAK.AGENT.KERNEL                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2014-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package provides Oak's Kernel Agents, the agent that represents
--  an instance of Oak. The Agent extends the Oak Agent data structure to
--  include kernel specific components.

with Oak.Agent.Storage;

with Oak.Oak_Time;  use Oak.Oak_Time;
with Oak.Scheduler; use Oak.Scheduler;

with System; use System;

package Oak.Agent.Kernel with Preelaborate is

   -----------
   -- Types --
   -----------

   type Run_Reason is (First_Run, Task_Yield, Timer, External_Interrupt);
   --  The reason why Oak was run.

   -----------------
   -- Subprograms --
   -----------------

   procedure Add_Agent_To_Charge_List
     (Oak_Kernel : in Kernel_Id;
      Agent      : in Oak_Agent_Id);
   --  Add the specified agent to the kernel's charge list.

   function Current_Agent (Oak_Kernel : in Kernel_Id) return Oak_Agent_Id;
   --  Return the currently selected agent for the kernel instance.

   function Find_Top_Active_Interrupt
     (Oak_Kernel : in Kernel_Id)
      return Interrupt_Id_With_No;
   --  Returns the top active interrupt agent that is handling an interrupt.

   procedure New_Kernel_Agent
     (Agent  : out Kernel_Id);
   --  Creates a new Kernel Agent with the given prameters. Allocates the
   --  storage for the Kernel Agent data structure and any dependents.

   function Reason_For_Run (Oak_Kernel : in Kernel_Id) return Run_Reason;
   --  The reason for the kernel running.

   procedure Remove_Agent_From_Charge_List
     (Oak_Kernel : in Kernel_Id;
      Agent      : in Oak_Agent_Id);
   --  Remove the specified agent from the kernel's charge list.

   procedure Set_Current_Agent
     (Oak_Kernel : in Kernel_Id;
      Agent      : in Oak_Agent_Id);
   --  Set the currently active agent.

   procedure Set_Reason_For_Run
     (Oak_Kernel : in Kernel_Id;
      Reason     : in Run_Reason);
   --  Set the reason for running the kernel agent.

private

   -------------------
   -- Private Types --
   -------------------

   type Interrupt_State is (Inactive, Handling);
   --  The active state of an interrupt agent.

   type Interrupt_Active_Set is array (Interrupt_Priority) of Interrupt_State
     with Pack;
   --  A type that holds the state of the interrupt agents used by the kernel.
   --  This is kept here in a bitfield rather than the inside each interrupt
   --  agent to allow for a quick scan through the packed array to find any
   --  active interrupts.

   type Interrupt_Ids is array (Interrupt_Priority) of Interrupt_Id;
   --  An array of ids belonging to Interrupt Agents that are responsible for
   --  handling interrupt for the System.Interrrupt_Priority range.

   type Oak_Kernel_Record is record
      Schedulers         : Scheduler_Table;
      --  The top-level scheduler agents assigned to the kernel. Forms the
      --  kernel's task priority space.

      Reason_For_Run     : Run_Reason;
      --  The reason why the kernel has been run.

      Current_Agent      : Oak_Agent_Id;
      --  The currently selected agent (that will run or has run).

      Entry_Exit_Stamp   : Oak_Time.Time;
      --  The time the kernel entered/exited its run loop.

      Interrupt_Agents   : Interrupt_Ids;
      --  An array of interrupt agents that handle interrupts, with an
      --  interrupt agent assigned for each Interrupt_Priority.

      Interrupt_States   : Interrupt_Active_Set;
      --  A set that describes whether an interrupt agent is handling an
      --  interrupt or not. Described here in a bit array to make it easier to
      --  search for active interrupts.

      Budgets_To_Charge  : Charge_List;
      --  A list of agent budgets to charge for the preceding use of the
      --  processor.
   end record;

   --------------------------
   -- Kernel Agent Storage --
   --------------------------

   package Kernel_Pool is new Oak.Agent.Storage
     (Agent_Record_Type => Oak_Kernel_Record,
      Agent_Id_Type     => Kernel_Id);

   use Kernel_Pool;

   --------------------------
   -- Function Expressions --
   --------------------------

   function Current_Agent (Oak_Kernel : in Kernel_Id) return Oak_Agent_Id is
     (Agent_Pool (Oak_Kernel).Current_Agent);

   function Reason_For_Run (Oak_Kernel : in Kernel_Id) return Run_Reason is
      (Agent_Pool (Oak_Kernel).Reason_For_Run);
end Oak.Agent.Kernel;
