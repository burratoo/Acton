------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                           OAK.AGENT.INTERRUPTS                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2013-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package provides Oak's Interrupt Agents, the agent that executes
--  interrupt handlers in Oak. An Interrupt Agent exists for each interrupt
--  priority. The Agent extends the Oak Agent data structure to include
--  interrupt specific components.

with Oak.Agent.Storage;

with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;

with Oak.Timers; use Oak.Timers;
with System;     use System;

package Oak.Agent.Interrupts with Preelaborate is

   -----------
   -- Types --
   -----------

   type Interrupt_Type is (External, Timer_Action);
   --  An interrupt agent runs handlers two form of interrupts: External
   --  interrupts that come from an interrupt controller (or on a simpler µC
   --  the processor) and the handler response for Timers.

   ----------------
   -- Subprograms --
   -----------------

   procedure Interrupt_Done
     (Kernel        : in Kernel_Id;
      Current_Agent : in Interrupt_Id);
   --  Handles an interrupt agent's completion, including the servicing of any
   --  open entries.

   function Interrupt_Kind (Agent : in Interrupt_Id) return Interrupt_Type;
   --  Returns the type of interrupt the Agent is handling.

   procedure New_Interrupt_Agent
     (Agent    : out Interrupt_Id;
      Priority : in Oak_Priority);
   --  Creates a new Interrupt Agent with the given parameters. Allocates the
   --  storage for the Task Agent data structure and any dependents.

   procedure Set_External_Id
     (For_Agent : in Interrupt_Id;
      Id        : in External_Interrupt_Id);
   --  Sets the external id of the external interrupt that the Agent will
   --  handle.

   procedure Set_Interrupt_Kind
     (For_Agent : in Interrupt_Id;
      Kind      : in Interrupt_Type);
   --  Sets the kind of interrupt that the Agent will handle. See the
   --  Interrupt_Type for the kinds of interrupts that the Agent can handle.

   procedure Set_Timer_To_Handle
     (Agent : in Interrupt_Id;
      Timer : in Oak_Timer_Id);
   --  If the Agent will run the handler of a timer, it is specified here.

   function Timer_To_Handle (Agent : in Interrupt_Id)
                             return Oak_Timer_Id;
   --  Return the Timer that will be handled by the Agent.

private

   -------------------
   -- Private Types --
   -------------------

   type Interrupt_Agent_Record is record
   --  Interrupt Agent components

      Interrupt_Kind  : Interrupt_Type;
      --  The type of interrupt that will be handled. There are different
      --  handling for different type of interrupts. See the type for more
      --  details.

      External_Id     : External_Interrupt_Id;
      --  The id associated with a given external interrupt. External
      --  interrupts come from outside the processor core and are tagged with
      --  an id to identify the source.

      Timer_To_Handle : Oak_Timer_Id;
      --  The timer that has an associated handler that needs running.
   end record;

   -----------------------------
   -- Interrupt Agent Storage --
   -----------------------------

   package Interrupt_Pool is new Oak.Agent.Storage
     (Agent_Record_Type => Interrupt_Agent_Record,
      Agent_Id_Type     => Interrupt_Id);

   use Interrupt_Pool;

   -------------------------
   -- Private Subprograms --
   -------------------------

   procedure Interrupt_Run_Loop;
   --  The run loop of the interrupt agent.

   --------------------------
   -- Function Expressions --
   --------------------------

   function Interrupt_Kind (Agent : in Interrupt_Id)
     return Interrupt_Type is (Agent_Pool (Agent).Interrupt_Kind);

   function Timer_To_Handle (Agent : in Interrupt_Id)
     return Oak_Timer_Id is (Agent_Pool (Agent).Timer_To_Handle);

end Oak.Agent.Interrupts;
