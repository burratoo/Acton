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

with Oak.Message;   use Oak.Message;
with Oak.Oak_Time;  use Oak.Oak_Time;
with Oak.Timers;    use Oak.Timers;

with System; use System;

package Oak.Agent.Kernel with Preelaborate is

   -----------------
   -- Subprograms --
   -----------------

   procedure Activate_Interrupt_Agent
     (Oak_Kernel : in Kernel_Id;
      Interrupt  : in Interrupt_Id) with Inline;
   --  Sets the interrupt agent with the given interrupt id to be active.

   procedure Add_Agent_To_Charge_List
     (Oak_Kernel : in Kernel_Id;
      Agent      : in Oak_Agent_Id);
   --  Add the specified agent to the kernel's charge list.

   procedure Add_Scheduler_To_Scheduler_Table
     (Oak_Kernel : in Kernel_Id;
      Scheduler  : in Scheduler_Id);
   --  Add the scheduler to the kernel's scheduler agent table.

   function Charge_List (Oak_Kernel : in Kernel_Id) return Charge_List_Head;
   --  Return the head of the charge list.

   function Current_Agent (Oak_Kernel : in Kernel_Id) return Oak_Agent_Id
     with Inline_Always;
   --  Return the currently selected agent for the kernel instance. This needs
   --  to be inlined since it is called from within interrupt handlers where we
   --  want to avoid calling subprograms as it messes with the agent's stack.

   function Current_Timer (Oak_Kernel : in Kernel_Id) return Oak_Timer_Id;
   --  Return the currently active timer.

   function Current_Priority (Oak_Kernel : in Kernel_Id) return Any_Priority;
   --  Returns the current priority that the kernel is running at.

   procedure Deactivate_Interrupt_Agent
     (Oak_Kernel : in Kernel_Id;
      Interrupt  : in Interrupt_Id) with Inline;
   --  Sets the interrupt agent with the given interrupt id to be inactive.

   function Entry_Exit_Stamp (Oak_Kernel : in Kernel_Id) return Oak_Time.Time;
   --  Returns the current Entry/Exit stamp for the kernel.

   function Find_Top_Active_Interrupt
     (Oak_Kernel : in Kernel_Id)
      return Interrupt_Id_With_No;
   --  Returns the top active interrupt agent that is handling an interrupt.

   procedure Flush_Scheduler_Ops_Stack (Oak_Kernel : in Kernel_Id);

   function Has_Scheduler_Operations_Pending
     (Oak_Kernel : in Kernel_Id)
      return Boolean;
   --  Returns true if there are Scheduler operations on the scheduler ops
   --  stack.

   function Interrupt_For_Priority
     (Oak_Kernel : in Kernel_Id;
      Priority   : in Interrupt_Priority)
     return Interrupt_Id;
   --  Returns the Id of the interrupt agent responsible for handling
   --  interrupts at the given priority.

   procedure Pull_Scheduler_Op
     (Oak_Kernel : in  Kernel_Id;
      Scheduler  : out Scheduler_Id;
      Operation  : out Oak_Message);
   --  Pop a scheduler operation onto the kernel's scheduler operation stack.

   procedure Push_Scheduler_Op
     (Oak_Kernel : in Kernel_Id;
      Scheduler  : in Scheduler_Id;
      Operation  : in Oak_Message);
   --  Push a scheduler operation onto the kernel's scheduler operation stack.

   procedure New_Kernel_Agent
     (Agent  : out Kernel_Id);
   --  Creates a new Kernel Agent with the given prameters. Allocates the
   --  storage for the Kernel Agent data structure and any dependents.

   procedure Remove_Agent_From_Charge_List
     (Oak_Kernel : in Kernel_Id;
      Agent      : in Oak_Agent_Id);
   --  Remove the specified agent from the kernel's charge list.

   function Top_Level_Schedulers
     (Oak_Kernel : in Kernel_Id)
      return Scheduler_Id_With_No;
   --  Returns the scheduler agent table.

   procedure Set_Current_Agent
     (Oak_Kernel : in Kernel_Id;
      Agent      : in Oak_Agent_Id);
   --  Set the currently active agent.

   procedure Set_Current_Priority
     (Oak_Kernel : in Kernel_Id;
      Priority   : in Any_Priority);
   --  Sets the current priority that the kernel is running at.

   procedure Set_Current_Timer
     (Oak_Kernel : in Kernel_Id;
      Timer      : in Oak_Timer_Id);
   --  Set the currently active timer.

   procedure Set_Entry_Exit_Stamp
     (Oak_Kernel : in Kernel_Id;
      Time       : in Oak_Time.Time);
   --  Set the time for the kernel's Entry/Exit time stamp.

   --  Protected object handling code

   procedure Add_Protected_Agent_To_Kernel
     (Oak_Kernel : in Kernel_Id;
      Agent      : in Protected_Id);

   procedure Remove_Protected_Agent_From_Kernel
     (Oak_Kernel : in Kernel_Id;
      Agent      : in Protected_Id);

   function Next_Protected_Agent_To_Run
     (Oak_Kernel : in Kernel_Id) return Protected_Id_With_No;

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

   type Scheduler_Op_Record is record
      Scheduler_Agent : Scheduler_Id_With_No;
      Operation       : Oak_Message;
   end record;

   type Scheduler_Op_Id is mod 2;

   type Scheduler_Ops_Stack is array (Scheduler_Op_Id) of Scheduler_Op_Record;

   type Oak_Kernel_Record is record
      Current_Agent      : Oak_Agent_Id;
      --  The currently selected agent (that will run or has run).

      Current_Priority   : Any_Priority;
      --  The current priority the system is operating at. This may be of a
      --  different priority

      Current_Timer      : Oak_Timer_Id;
      --  The current active timer.

      Schedulers         : Scheduler_Id_With_No;
      --  The top-level scheduler agents assigned to the kernel. Forms the
      --  kernel's task priority space.

      Entry_Exit_Stamp   : Oak_Time.Time;
      --  The time the kernel entered/exited its run loop.

      Active_Protected_Agents : Protected_Id_With_No;
      --  List of active protected agents sorted by priority.

      Interrupt_Agents   : Interrupt_Ids;
      --  An array of interrupt agents that handle interrupts, with an
      --  interrupt agent assigned for each Interrupt_Priority.

      Interrupt_States   : Interrupt_Active_Set;
      --  A set that describes whether an interrupt agent is handling an
      --  interrupt or not. Described here in a bit array to make it easier to
      --  search for active interrupts.

      Budgets_To_Charge  : Charge_List_Head;
      --  A list of agent budgets to charge for the preceding use of the
      --  processor.

      Scheduler_Ops      : Scheduler_Ops_Stack;
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

   function Charge_List (Oak_Kernel : in Kernel_Id) return Charge_List_Head is
     (Agent_Pool (Oak_Kernel).Budgets_To_Charge);

   function Current_Agent (Oak_Kernel : in Kernel_Id) return Oak_Agent_Id is
     (Agent_Pool (Oak_Kernel).Current_Agent);

   function Current_Priority (Oak_Kernel : in Kernel_Id) return Any_Priority is
      (Agent_Pool (Oak_Kernel).Current_Priority);

   function Current_Timer (Oak_Kernel : in Kernel_Id) return Oak_Timer_Id is
     (Agent_Pool (Oak_Kernel).Current_Timer);

   function Entry_Exit_Stamp (Oak_Kernel : in Kernel_Id)
                              return Oak_Time.Time is
      (Agent_Pool (Oak_Kernel).Entry_Exit_Stamp);

   function Has_Scheduler_Operations_Pending
     (Oak_Kernel : in Kernel_Id)
      return Boolean is
     (Agent_Pool (Oak_Kernel).Scheduler_Ops
      (Scheduler_Op_Id'First).Scheduler_Agent /= No_Agent or
          Agent_Pool (Oak_Kernel).Scheduler_Ops
      (Scheduler_Op_Id'Last).Scheduler_Agent /= No_Agent);

   function Interrupt_For_Priority
     (Oak_Kernel : in Kernel_Id;
      Priority   : in Interrupt_Priority)
      return Interrupt_Id is
      (Agent_Pool (Oak_Kernel).Interrupt_Agents (Priority));

   function Top_Level_Schedulers
     (Oak_Kernel : in Kernel_Id)
      return Scheduler_Id_With_No is
      (Agent_Pool (Oak_Kernel).Schedulers);
end Oak.Agent.Kernel;
