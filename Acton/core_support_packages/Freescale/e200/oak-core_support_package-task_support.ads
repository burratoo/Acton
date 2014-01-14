------------------------------------------------------------------------------
--                                                                          --
--                         OAK CORE SUPPORT PACKAGE                         --
--                              FREESCALE e200                              --
--                                                                          --
--                  OAK.CORE_SUPPORT_PACKAGE.TASK_SUPPORT                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--
--  Core Support Package for the e200z6 PowerPC Core.
--

with Oak.Oak_Time;
with ISA.Power.e200.Processor_Control_Registers;
with ISA;

package Oak.Core_Support_Package.Task_Support with Preelaborate is

   use ISA.Power.e200.Processor_Control_Registers;
   use ISA;

   --  This procedure saves the Kernel's registers to its respective store,
   --  loads in the task's registers, enables Oak's wakeup call and then
   --  switches the Instruction Register. On Power Architectures, we'll just
   --  place the instruction pointer and its stack pointer into a SPR. All this
   --  has to be in one function as we do not want to touch the stack as we
   --  carry out these steps. They all have to be done using assembly
   --  instructions as well. Store and load the registers from onto their
   --  respective task's or kernel's stack. If we are using a processor without
   --  SPRs we have to look up the kernel's SP either through a fixed memory
   --  address or reserve a register, the latter being unlikely.

   --  In the future we may want to speed things up by creating specialised
   --  context switch functions for switching between the kernel and the
   --  scheduler agents. The scheduler agents do not need to store all their
   --  registers on the call stack when they are switched out of context, as
   --  they only loose context once they have finished their one of their
   --  functions. Since this involves calling a Parameterless function the only
   --  registers to store is the Stack Pointer. Even then, this could just be
   --  set up by the kernel before the context switch... mmmm........

   procedure Initialise_Task_Enviroment;

   procedure Context_Switch_To_Agent with Inline => False;
   procedure Context_Switch_To_Kernel with Inline_Always;

   procedure Quick_Switch with Inline_Always;
   --  Performs a quick switch to the current agent. This causes only the
   --  stack, instruction and link return pointers to be switched. Ensure that
   --  this procedure is inlined.

   procedure Yield_Processor_To_Kernel with Inline => False;

   procedure Set_Oak_Wake_Up_Timer (Wake_Up_At : Oak.Oak_Time.Time);

   procedure Sleep_Agent;

   Agent_MSR : constant Machine_State_Register_Type :=
                 (Computation_Mode           => Mode_32,
                  Interrupt_Computation_Mode => Mode_32,
                  User_Mode_Cache_Lock       => Disable,
                  Signal_Processing          => Enable,
                  Wait_State                 => Disable,
                  Critical_Interrupts        => Disable,
                  External_Interrupts        => Enable,
                  Processor_Mode             => Supervisor,
                  Floating_Point             => Not_Available,
                  Machine_Check              => Disable,
                  FP_Exception_Mode_0        => False,
                  Debug_Interrupt            => Disable,
                  FP_Exception_Mode_1        => False,
                  Instruction_Address_Space  => 0,
                  Data_Address_Space         => 0,
                  Performance_Monitor        => Disable);

   Oak_MSR : constant Machine_State_Register_Type :=
                 (Computation_Mode           => Mode_32,
                  Interrupt_Computation_Mode => Mode_32,
                  User_Mode_Cache_Lock       => Disable,
                  Signal_Processing          => Enable,
                  Wait_State                 => Disable,
                  Critical_Interrupts        => Disable,
                  External_Interrupts        => Disable,
                  Processor_Mode             => Supervisor,
                  Floating_Point             => Not_Available,
                  Machine_Check              => Disable,
                  FP_Exception_Mode_0        => False,
                  Debug_Interrupt            => Disable,
                  FP_Exception_Mode_1        => False,
                  Instruction_Address_Space  => 0,
                  Data_Address_Space         => 0,
                  Performance_Monitor        => Disable);
end Oak.Core_Support_Package.Task_Support;
