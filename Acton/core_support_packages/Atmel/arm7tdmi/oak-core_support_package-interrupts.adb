------------------------------------------------------------------------------
--                                                                          --
--                         OAK CORE SUPPORT PACKAGE                         --
--                                                                          --
--                    OAK.CORE_SUPPORT_PACKAGE.INTERRUPTS                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with ISA;
with ISA.ARM.ARM7TDMI; use ISA.ARM.ARM7TDMI;

with Oak.Agent;           use Oak.Agent;
with Oak.Agent.Kernel;    use Oak.Agent.Kernel;
with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;

with Oak.Core;    use Oak.Core;

with Oak.Processor_Support_Package.Interrupts;
with Oak.Processor_Support_Package.Time;

with System.Machine_Code; use System.Machine_Code;

--  Note that the full context save for a non-kernel agent looks like this:
--
--  ----------- ---
--  |   r0    |
--  |    |    |
--  |   r14   | 17 x 4 bytes = 68 bytes
--  |   LR    |
--  |   SPSR  |
--  ----------- ---

--  While the kernel agent and a voluntary agent yield only stores:
--
--  -----------
--  |   SP    |
--  |   LR    |
--  |   SPSR  |
--  -----------

package body Oak.Core_Support_Package.Interrupts is

   --  Suppress all checks since they get in the way and cause unpredictable
   --  problems.

   pragma Suppress (All_Checks);

   -----------------------
   -- Set_Up_Interrupts --
   -----------------------

   procedure Set_Up_Interrupts is
      Task_Stack_Pointer : Address;
   begin
      --  Core specific interupt setup code;

      --  Set up Supervisor mode stack register (is equal to the kernel's
      --  stack pointer – or at least the value stored by Set_Stack_Pointer)

      --  Should already be in Supervisor mode

      --  Store current sp into r0. Load kernel register store address Address
      --  into sp. Using r0 since its caller save.

      Asm ("mov r0, sp", Volatile => True);

      Task_Stack_Pointer := Stack_Pointer (This_Oak_Kernel);

      Asm ("mov sp, %0",
           Inputs => Address'Asm_Input ("r", Task_Stack_Pointer),
           Volatile => True);

      --  Drop into System Mode and load r0 into sp

      Switch_To_System_Mode;
      Asm ("mov sp, r0", Volatile => True);

      --  In System Mode from here-on in

      --  Processor specific initialisation routines
      Oak.Processor_Support_Package.Interrupts.Initialise_Interrupts;
      Oak.Processor_Support_Package.Time.Initialise_Clock;
   end Set_Up_Interrupts;

   ---------------------------
   -- Decrementer_Interrupt --
   ---------------------------

   procedure Decrementer_Interrupt is
   begin
      --  Entered in FIQ Mod

      --  Push r0 onto the interrupted agent's stack and load the reason
      --  for Oak to run into r0 which Oak will later pull out.
      Asm
        ("stm sp, {r0}" & ASCII.LF & ASCII.HT &
         "mov r0, %0",
         Inputs   => Run_Reason'Asm_Input ("i", Timer),
         Volatile => True);
      Full_Context_Switch_To_Oak;
   end Decrementer_Interrupt;

   --------------------------------
   -- External_Interrupt_Handler --
   --------------------------------

   procedure External_Interrupt_Handler is
   begin
      --  Entered in IQR Mode

      --  Disable FIQ interrupts as well. Easiest on ARM4t to use a constant
      --  to do the job, confirming IRQ mode and all interrupts disabled. This
      --  is because the following instruction (the "_c" bit only modifies
      --  the interrupt flags and system flags only).

      Switch_To_IRQ_Mode;

      --  Push r0 onto the interrupted agent's stack and load the reason
      --  for Oak to run into r0 which Oak will later pull out.
      Asm
        ("stm sp, {r0}" & ASCII.LF & ASCII.HT &
           "mov r0, %0",
           Inputs   => Run_Reason'Asm_Input ("i", External_Interrupt),
         Volatile => True);
      Full_Context_Switch_To_Oak;
   end External_Interrupt_Handler;

   --------------------------------------------
   -- Full_Context_Switch_To_Agent_Interrupt --
   --------------------------------------------

   procedure Full_Context_Switch_To_Agent_Interrupt is
      Task_Stack_Pointer : Address;
   begin

      --  Entered in Supervisor Mode.

      --  Disable all interrupts. Easiest on ARM4t to use a constant
      --  to do the job, confirming Supervisor mode and all interrupts disabled

      Switch_To_Supervisor_Mode;

      --  Update SWI address

      SWI_Vector := SWI_Return_Vector;

      --  This is the spot where we store the kernel's register. Only we let
      --  most of them get trashed. Also note that SP should already be
      --  pointing to the kernel's register store.

      Asm ("stm sp, {sp}^", Volatile => True);

      --  Store the kernel's instruction pointer (currently in lr_svc) and its
      --  SPSR onto its stack

      Asm ("mrs r0, spsr", Volatile => True);
      Asm ("stm sp, {r0, lr}", Volatile => True);

      --  Store the kernel's stack pointer and load the current agent's

      Asm ("mov %0, sp",
           Outputs => Address'Asm_Output ("=r", Task_Stack_Pointer),
           Volatile => True);

      Set_Stack_Pointer (This_Oak_Kernel, Task_Stack_Pointer);
      Task_Stack_Pointer := Stack_Pointer (Current_Agent (This_Oak_Kernel));

      --  Move the task's stack pointer into sp

      Asm ("mov sp, %0",
           Inputs => Address'Asm_Input ("r", Task_Stack_Pointer),
           Volatile => True);

      --  Install sp into FIQ and IRQ modes

      --  First calculate the value sp will have after registers have been
      --  popped off. Note that there are 17 * 4 bytes on the stack. r0 is the
      --  scratch register used to hold this value.

      Asm ("add r0, sp, #68", Volatile => True);
      Switch_To_IRQ_Mode;
      Asm ("mov sp, r0",  Volatile => True);
      Switch_To_FIQ_Mode;
      Asm ("mov sp, r0",  Volatile => True);
      Switch_To_System_Mode;

      --  Restore task's registers
      Asm ("ldm sp, {r0, lr}", Volatile => True);
      Asm ("msr spsr_cxsf, r0", Volatile => True);
      Asm ("ldm sp, {r0 - lr}^", Volatile => True);

      Asm ("subs pc, r14,#4", Volatile => True);
      --  This return sequence is used since the agent was originally
      --  interrupted by an IRQ or FIQ.

   end Full_Context_Switch_To_Agent_Interrupt;

   --------------------------------
   -- Full_Context_Switch_To_Oak --
   --------------------------------

   procedure Full_Context_Switch_To_Oak is
      Task_Stack_Pointer : Address;
   begin

      --  Entered in IRQ or FIQ Mode. Do not enter using SWI.

      --  This is the spot where we store the agent's registers. Note that
      --  r1 has already been saved.

      Asm ("stm sp, {r1 - lr}^", Volatile => True);
      Asm ("mrs r0, spsr", Volatile => True);
      Asm ("stm sp, {r0, lr}", Volatile => True);

      --  Store the interrupt agent's stack pointer and load the kernel's

      Asm ("mov %0, sp",
           Outputs => Address'Asm_Output ("=r", Task_Stack_Pointer),
           Volatile => True);

      Set_Stack_Pointer (Current_Agent (This_Oak_Kernel), Task_Stack_Pointer);
      Task_Stack_Pointer := Stack_Pointer (This_Oak_Kernel);

      --  Move the task's stack pointer into sp and restore task's registers

      Asm ("mov sp, %0",
           Inputs => Address'Asm_Input ("r", Task_Stack_Pointer),
           Volatile => True);
      Asm ("ldm sp, {r0, lr}", Volatile => True);
      Asm ("msr spsr_cxsf, r0", Volatile => True);
      Asm ("ldm sp, {sp}^", Volatile => True);

      Asm ("subs pc, r14, #4", Volatile => True);
      --  This return sequence is used since the agent was originally
      --  interrupted by an IRQ or FIQ.

   end Full_Context_Switch_To_Oak;

   ------------------------------------------------
   -- In_Place_Context_Switch_To_Agent_Interrupt --
   ------------------------------------------------

   procedure In_Place_Context_Switch_To_Agent_Interrupt is
   begin
      --  Just return since there is no point doing an in-place switch on this
      --  ARM.
      Asm ("subs pc, lr", Volatile => True);
   end In_Place_Context_Switch_To_Agent_Interrupt;

   ----------------------------------------------
   -- In_Place_Context_Switch_To_Oak_Interrupt --
   ----------------------------------------------

   procedure In_Place_Context_Switch_To_Oak_Interrupt is
   begin
      --  See above.
      Asm ("subs pc, lr", Volatile => True);
   end In_Place_Context_Switch_To_Oak_Interrupt;

   -----------------------------------------------
   -- Request_Context_Switch_To_Agent_Interrupt --
   -----------------------------------------------

   procedure Request_Context_Switch_To_Agent_Interrupt is
      Task_Stack_Pointer : Address;
   begin
      --  Note that all non speciality registers are fair game here

      --  Entered in Supervisor Mode.

      --  Disable all interrupts. Easiest on ARM4t to use a constant
      --  to do the job, confirming Supervisor mode and all interrupts disabled

      Switch_To_Supervisor_Mode;

      --  Update SWI address

      SWI_Vector := SWI_Return_Vector;

      --  This is the spot where we store the kernel's register. Only we let
      --  most of them get trashed. Also note that SP should already be
      --  pointing to the kernel's register store.

      Asm ("stm sp, {sp}^", Volatile => True);

      --  Store the kernel's instruction pointer (currently in lr_svc) and its
      --  SPSR onto its stack

      Asm ("mrs r0, spsr", Volatile => True);
      Asm ("stm sp, {r0, lr}", Volatile => True);

      --  Store the kernel's stack pointer and load the current agent's

      Asm ("mov %0, sp",
           Outputs => Address'Asm_Output ("=r", Task_Stack_Pointer),
           Volatile => True);

      Set_Stack_Pointer (This_Oak_Kernel, Task_Stack_Pointer);
      Task_Stack_Pointer := Stack_Pointer (Current_Agent (This_Oak_Kernel));

      --  Move the task's stack pointer into sp and restore task's registers.
      --  Note that only the task's sp is restored.

      Asm ("mov sp, %0",
           Inputs => Address'Asm_Input ("r", Task_Stack_Pointer),
           Volatile => True);
      Asm ("ldm sp, {r0, lr}", Volatile => True);
      Asm ("msr spsr_cxsf, r0", Volatile => True);
      Asm ("ldm sp, {sp}^", Volatile => True);

      --  Install sp into FIQ and IRQ modes. Unlike for the full context switch
      --  this can be done after the register restore since the callee saved
      --  registers are saved by the agent before calling swi.

      --  Move sp into an accessible register.
      Asm ("mov r5, sp", Volatile => True);

      if Current_Agent (This_Oak_Kernel) in Scheduler_Id then
         --  If the agent being switched to is a scheduler agent then it can
         --  only be interrupted by by an FIQ source. So we only install the
         --  FIQ mode and update its SPSR.
         Asm ("msr spsr_c, #0x50", Volatile => True);
      else
         Switch_To_IRQ_Mode;
         Asm ("mov sp, r5",  Volatile => True);
      end if;

      Switch_To_FIQ_Mode;
      Asm ("mov sp, r5",  Volatile => True);
      Switch_To_System_Mode;

      --  Return
      Asm ("subs pc, lr", Volatile => True);
      --  Since the kernel entered via a SWI
   end Request_Context_Switch_To_Agent_Interrupt;

   ---------------------------------------------
   -- Request_Context_Switch_To_Oak_Interrupt --
   ---------------------------------------------

   procedure Request_Context_Switch_To_Oak_Interrupt is
            Task_Stack_Pointer : Address;
   begin
      --  Entered in Supervisor Mode. Can use any register except r0 and r1
      --  since they carry the Agent's instructions to Oak. All other registers
      --  should be saved by the caller.

      --  Disable all interrupts. Easiest on ARM4t to use a constant
      --  to do the job, confirming Supervisor mode and all interrupts disabled

      Switch_To_Supervisor_Mode;

      --  This is the spot where we store the agent's registers. Note that
      --  only the stack register is saved.

      Asm ("stm sp, {sp}^", Volatile => True);
      Asm ("mrs r0, spsr", Volatile => True);
      Asm ("stm sp, {r0, lr}", Volatile => True);

      --  Store the interrupt agent's stack pointer and load the kernel's

      Asm ("mov %0, sp",
           Outputs => Address'Asm_Output ("=r", Task_Stack_Pointer),
           Volatile => True);

      Set_Stack_Pointer (Current_Agent (This_Oak_Kernel), Task_Stack_Pointer);
      Task_Stack_Pointer := Stack_Pointer (This_Oak_Kernel);

      --  Move the task's stack pointer into sp

      Asm ("mov sp, %0",
           Inputs => Address'Asm_Input ("r", Task_Stack_Pointer),
           Volatile => True);

      --  Restore task's registers

      Asm ("ldm sp, {r0, lr}", Volatile => True);
      Asm ("msr spsr_cxsf, r0", Volatile => True);
      Asm ("ldm sp, {sp}^", Volatile => True);

      --  Return
      Asm ("subs pc, lr", Volatile => True);
      --  Since the kernel entered via a SWI
   end Request_Context_Switch_To_Oak_Interrupt;

   -----------------------------------
   -- Disable_Oak_Wake_Up_Interrupt --
   -----------------------------------

   procedure Disable_Oak_Wake_Up_Interrupt is
   begin
      Oak.Processor_Support_Package.Time.Disable_Alarm;
   end Disable_Oak_Wake_Up_Interrupt;

   ----------------------------------
   -- Enable_Oak_Wake_Up_Interrupt --
   ----------------------------------

   procedure Enable_Oak_Wake_Up_Interrupt is
   begin
      Oak.Processor_Support_Package.Time.Disable_Alarm;
   end Enable_Oak_Wake_Up_Interrupt;

end Oak.Core_Support_Package.Interrupts;
