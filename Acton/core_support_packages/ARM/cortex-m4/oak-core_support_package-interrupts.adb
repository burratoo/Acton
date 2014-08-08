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

with System.Machine_Code;     use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;

--  Note that the full context save for a non-kernel agent looks like this:
--
--                   ----------- ---
--                   |   r14^  |  |
--                   |    |    |  | 15 x 4 bytes = 60 bytes
--  Stack.Pointer -> |   r0^   |  -
--                   |   LR    |
--                   |   SPSR  |
--                   ----------- ---

--  While a voluntary agent yield only stores:
--
--                   -----------
--                   |   LR^   |
--                   |   SP^   |
--                   |   IP^   |
--  Stack.Pointer -> |   FP^   |
--                   |   LR    |
--                   |   SPSR  |
--                   -----------

--  The kernel is simply:
--
--                   -----------
--  Stack.Pointer -> |   SP^   |
--                   |   LR    |
--                   |   SPSR  |
--                   -----------
--
--  Note that Stack.Pointer does not move since the addressing modes for the
--  STM and LDM instructions enable the ablitity to move data above and below
--  the address. This is useful as the there are only two load/store operations
--  for each agent during a context switch (the registers in the User Mode
--  and the register in the interrupt mode). The Stack.Pointer for the running
--  agent is stored the sp registers for the IRQ and FIQ modes so their
--  handlers can safely and easily store the interrupted agent's register to
--  its stack.

--  The nop assembly instructions after each stm and ldm instructions that
--  touch the user mode registers is needed for ARM versions less than ARMv6,
--  see the ARM Architecture Reference Manual for the reason.

--  Note that any change to the stack frame will require changes to
--  Call_Stack.Ops

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
      Task_Stack_Pointer := Task_Stack_Pointer - 8;
      Set_Stack_Pointer (For_Agent => This_Oak_Kernel,
                         Stack_Pointer => Task_Stack_Pointer);

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

      --  Push interrupted agent's registers onto its call stack and load the
      --  reason for Oak to run into r0 which Oak will later pull out.
      Asm
        ("stm sp, {r0 - lr}^" & ASCII.LF & ASCII.HT &
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

      --  Push interrupted agent's registers onto its call stack and load the
      --  reason for Oak to run into r0 which Oak will later pull out.
      Asm
        ("stm sp, {r0 - lr}^" & ASCII.LF & ASCII.HT &
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

      Asm ("stm sp, {fp - lr}^", Volatile => True);
      Asm ("nop", Volatile => True);

      --  Store the kernel's instruction pointer (currently in lr_svc) and its
      --  SPSR onto its stack

      Asm ("mrs r0, spsr", Volatile => True);
      Asm ("stmdb sp, {r0, lr}", Volatile => True);

      --  Load the current agent's register store

      Task_Stack_Pointer := Stack_Pointer (Current_Agent (This_Oak_Kernel));
      Asm ("mov sp, %0",
           Inputs => Address'Asm_Input ("r", Task_Stack_Pointer),
           Volatile => True);

      --  Install sp into FIQ and IRQ modes
      Asm ("mov r5, sp",  Volatile => True);
      Switch_To_IRQ_Mode;
      Asm ("mov sp, r5",  Volatile => True);
      Switch_To_FIQ_Mode;
      Asm ("mov sp, r5",  Volatile => True);
      Switch_To_Supervisor_Mode;

      --  Restore task's registers
      Asm ("ldmdb sp, {r0, lr}", Volatile => True);
      Asm ("msr spsr_all, r0", Volatile => True);
      Asm ("ldm sp, {r0 - lr}^", Volatile => True);
      Asm ("nop", Volatile => True);

      Asm ("subs pc, lr,#4", Volatile => True);
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
      --  Note that the the agent's register must be saved by the caller
      --  procedure; this allows r4 to be set by these procedures so Oak
      --  can know why it is running. Don't use r0, this is used by Oak to
      --  figure out why it was run.

      --  This is the spot where we store the agent's registers. Note that
      --  r1 has already been saved.

      Asm ("mrs r4, spsr", Volatile => True);
      Asm ("stmdb sp, {r4, lr}", Volatile => True);

      Task_Stack_Pointer := Stack_Pointer (This_Oak_Kernel);
      --  Sneakily, this line should be removed by the compiler.
      Asm ("mov r3, %0",
           Inputs => Address'Asm_Input ("r", Task_Stack_Pointer),
           Volatile => True);

      --  Install Oak's stack register store into Supervisor Mode first then
      --  enter IRQ mode (could have been FIQ, but it doesn't matter here.

      Switch_To_Supervisor_Mode;
      Asm ("mov sp, r3",  Volatile => True);
      Switch_To_FIQ_Mode;

      --  Move the task's stack pointer into sp and restore Oak's registers

      Asm ("mov sp, r3", Volatile => True);
      Asm ("ldmdb sp, {r4, lr}", Volatile => True);
      Asm ("msr spsr_all, r4", Volatile => True);
      Asm ("ldm sp, {fp - lr}^", Volatile => True);
      Asm ("nop", Volatile => True);

      Asm ("movs pc, lr", Volatile => True);
      --  This return sequence is used since Oak was entered via SWI.

   end Full_Context_Switch_To_Oak;

   ------------------------------------------------
   -- In_Place_Context_Switch_To_Agent_Interrupt --
   ------------------------------------------------

   procedure In_Place_Context_Switch_To_Agent_Interrupt is
   begin
      --  Just return since there is no point doing an in-place switch on this
      --  ARM.
      Asm ("movs pc, lr", Volatile => True);
   end In_Place_Context_Switch_To_Agent_Interrupt;

   ----------------------------------------------
   -- In_Place_Context_Switch_To_Oak_Interrupt --
   ----------------------------------------------

   procedure In_Place_Context_Switch_To_Oak_Interrupt is
   begin
      --  See above.
      Asm ("movs pc, lr", Volatile => True);
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

      Asm ("stm sp, {fp - lr}^", Volatile => True);
      Asm ("nop", Volatile => True);

      --  Store the kernel's instruction pointer (currently in lr_svc) and its
      --  SPSR onto its stack

      Asm ("mrs r4, spsr", Volatile => True);
      Asm ("stmdb sp, {r4, lr}", Volatile => True);

      --  Load the current agent's register store address
      Task_Stack_Pointer := Stack_Pointer (Current_Agent (This_Oak_Kernel));

      --  Move the task's stack pointer into sp and restore task's registers.
      --  Note that only the task's sp, fp and lr is restored.

      Asm ("mov sp, %0",
           Inputs => Address'Asm_Input ("r", Task_Stack_Pointer),
           Volatile => True);
      Asm ("ldmdb sp, {r4, lr}", Volatile => True);
      Asm ("msr spsr_all, r4", Volatile => True);
      Asm ("ldm sp, {fp - lr}^", Volatile => True);
      Asm ("nop", Volatile => True);

      --  Install sp into FIQ and IRQ modes. Unlike for the full context switch
      --  this can be done after the register restore since the callee saved
      --  registers are saved by the agent before calling swi.

      --  Move sp into an accessible register.
      Asm ("mov r5, sp", Volatile => True);

      --  Install the register store address into IRQ and FIQ modes. This
      --  allows the handlers to store the agent's registers without having
      --  to mess around with trying to load the register store address and
      --  trying to safely store the registers.

      --  If the agent being switched to is not a task agent or the system
      --  is operating in an interrupt priority, the switched to agent can
      --  only be interrupted by by an FIQ source. So we only install the
      --  FIQ mode and update its SPSR. This is because masking interrupts
      --  is an all or nothing affair on the AT91SAM7. FIQ is kept active so
      --  the timer can keep ticking.

      if Current_Agent (This_Oak_Kernel) = Sleep_Agent
        or else (Current_Agent (This_Oak_Kernel) in Task_Id
                 and then Current_Priority (This_Oak_Kernel)
                 not in Interrupt_Priority)
      then
         Asm ("msr spsr_c, #0x10", Volatile => True);
         Switch_To_IRQ_Mode;
         Asm ("mov sp, r5",  Volatile => True);
      else
         Asm ("msr spsr_c, #0x90", Volatile => True);
      end if;

      Switch_To_FIQ_Mode;
      Asm ("mov sp, r5",  Volatile => True);
      Switch_To_Supervisor_Mode;

      --  Return
      Asm ("movs pc, lr", Volatile => True);
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
      --  only the fp, sp and lr are saved.

      Asm ("stm sp, {fp - lr}^", Volatile => True);
      Asm ("nop", Volatile => True);
      Asm ("mrs r4, spsr", Volatile => True);
      Asm ("stmdb sp, {r4, lr}", Volatile => True);

      --  Load the kernel's register store

      Task_Stack_Pointer := Stack_Pointer (This_Oak_Kernel);

      --  Move the kernel's stack pointer into sp

      Asm ("mov sp, %0",
           Inputs => Address'Asm_Input ("r", Task_Stack_Pointer),
           Volatile => True);

      --  Restore kernel's registers

      Asm ("ldmdb sp, {r4, lr}", Volatile => True);
      Asm ("msr spsr_cxsf, r4", Volatile => True);
      Asm ("ldm sp, {fp - lr}^", Volatile => True);
      Asm ("nop", Volatile => True);
      --  Return
      Asm ("movs pc, lr", Volatile => True);
      --  Since the kernel entered via a SWI
   end Request_Context_Switch_To_Oak_Interrupt;

end Oak.Core_Support_Package.Interrupts;
