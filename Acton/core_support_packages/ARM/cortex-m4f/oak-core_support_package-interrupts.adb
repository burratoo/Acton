------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    ARM CORTEX M4F                                    --
--                                                                                      --
--                          OAK.CORE_SUPPORT_PACKAGE.INTERRUPTS                         --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Oak.Agent;           use Oak.Agent;
with Oak.Agent.Kernel;    use Oak.Agent.Kernel;
with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;

with Oak.Core;    use Oak.Core;

with Oak.Core_Support_Package.Clock; use Oak.Core_Support_Package.Clock;

with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;

with ISA;                       use ISA;
with ISA.ARM.Cortex_M4;         use ISA.ARM.Cortex_M4;
with ISA.ARM.Cortex_M4.FPU;     use ISA.ARM.Cortex_M4.FPU;
with ISA.ARM.Cortex_M4.SCB;     use ISA.ARM.Cortex_M4.SCB;
with ISA.ARM.Cortex_M4.SysTick; use ISA.ARM.Cortex_M4.SysTick;

with Interfaces; use Interfaces;

with System.Machine_Code;     use System.Machine_Code;

--  Note that the full context save for a non-kernel agent looks like this:
--
--                   ------------ ---
--                   |   xPSR   |
--                   |    PC    |
--                   |    LR    |
--                   |   r12/ip |  <-- Automatically saved (32 bytes)
--                   |    r3    |
--                   |    r2    |
--                   |    r1    |
--                   |    r0    |
--                   ------------ ---
--                   |    r11   |
--                   |     |    |  <-- Manually saved (32 bytes)
--                   |    r4    |
--                   ------------ ---
--                   | Ex_Return|
--                   ------------ ---

--  While a voluntary agent yield and kernel only stores:
--
--                   ------------ ---
--                   |   xPSR   |
--                   |    PC    |
--                   |    LR    |
--                   |   r12/ip |  <-- Automatically saved (32 bytes)
--                   |    r3    |
--                   |    r2    |
--                   |    r1    |
--                   |    r0    |
--                   ------------ ---
--                   | Ex_Return| <--  Not used for the kernel
--                   ------------ ---

--
--  An agent that uses the FPU will look like this
--
--                   ------------ ---
--                   |   FPSCR  |
--                   |   s15    |   <-- Automatically saved (68 bytes)
--                   |    |     |
--                   |    s0    |
--                   ------------ ---
--                   |   xPSR   |
--                   |    PC    |
--                   |    LR    |
--                   |   r12/ip |  <-- Automatically saved (32 bytes)
--                   |    r3    |
--                   |    r2    |
--                   |    r1    |
--                   |    r0    |
--                   ------------ ---
--                   |    r11   |
--                   |     |    |  <-- Manually saved (32 bytes)
--                   |    r4    |
--                   ------------ ---
--                   |   s31    |
--                   |    |     |  <-- Manually saved (64 bytes)
--                   |   s16    |
--                   ------------ ---
--                   | Ex_Return|
--                   ------------ ---

--  Note that any change to the stack frame will require changes to
--  Call_Stack.Ops

package body Oak.Core_Support_Package.Interrupts is

   --  Suppress all checks since they get in the way and cause unpredictable
   --  problems.

   pragma Suppress (All_Checks);

   Floating_Point_Return : constant Unsigned_32 := 16#FFFF_FFED#;

   -----------------------
   -- Set_Up_Interrupts --
   -----------------------

   procedure Set_Up_Interrupts is
   begin
      --  Core specific interupt setup code;

      Vector_Table_Offset_Register := Exception_Vector_Table'Address;
      Configuration_And_Control_Register :=
        (Stack_Alignment                 => Eight_Bytes,
         Flault_Handlers_Ignore_BusFault => False,
         Trap_Divide_By_0                => True,
         Trap_Unaligned_Access           => False,
         Unpriviledged_Soft_Intr_Trigger => True,
         Thread_Mode_Access              => Any_Level);

      System_Handler_Control_And_State_Register :=
        (Usage_Fault => Enable,
         Bus_Fault   => Enable,
         Mem_Manage  => Enable,
         others      => False);

      --  Oak makes use of the Cortex's NVIC subpriorities rather than its
      --  group priorities. This way interrupts do not need to be explicitly
      --  disabled while switching to Oak to handle an exception.

      Application_Interrupt_And_Reset_Control_Register :=
        (Veckey               => Write,
         Endianness           => Little,
         Priority_Group       => 0,
         System_Reset_Request => False);

      --  Set the priority of the svc and timer interrupts to 0.

      System_Handler_Priority_Register (Exceptions.SVCall) := 0;
      System_Handler_Priority_Register (Exceptions.SysTick) := 0;

      --  Setup SysTick. Disable the timer just in case.

      ARM.Cortex_M4.SysTick.Control_And_Status_Register :=
        (Counter      => Disable,
         Interrupt    => Disable,
         Clock_Source => Processor,
         Count_Flag   => False);

      ARM.Cortex_M4.SysTick.Reload_Value_Register  := Time_Base_Tick;
      ARM.Cortex_M4.SysTick.Current_Value_Register := 0;

      ARM.Cortex_M4.SysTick.Control_And_Status_Register :=
        (Counter      => Enable,
         Interrupt    => Enable,
         Clock_Source => Processor,
         Count_Flag   => False);

      --  Setup FPU for correct context switching operation

      FPU.Floating_Point_Context_Control_Register :=
        (Automatic_State_Preservation => Enable,
         Lazy_State_Preservation      => Enable,
         others => False);

      Agent_With_FPR_To_Save := No_Agent;

      --  Processor specific initialisation routines

      Oak.Processor_Support_Package.Interrupts.Initialise_Interrupts;

   end Set_Up_Interrupts;

   ---------------------------
   -- Decrementer_Interrupt --
   ---------------------------

   procedure Decrementer_Interrupt is
   begin
      --  Push interrupted agent's registers onto its call stack and load the
      --  reason for Oak to run into r1 which Oak will later pull out.
      Asm
      --  load the banked stack pointer for the agent
        ("mrs r1, psp"           & ASCII.LF & ASCII.HT &
         "stmfd r1!, {r4 - r11}" & ASCII.LF & ASCII.HT &
         "mov r4, %0",
         Inputs   => Run_Reason'Asm_Input ("i", Timer),
         Volatile => True);
      Full_Context_Switch_To_Oak;
   end Decrementer_Interrupt;

   --------------------------------
   -- External_Interrupt_Handler --
   --------------------------------

   procedure IRQ_Interrupt_Handler is
   begin
      --  At this point r0 ??? r3, r12, lr, pc and xPSR have been pushed onto
      --  the stack. Now we push the remaining registers and place the reason
      --  for Oak to run in r4.

      Asm
      --  load the banked stack pointer for the agent
        ("mrs r1, psp"           & ASCII.LF & ASCII.HT &
         "stmfd r1!, {r4 - r11}" & ASCII.LF & ASCII.HT &
         "mov r4, %0",
         Inputs   => Run_Reason'Asm_Input ("i", External_Interrupt),
         Volatile => True);

      Current_IRQ := Exceptions.Current_IRQ;

      Full_Context_Switch_To_Oak;
   end IRQ_Interrupt_Handler;

   --------------------------------------------
   -- Full_Context_Switch_To_Agent_Interrupt --
   --------------------------------------------

   procedure Full_Context_Switch_To_Agent_Interrupt is
      Task_Stack_Pointer : Address;
      Hardware_Priority  : Exception_Priority;
   begin
      --  Ensure this code does not modify the stack pointer. Can modify any
      --  other register.

      --  Update SWI address

      SVC_Vector := SVC_Return_Vector;

      --  Load the current agents stack pointer

      Task_Stack_Pointer := Stack_Pointer (Current_Agent (This_Oak_Kernel));
      Hardware_Priority  := To_Cortex_Priority
        (Current_Priority (This_Oak_Kernel));

      --  Set base priority, restore agent's exc_return value and load its
      --  stack address into the banked psp register.

      Asm ("ldmfd %0!, {lr}",
           Outputs => Address'Asm_Output ("+r", Task_Stack_Pointer),
           Volatile => True);

      --  Handle lazy stacking of floating point registers

      if Current_Agent (This_Oak_Kernel) = No_Agent then
         --  Sleep agent does not use fpu
         null;

      elsif Current_Agent (This_Oak_Kernel) = Agent_With_FPR_To_Save then
         --  No need to save registers since we are switching back to the
         --  same agent. Delete space reserved for them.

         Asm ("add %0, #64",
              Outputs  => Address'Asm_Output ("+r", Task_Stack_Pointer),
              Volatile => True);

      elsif Current_Agent (This_Oak_Kernel) in Task_Id then
         if Agent_With_FPR_To_Save /= No_Agent then
            Asm ("add    r0, %0, #4"       & ASCII.LF & ASCII.HT &
                 "vstmia r0, {s16 - s31}",
                 Inputs   => Address'Asm_Input ("r",
                   Stack_Pointer (Agent_With_FPR_To_Save)),
                 Clobber  => "r0",
                 Volatile => True);
            Agent_With_FPR_To_Save := No_Agent;
         end if;

         --  Restore fp registers if needed
         Asm ("tst   lr, #0x10"           & ASCII.LF & ASCII.HT &
              "it    eq"                  & ASCII.LF & ASCII.HT &
              "vldmiaeq %0!, {s16 - s31}",
              Outputs  => Address'Asm_Output ("+r", Task_Stack_Pointer),
              Volatile => True);
      end if;

      Asm ("ldmfd %0!, {r4 - r11}"     & ASCII.LF & ASCII.HT &
           "msr psp, %0"               & ASCII.LF & ASCII.HT &
           "msr basepri, %1"           & ASCII.LF & ASCII.HT &
           "bx lr", -- lr holds the exc_return value
           Inputs => (Address'Asm_Input ("r", Task_Stack_Pointer),
                      Exception_Priority'Asm_Input ("r", Hardware_Priority)),
           Clobber => "r3",
           Volatile => True);

   end Full_Context_Switch_To_Agent_Interrupt;

   --------------------------------
   -- Full_Context_Switch_To_Oak --
   --------------------------------

   procedure Full_Context_Switch_To_Oak is
      Task_Stack_Pointer : Address;
      Ex_Return          : Unsigned_32;
   begin
      --  This procedure should have been entered with all registers saved on
      --  the stack and r4 holding the reason for run. All that is left is to
      --  do is save the exc_return value that is in lr. Note that r1 holds the
      --  stack address and is the expected register for the output

      Asm ("mov %0, r1" & ASCII.LF & ASCII.HT &
           "mov %1, lr",
           Outputs => (Address'Asm_Output ("=r", Task_Stack_Pointer),
                       Unsigned_32'Asm_Output ("=r", Ex_Return)),
           Volatile => True);

      if Ex_Return = Floating_Point_Return then
         Agent_With_FPR_To_Save := Current_Agent (This_Oak_Kernel);
         --  Create space for the floating point register
         Asm ("sub %0, #64",
              Outputs  => Address'Asm_Output ("+r", Task_Stack_Pointer),
              Volatile => True);
      end if;

      Asm ("stmfd %0!, {lr}",
           --  Save the stack pointer
           Outputs => Address'Asm_Output ("+r", Task_Stack_Pointer),
           Clobber  => "r4, r5", -- For the protection of these variables
           Volatile => True);

      Set_Stack_Pointer (Current_Agent (This_Oak_Kernel), Task_Stack_Pointer);

      --  Return to Oak, setting lr := 16#FFFF_FFF9# (stay in thread mode, use
      --  main stack pointer). Set base priority.

      Asm ("mov r0, %0"      & ASCII.LF & ASCII.HT &
           "msr basepri, r0" & ASCII.LF & ASCII.HT &
           "mvn lr, #0x6"    & ASCII.LF & ASCII.HT &
           "bx lr",
           Inputs => Any_Priority'Asm_Input ("i", Oak_Mask_Priority),
             Volatile => True);
   end Full_Context_Switch_To_Oak;

   ------------------------------------------------
   -- In_Place_Context_Switch_To_Agent_Interrupt --
   ------------------------------------------------

   procedure In_Place_Context_Switch_To_Agent_Interrupt is
   begin
      --  Just return at this point. Since the Cortex-M4 supports a MMU this
      --  code should be updated to support it.
      Asm ("bx lr", Volatile => True);
   end In_Place_Context_Switch_To_Agent_Interrupt;

   ----------------------------------------------
   -- In_Place_Context_Switch_To_Oak_Interrupt --
   ----------------------------------------------

   procedure In_Place_Context_Switch_To_Oak_Interrupt is
   begin
      --  See above.
      Asm ("bx lr", Volatile => True);
   end In_Place_Context_Switch_To_Oak_Interrupt;

   -----------------------------------------------
   -- Request_Context_Switch_To_Agent_Interrupt --
   -----------------------------------------------

   procedure Request_Context_Switch_To_Agent_Interrupt is
      Task_Stack_Pointer : Address;
      Hardware_Priority  : Exception_Priority;
   begin
      --  Ensure this code does not modify the stack pointer. Can modify any
      --  other register.

      --  Update SVC address

      SVC_Vector := SVC_Return_Vector;

      --  Load the current agents stack pointer

      Task_Stack_Pointer := Stack_Pointer (Current_Agent (This_Oak_Kernel));
      if Current_Agent (This_Oak_Kernel) in Scheduler_Id then
         Hardware_Priority := Oak_Mask_Priority;
      else
         Hardware_Priority  := To_Cortex_Priority
           (Current_Priority (This_Oak_Kernel));
      end if;

      --  Set base priority, restore agent's exc_return value and load its
      --  stack address into the banked psp register.

      Asm ("ldmfd %0!, {lr}",
           Outputs => Address'Asm_Output ("+r", Task_Stack_Pointer),
           Volatile => True);

      --  Handle lazy stacking of floating point registers

      if Current_Agent (This_Oak_Kernel) = No_Agent then
         --  Sleep agent does not use fpu
         null;

      elsif Current_Agent (This_Oak_Kernel) = Agent_With_FPR_To_Save then
         --  No need to save registers since we are switching back to the
         --  same agent. Delete space reserved for them.

         Asm ("add %0, #64",
              Outputs => Address'Asm_Output ("+r", Task_Stack_Pointer),
              Volatile => True);

      elsif Current_Agent (This_Oak_Kernel) in Task_Id then
         if Agent_With_FPR_To_Save /= No_Agent then
            Asm ("add    r0, %0, #4"       & ASCII.LF & ASCII.HT &
                 "vstmia r0, {s16 - s31}",
                 Inputs   => Address'Asm_Input ("r",
                   Stack_Pointer (Agent_With_FPR_To_Save)),
                 Clobber  => "r0",
                 Volatile => True);
            Agent_With_FPR_To_Save := No_Agent;
         end if;

         --  Restore fp registers if needed
         Asm ("tst   lr, #0x10"           & ASCII.LF & ASCII.HT &
              "it    eq"                  & ASCII.LF & ASCII.HT &
              "vldmiaeq %0!, {s16 - s31}",
              Outputs  => Address'Asm_Output ("+r", Task_Stack_Pointer),
              Volatile => True);
      end if;

      Asm ("msr psp, %0"           & ASCII.LF & ASCII.HT &
           "msr basepri, %1"       & ASCII.LF & ASCII.HT &
           "bx lr", -- r3 holds the exc_return value
           Inputs => (Address'Asm_Input ("r", Task_Stack_Pointer),
                      Exception_Priority'Asm_Input ("r", Hardware_Priority)),
           Volatile => True);
   end Request_Context_Switch_To_Agent_Interrupt;

   ---------------------------------------------
   -- Request_Context_Switch_To_Oak_Interrupt --
   ---------------------------------------------

   procedure Request_Context_Switch_To_Oak_Interrupt is
      Task_Stack_Pointer : Address;
      Ex_Return          : Unsigned_32;
   begin
      --  Store the agent's exc_return value by first grabing its stack pointer
      Asm ("mrs %0, psp" & ASCII.LF & ASCII.HT &
           "mov %1, lr",
           Outputs => (Address'Asm_Output ("=r", Task_Stack_Pointer),
                       Unsigned_32'Asm_Output ("=r", Ex_Return)),
           Volatile => True);

      if Ex_Return = Floating_Point_Return then
         Agent_With_FPR_To_Save := Current_Agent (This_Oak_Kernel);
         --  Create space for the floating point register
         Asm ("sub %0, #64",
              Outputs  => Address'Asm_Output ("+r", Task_Stack_Pointer),
              Volatile => True);
      end if;

      Asm ("stmfd %0!, {lr}",
           --  Save the stack pointer
           Outputs => Address'Asm_Output ("+r", Task_Stack_Pointer),
           Clobber  => "r4, r5", -- For the protection of these variables
           Volatile => True);

      Set_Stack_Pointer (Current_Agent (This_Oak_Kernel), Task_Stack_Pointer);

      --  Return to Oak, setting lr := 16#FFFF_FFF9# (stay in thread mode, use
      --  main stack pointer). Set base priority.

      Asm ("mov r0, %0"      & ASCII.LF & ASCII.HT &
           "msr basepri, r0" & ASCII.LF & ASCII.HT &
           "mvn lr, #0x6"    & ASCII.LF & ASCII.HT &
           "bx lr",
           Inputs => Any_Priority'Asm_Input ("i", Oak_Mask_Priority),
           Volatile => True);
   end Request_Context_Switch_To_Oak_Interrupt;

   procedure SVCall_Handler is
   begin
      Asm ("bx %0",
           Inputs => Address'Asm_Input ("r", SVC_Vector),
           Volatile => True);
   end SVCall_Handler;

end Oak.Core_Support_Package.Interrupts;
