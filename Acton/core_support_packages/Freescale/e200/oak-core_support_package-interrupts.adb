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
with ISA.Power.e200.Timer_Registers;
with ISA.Power.e200.z6.HID;
with ISA.Power.e200.Processor_Control_Registers;
use ISA.Power.e200.Processor_Control_Registers;

with Oak.Agent;           use Oak.Agent;
with Oak.Agent.Kernel;    use Oak.Agent.Kernel;
with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;

with Oak.Core;    use Oak.Core;

with Oak.Processor_Support_Package.Interrupts;
with Oak.Core_Support_Package.Task_Support;

with System;              use System;
with System.Machine_Code; use System.Machine_Code;

package body Oak.Core_Support_Package.Interrupts is

   --  Suppress all checks since they get in the way and cause unpredictable
   --  problems.

   pragma Suppress (All_Checks);

   -----------------------
   -- Set_Up_Interrupts --
   -----------------------

   procedure Set_Up_Interrupts is
      subtype HID0_Type is
        ISA.Power.e200.z6.HID.
          Hardware_Implementation_Dependent_Register_0_Type;
      HID0 : HID0_Type;
   begin
      Disable_External_Interrupts;
      --  Setup interrupt pointers for external and decrementer interrupts.

      Asm
        ("mtivpr    %0"        & ASCII.LF & ASCII.HT &
         "mtivor4   %0"        & ASCII.LF & ASCII.HT &
         "mtivor10  %1"        & ASCII.LF & ASCII.HT &
         "lis       r3, 0"     & ASCII.LF & ASCII.HT &
         "mttbl     r3"        & ASCII.LF & ASCII.HT & -- Sets time base
         "mttbu     r3",                               -- register
         Inputs   => (System.Address'Asm_Input
                      ("r", External_Interrupt_Handler'Address),
                      System.Address'Asm_Input
                        ("r", Decrementer_Interrupt'Address)),
         Clobber  => "r3",
         Volatile => True);

      --  Enable time base.
      Asm
        ("mfspr  %0, 1008",
         Outputs  => (HID0_Type'Asm_Output ("=r", HID0)),
         Volatile => True);
      HID0.Time_Base := ISA.Enable;
      Asm
        ("mtspr  1008, %0",
         Inputs   => (HID0_Type'Asm_Input ("r", HID0)),
         Volatile => True);

      Enable_Oak_Wake_Up_Interrupt;
      Oak.Processor_Support_Package.Interrupts.Initialise_Interrupts;
   end Set_Up_Interrupts;

   ---------------------------
   -- Decrementer_Interrupt --
   ---------------------------

   procedure Decrementer_Interrupt is
   begin
      --  Clear the decrementer interrupt.
      --
      --  The following is written to the time status register:
      --  TSR : constant Timer_Status_Register_Type :=
      --    (Next_Watchdog_Time       => Disable,
      --     Watchdog_Timer_Interrupt => Not_Occurred,
      --     Watchdog_Timer_Reset     => 0,
      --     Decrement_Interrupt      => Occurred,
      --     Fixed_Interval_Interrupt => Not_Occurred);
      Asm
        ("stwu    r1, -8(r1)"    & ASCII.LF & ASCII.HT &
           "stw     r2,  0(r1)"    & ASCII.LF & ASCII.HT &
           "lis     r2,  0x800"    & ASCII.LF & ASCII.HT &
           "mttsr   r2"            & ASCII.LF & ASCII.HT &
           "lwz     r2,  0(r1)"    & ASCII.LF & ASCII.HT &
           "addi    r1, r1, 8",
         Volatile => True);

      Enable_SPE_Instructions;
      Asm
        ("stu    r1, -8(r1)" & ASCII.LF & ASCII.HT &
           "evstdd r3,  0(r1)" & ASCII.LF & ASCII.HT &
           "li     r3,     %0",
         Inputs   => Run_Reason'Asm_Input ("I", Timer), --  or "i"
         Volatile => True);

      Full_Context_Switch_To_Oak;
   end Decrementer_Interrupt;

   ---------------------------------
   -- Disable_External_Interrupts --
   ---------------------------------

   procedure Disable_External_Interrupts is
   begin
      Asm ("wrteei 0", Volatile => True);
   end Disable_External_Interrupts;

   --------------------------------
   -- Enable_External_Interrupts --
   --------------------------------

   procedure Enable_External_Interrupts is
   begin
      Asm ("wrteei 1", Volatile => True);
   end Enable_External_Interrupts;

   -----------------------------
   -- Enable_SPE_Instructions --
   -----------------------------

   --  We use r2 and r13 as they are the only registers guaranteed not to
   --  be using the whole 64 bits of the register.
   procedure Enable_SPE_Instructions is
   begin
      --  The machine state register is modified as follows:
      --       MSR.Signal_Processing := ISA.Enable;
      Asm
        ("stwu    r1, -8(r1)"     & ASCII.LF & ASCII.HT &
           "stw     r2,  4(r1)"     & ASCII.LF & ASCII.HT &
           "stw     r13, 0(r1)"     & ASCII.LF & ASCII.HT &
           "li      r2,  1"         & ASCII.LF & ASCII.HT &
           "mfmsr   r13"            & ASCII.LF & ASCII.HT &
           "rlwimi  r13,r2,25,6,6"  & ASCII.LF & ASCII.HT &
           "mtmsr   r13"            & ASCII.LF & ASCII.HT &
           "lwz     r2,  4(r1)"     & ASCII.LF & ASCII.HT &
           "lwz     r13, 0(r1)"     & ASCII.LF & ASCII.HT &
           "addi    r1, r1, 8",
         Volatile => True);
   end Enable_SPE_Instructions;

   --------------------------------
   -- External_Interrupt_Handler --
   --------------------------------

   procedure External_Interrupt_Handler is
   begin
      Enable_SPE_Instructions;
      Asm
        ("stu    r1, -8(r1)" & ASCII.LF & ASCII.HT &
           "evstdd r3,  0(r1)" & ASCII.LF & ASCII.HT &
           "li     r3,     %0",
         Inputs   => Run_Reason'Asm_Input ("I", External_Interrupt), --  or "i"
         Volatile => True);
      Full_Context_Switch_To_Oak;
   end External_Interrupt_Handler;

   --------------------------------------------
   -- Full_Context_Switch_To_Agent_Interrupt --
   --------------------------------------------

   procedure Full_Context_Switch_To_Agent_Interrupt is
      Task_Stack_Pointer : Address;
   begin

      Enable_SPE_Instructions;

      --  Save kernel's stack and instruction pointer into their special store

      Asm
        ("msync"      & ASCII.LF & ASCII.HT &
         "mtsprg0 r1" & ASCII.LF & ASCII.HT & -- store kernel stack pointer
         "mfsrr0  r9" & ASCII.LF & ASCII.HT & -- store kernel inst. pointer
         "mtsprg1 r9",
         Volatile => True);

      --  Setup IVOR8 with the handler value already stored in SPRG2

      Asm (
           "mfsprg2 r10" & ASCII.LF & ASCII.HT &
           "mtivor8 r10",
           Volatile => True);

      Task_Stack_Pointer := Stack_Pointer (Current_Agent (This_Oak_Kernel));

      --  Load the appropriate Machine State Register for the agent.

      Asm
        ("mtsrr1   %0",
         Inputs   => Machine_State_Register_Type'Asm_Input
           ("r", Core_Support_Package.Task_Support.Agent_MSR),
         Volatile => True);

      --  Load task's registers
      Asm
        ("mr     r1,       %0" & ASCII.LF & ASCII.HT & -- load task stack ptr.
         "lwz    r24,  24(r1)" & ASCII.LF & ASCII.HT & -- Load SPRs to stack
         "lwz    r25,  20(r1)" & ASCII.LF & ASCII.HT &
         "lwz    r26,  16(r1)" & ASCII.LF & ASCII.HT &
         "lwz    r27,  12(r1)" & ASCII.LF & ASCII.HT &
         "lwz    r28,   8(r1)" & ASCII.LF & ASCII.HT &
         "lwz    r29,   4(r1)" & ASCII.LF & ASCII.HT &
         "lwz    r30,   0(r1)" & ASCII.LF & ASCII.HT &
         "mtspefscr       r24" & ASCII.LF & ASCII.HT &
         "mtxer           r25" & ASCII.LF & ASCII.HT &
         "mtlr            r26" & ASCII.LF & ASCII.HT & -- Copy GPRs to SPRs
         "mtctr           r27" & ASCII.LF & ASCII.HT &
         "mtcr            r28" & ASCII.LF & ASCII.HT &
         "mtusprg0        r29" & ASCII.LF & ASCII.HT &
         "mtsrr0          r30" & ASCII.LF & ASCII.HT & -- Next instruction addr
         "evldd  r9,   32(r1)" & ASCII.LF & ASCII.HT & -- Restore accumulator
         "evmra  r10,      r9" & ASCII.LF & ASCII.HT &
         "addi   r1, r1, 40"   & ASCII.LF & ASCII.HT & -- Drop stack frame (?)
         "evldd  r0,  232(r1)" & ASCII.LF & ASCII.HT & -- and restore GPRs
         "evldd  r2,  224(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r4,  216(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r5,  208(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r6,  200(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r7,  192(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r8,  184(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r9,  176(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r10, 168(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r11, 160(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r12, 152(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r13, 144(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r14, 136(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r15, 128(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r16, 120(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r17, 112(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r18, 104(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r19, 96(r1)"  & ASCII.LF & ASCII.HT &
         "evldd  r20, 88(r1)"  & ASCII.LF & ASCII.HT &
         "evldd  r21, 80(r1)"  & ASCII.LF & ASCII.HT &
         "evldd  r22, 72(r1)"  & ASCII.LF & ASCII.HT &
         "evldd  r23, 64(r1)"  & ASCII.LF & ASCII.HT &
         "evldd  r24, 56(r1)"  & ASCII.LF & ASCII.HT &
         "evldd  r25, 48(r1)"  & ASCII.LF & ASCII.HT &
         "evldd  r26, 40(r1)"  & ASCII.LF & ASCII.HT &
         "evldd  r27, 32(r1)"  & ASCII.LF & ASCII.HT &
         "evldd  r28, 24(r1)"  & ASCII.LF & ASCII.HT &
         "evldd  r29, 16(r1)"  & ASCII.LF & ASCII.HT &
         "evldd  r30,  8(r1)"  & ASCII.LF & ASCII.HT &
         "evldd  r31,  0(r1)"  & ASCII.LF & ASCII.HT &
         "addi   r1, r1, 240"  & ASCII.LF & ASCII.HT & -- restore stack
         "evldd  r3 ,  0(r1)"  & ASCII.LF & ASCII.HT & -- load GRP3 as it was
         "addi   r1, r1, 8"    & ASCII.LF & ASCII.HT & -- the first thing on.
         "rfi",                                        -- exit handler.
         Inputs   => Address'Asm_Input ("r", Task_Stack_Pointer),
         Volatile => True);

   end Full_Context_Switch_To_Agent_Interrupt;

   --------------------------------
   -- Full_Context_Switch_To_Oak --
   --------------------------------

   procedure Full_Context_Switch_To_Oak is
      Task_Stack_Pointer : Address;
   begin

      Enable_SPE_Instructions;

      --  Save agent context, except GPR3 which should already be saved (as it
      --  carries the reason for the full context switch.
      Asm (
         "stwcx. r1,  r0, r1"  & ASCII.LF & ASCII.HT & -- Clear memory
         "msync" & ASCII.LF & ASCII.HT &              --  reservation
         "stwu   r1, -240(r1)" & ASCII.LF & ASCII.HT & -- Allocate stack space
         "evstdd r0,  232(r1)" & ASCII.LF & ASCII.HT & -- Note that we only
         "evstdd r2,  224(r1)" & ASCII.LF & ASCII.HT & -- allocatespace for the
         "evstdd r4,  216(r1)" & ASCII.LF & ASCII.HT & -- GPR at this access
         "evstdd r5,  208(r1)" & ASCII.LF & ASCII.HT & -- so we can use offset
         "evstdd r6,  200(r1)" & ASCII.LF & ASCII.HT & -- referencing
         "evstdd r7,  192(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r8,  184(r1)" & ASCII.LF & ASCII.HT & -- Note that it is
         "evstdd r9,  176(r1)" & ASCII.LF & ASCII.HT & -- expected that gpr3
         "evstdd r10, 168(r1)" & ASCII.LF & ASCII.HT & -- has already been
         "evstdd r11, 160(r1)" & ASCII.LF & ASCII.HT & -- saved.
         "evstdd r12, 152(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r13, 144(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r14, 136(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r15, 128(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r16, 120(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r17, 112(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r18, 104(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r19,  96(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r20,  88(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r21,  80(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r22,  72(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r23,  64(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r24,  56(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r25,  48(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r26,  40(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r27,  32(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r28,  24(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r29,  16(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r30,   8(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r31,   0(r1)" & ASCII.LF & ASCII.HT &
      --  Copy SPRs to GPRs to Stack
      --  Allocate more stack space. Note that it is double word align.
         "stwu   r1,  -40(r1)" & ASCII.LF & ASCII.HT &
         "li     r22,       0" & ASCII.LF & ASCII.HT &  -- Store Accumulator
         "evaddusiaaw r23, r22" & ASCII.LF & ASCII.HT & -- register
         "evstdd r23,  32(r1)" & ASCII.LF & ASCII.HT &
         "mfspefscr       r24" & ASCII.LF & ASCII.HT & -- Next instruction
         "mfxer           r25" & ASCII.LF & ASCII.HT & -- address
         "mflr            r26" & ASCII.LF & ASCII.HT &
         "mfctr           r27" & ASCII.LF & ASCII.HT &
         "mfcr            r28" & ASCII.LF & ASCII.HT &
         "mfusprg0        r29" & ASCII.LF & ASCII.HT &
         "mfsrr0          r30" & ASCII.LF & ASCII.HT &
         "stw    r24,  24(r1)" & ASCII.LF & ASCII.HT & -- Store SPRs to stack
         "stw    r25,  20(r1)" & ASCII.LF & ASCII.HT &
         "stw    r26,  16(r1)" & ASCII.LF & ASCII.HT &
         "stw    r27,  12(r1)" & ASCII.LF & ASCII.HT &
         "stw    r28,   8(r1)" & ASCII.LF & ASCII.HT &
         "stw    r29,   4(r1)" & ASCII.LF & ASCII.HT &
         "stw    r30,   0(r1)" & ASCII.LF & ASCII.HT &
         "mr     %0,       r1",
         Outputs  => (Address'Asm_Output ("=r", Task_Stack_Pointer)),
         Volatile => True);

      Set_Stack_Pointer (Current_Agent (This_Oak_Kernel), Task_Stack_Pointer);

      --  Load Oak's Machine State Register
      Asm
        ("mtsrr1   %0",
         Inputs   => Machine_State_Register_Type'Asm_Input
                       ("r", Core_Support_Package.Task_Support.Oak_MSR),
         Volatile => True);

      --  Restore Oak Kernel's stack and instruction pointer.

      Asm
        ("mfsprg0  r1" & ASCII.LF & ASCII.HT & -- load kernel stack ptr
         "mfsprg1  r9" & ASCII.LF & ASCII.HT & -- kernel instruction addr
         "mtsrr0   r9" & ASCII.LF & ASCII.HT &
         "rfi",                     --   switch to oak.
         Volatile => True);

   end Full_Context_Switch_To_Oak;

   ------------------------------------------------
   -- In_Place_Context_Switch_To_Agent_Interrupt --
   ------------------------------------------------

   procedure In_Place_Context_Switch_To_Agent_Interrupt is
      Task_Stack_Pointer : Address;
   begin
      --  Save kernel's stack and instruction pointer into their special store

      Asm
        ("msync"      & ASCII.LF & ASCII.HT &
           "mtsprg0 r1" & ASCII.LF & ASCII.HT & -- store kernel stack pointer
           "mfsrr0  r9" & ASCII.LF & ASCII.HT & -- store kernel inst. pointer
           "mtsprg1 r9",
         Volatile => True);

      --  Setup IVOR8 with the handler value already stored in SPRG2

      Asm (
           "mfsprg2 r10" & ASCII.LF & ASCII.HT &
             "mtivor8 r10",
           Volatile => True);

      --  Load the in place Machine State Register - keeps interrupts disables
      --  but places the processor into user mode.

         Asm
        ("mtsrr1   %0",
         Inputs   => Machine_State_Register_Type'Asm_Input
           ("r", Core_Support_Package.Task_Support.In_Place_MSR),
         Volatile => True);

      --  Load task's stack pointer.

      Task_Stack_Pointer := Stack_Pointer (Current_Agent (This_Oak_Kernel));

      Asm
        ("mr r1, %0" & ASCII.LF & ASCII.HT &
         "rfi",
         Inputs   => Address'Asm_Input ("r", Task_Stack_Pointer),
         Volatile => True);
   end In_Place_Context_Switch_To_Agent_Interrupt;

   ----------------------------------------------
   -- In_Place_Context_Switch_To_Oak_Interrupt --
   ----------------------------------------------

   procedure In_Place_Context_Switch_To_Oak_Interrupt is
   begin
      --  In place switching does not store any modifications to the agent.

      --  Load Oak's Machine State Register
      Asm
        ("mtsrr1   %0",
         Inputs   => Machine_State_Register_Type'Asm_Input
           ("r", Core_Support_Package.Task_Support.Oak_MSR),
         Volatile => True);

      --  Restore Oak Kernel's stack and instruction pointer.

      Asm
        ("mfsprg0  r1" & ASCII.LF & ASCII.HT & -- load kernel stack ptr
          "mfsprg1  r9" & ASCII.LF & ASCII.HT & -- kernel instruction addr
          "mtsrr0   r9" & ASCII.LF & ASCII.HT &
          "rfi",                     --   switch to oak.
         Volatile => True);
   end In_Place_Context_Switch_To_Oak_Interrupt;

   -----------------------------------------------
   -- Request_Context_Switch_To_Agent_Interrupt --
   -----------------------------------------------

   procedure Request_Context_Switch_To_Agent_Interrupt is
      Task_Stack_Pointer : Address;
   begin
      --  Save kernel's stack and instruction pointer into their special store

      Asm
        ("msync"      & ASCII.LF & ASCII.HT &
           "mtsprg0 r1" & ASCII.LF & ASCII.HT & -- store kernel stack pointer
           "mfsrr0  r9" & ASCII.LF & ASCII.HT & -- store kernel inst. pointer
           "mtsprg1 r9",
         Volatile => True);

      --  Setup IVOR8 with the handler value already stored in SPRG2

      Asm (
           "mfsprg2 r10" & ASCII.LF & ASCII.HT &
           "mtivor8 r10",
           Volatile => True);

      Task_Stack_Pointer := Stack_Pointer (Current_Agent (This_Oak_Kernel));

      --  Load the appropriate Machine State Register for the agent.

      if Current_Agent (This_Oak_Kernel) in Scheduler_Id then
         Asm
           ("mtsrr1   %0",
            Inputs   => Machine_State_Register_Type'Asm_Input
              ("r", Core_Support_Package.Task_Support.Oak_MSR),
            Volatile => True);
      else
         Asm
           ("mtsrr1   %0",
            Inputs   => Machine_State_Register_Type'Asm_Input
              ("r", Core_Support_Package.Task_Support.Agent_MSR),
            Volatile => True);
      end if;

      --  Load agent's stack pointer and instruction register.

      Asm
        ("mr     r1,    %0" & ASCII.LF & ASCII.HT & -- load task stack ptr.
         "lwz    r9, 0(r1)" & ASCII.LF & ASCII.HT & -- load instr. pointer
         "addi   r1, r1, 4" & ASCII.LF & ASCII.HT &
         "mtsrr0 r9"        & ASCII.LF & ASCII.HT &
         "rfi",                                     -- exit handler.
         Inputs   => Address'Asm_Input ("r", Task_Stack_Pointer),
         Volatile => True);
   end Request_Context_Switch_To_Agent_Interrupt;

   ---------------------------------------------
   -- Request_Context_Switch_To_Oak_Interrupt --
   ---------------------------------------------

   procedure Request_Context_Switch_To_Oak_Interrupt is
            Task_Stack_Pointer : Address;
   begin
      --  Save agent's stack and instruction pointer.
      Asm (
           "msync" & ASCII.LF & ASCII.HT &
            "stwu r1, -4(r1)" & ASCII.LF & ASCII.HT & -- Allocate stack space
            "mfsrr0       r9" & ASCII.LF & ASCII.HT &
            "stw  r9,  0(r1)" & ASCII.LF & ASCII.HT &
            "mr   %0,     r1",
           Outputs  => (Address'Asm_Output ("=r", Task_Stack_Pointer)),
           Volatile => True);

      Set_Stack_Pointer (Current_Agent (This_Oak_Kernel), Task_Stack_Pointer);

      --  Load Oak's Machine State Register
      Asm
        ("mtsrr1   %0",
         Inputs   => Machine_State_Register_Type'Asm_Input
           ("r", Core_Support_Package.Task_Support.Oak_MSR),
         Volatile => True);

      --  Restore Oak Kernel's stack and instruction pointer.

      Asm
        ("mfsprg0  r1" & ASCII.LF & ASCII.HT & -- load kernel stack ptr
         "mfsprg1  r9" & ASCII.LF & ASCII.HT & -- kernel instruction addr
         "mtsrr0   r9" & ASCII.LF & ASCII.HT &
           "rfi",                     --   switch to oak.
         Volatile => True);
   end Request_Context_Switch_To_Oak_Interrupt;

   -----------------------------------
   -- Disable_Oak_Wake_Up_Interrupt --
   -----------------------------------

   procedure Disable_Oak_Wake_Up_Interrupt is
      use ISA;
      use ISA.Power.e200.Timer_Registers;

      TCR : Timer_Control_Register_Type;
   begin
      --  Read the Time Control Register
      Asm
        ("mftcr  %0",
         Outputs  => (Timer_Control_Register_Type'Asm_Output ("=r", TCR)),
         Volatile => True);
      TCR.Decrementer_Interrupt := Disable;
      --  Write the Time Control Register
      Asm
        ("mttcr  %0",
         Inputs   => (Timer_Control_Register_Type'Asm_Input ("r", TCR)),
         Volatile => True);
   end Disable_Oak_Wake_Up_Interrupt;

   ----------------------------------
   -- Enable_Oak_Wake_Up_Interrupt --
   ----------------------------------

   procedure Enable_Oak_Wake_Up_Interrupt is
      use ISA;
      use ISA.Power.e200.Timer_Registers;

      TCR : Timer_Control_Register_Type;
   begin
      --  Read the Time Control Register
      Asm
        ("mftcr  %0",
         Outputs  => (Timer_Control_Register_Type'Asm_Output ("=r", TCR)),
         Volatile => True);
      TCR.Decrementer_Interrupt := Enable;
      --  Write the Time Control Register
      Asm
        ("mttcr  %0",
         Inputs   => (Timer_Control_Register_Type'Asm_Input ("r", TCR)),
         Volatile => True);
   end Enable_Oak_Wake_Up_Interrupt;

end Oak.Core_Support_Package.Interrupts;
