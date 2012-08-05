with ISA;
with ISA.Power.e200.z6.HID;
with Oak.Agent.Tasks;
with Oak.Core;
with Oak.Processor_Support_Package.Interrupts;

with Oak.Core_Support_Package.Task_Support;
use  Oak.Core_Support_Package.Task_Support;
with System;                                     use System;
with System.Machine_Code;                        use System.Machine_Code;

package body Oak.Core_Support_Package.Task_Interrupts is

   pragma Suppress (All_Checks);
   --  Suppress all checks since they get in the way and cause unpredictable
   --  problems.

   procedure Clear_Decrementer_Interrupt with Inline_Always;

   procedure Initialise_Task_Enviroment is
      subtype HID0_Type is
        ISA.Power.e200.z6.HID.
          Hardware_Implementation_Dependent_Register_0_Type;
      HID0 : HID0_Type;
   begin
      Disable_External_Interrupts;
      --  Setup interrupt pointers
      Asm
        ("lwz       r3, %0"    & ASCII.LF & ASCII.HT &
         "mtivpr    r3"        & ASCII.LF & ASCII.HT &
         "mtivor4   r3"        & ASCII.LF & ASCII.HT &
         "lwz       r3, %1"    & ASCII.LF & ASCII.HT &
         "mtivor8   r3"        & ASCII.LF & ASCII.HT &
         "lwz       r3, %2"    & ASCII.LF & ASCII.HT &
         "mtivor10  r3"        & ASCII.LF & ASCII.HT &
         "lis       r3, 0"     & ASCII.LF & ASCII.HT &
         "mttbl     r3"        & ASCII.LF & ASCII.HT &
         "mttbu     r3",
         Inputs   => (System.Address'Asm_Input ("m", IVOR4_Ex_Interrupt),
                      System.Address'Asm_Input ("m", IVOR8_CS_To_Task),
                      System.Address'Asm_Input ("m", IVOR10_Decrementer_Intr)),
         Clobber  => "r3",
         Volatile => True);
      Asm
        ("mfspr  %0, 1008",
         Outputs  => (HID0_Type'Asm_Output ("=r", HID0)),
         Volatile => True);
      HID0.Time_Base := ISA.Enable;
      Asm
        ("mtspr  1008, %0",
         Inputs   => (HID0_Type'Asm_Input ("r", HID0)),
         Volatile => True);
      Oak.Processor_Support_Package.Interrupts.Initialise_Interrupts;
   end Initialise_Task_Enviroment;

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

   procedure Clear_Decrementer_Interrupt is
   begin
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
   end Clear_Decrementer_Interrupt;

   procedure Enable_External_Interrupts is
   begin
      Asm ("wrteei 1", Volatile => True);
   end Enable_External_Interrupts;

   procedure Disable_External_Interrupts is
   begin
      Asm ("wrteei 0", Volatile => True);
   end Disable_External_Interrupts;

   ---------------------------------
   -- E200_Context_Switch_To_Task --
   ---------------------------------
   procedure E200_Context_Switch_To_Task is
      use Oak.Core_Support_Package;

      Task_Stack_Pointer : Address;
   begin

      Enable_SPE_Instructions;

      Asm
        ("stwu   r1, -148(r1)" & ASCII.LF & ASCII.HT &  -- Allocate stack space
         "stwcx. r1,  r0,  r1" & ASCII.LF & ASCII.HT &  -- Clear memory
         "msync" & ASCII.LF & ASCII.HT &                -- reservation
         "stw    r0,  144(r1)" & ASCII.LF & ASCII.HT &
         "stw    r2,  140(r1)" & ASCII.LF & ASCII.HT &
         "stw    r3,  136(r1)" & ASCII.LF & ASCII.HT &
         "stw    r4,  132(r1)" & ASCII.LF & ASCII.HT &
         "stw    r5,  128(r1)" & ASCII.LF & ASCII.HT &
         "stw    r6,  124(r1)" & ASCII.LF & ASCII.HT &
         "stw    r7,  120(r1)" & ASCII.LF & ASCII.HT &
         "stw    r8,  116(r1)" & ASCII.LF & ASCII.HT &
         "stw    r9,  112(r1)" & ASCII.LF & ASCII.HT &
         "stw    r10, 108(r1)" & ASCII.LF & ASCII.HT &
         "stw    r11, 104(r1)" & ASCII.LF & ASCII.HT &
         "stw    r12, 100(r1)" & ASCII.LF & ASCII.HT &
         "stw    r13,  96(r1)" & ASCII.LF & ASCII.HT &
         "stw    r14,  92(r1)" & ASCII.LF & ASCII.HT &
         "stw    r15,  88(r1)" & ASCII.LF & ASCII.HT &
         "stw    r16,  84(r1)" & ASCII.LF & ASCII.HT &
         "stw    r17,  80(r1)" & ASCII.LF & ASCII.HT &
         "stw    r18,  76(r1)" & ASCII.LF & ASCII.HT &
         "stw    r19,  72(r1)" & ASCII.LF & ASCII.HT &
         "stw    r20,  68(r1)" & ASCII.LF & ASCII.HT &
         "stw    r21,  64(r1)" & ASCII.LF & ASCII.HT &
         "stw    r22,  60(r1)" & ASCII.LF & ASCII.HT &
         "stw    r23,  56(r1)" & ASCII.LF & ASCII.HT &
         "stw    r24,  52(r1)" & ASCII.LF & ASCII.HT &
         "stw    r25,  48(r1)" & ASCII.LF & ASCII.HT &
         "stw    r26,  44(r1)" & ASCII.LF & ASCII.HT &
         "stw    r27,  40(r1)" & ASCII.LF & ASCII.HT &
         "stw    r28,  36(r1)" & ASCII.LF & ASCII.HT &
         "stw    r29,  32(r1)" & ASCII.LF & ASCII.HT &
         "stw    r30,  28(r1)" & ASCII.LF & ASCII.HT &
         "stw    r31,  24(r1)" & ASCII.LF & ASCII.HT &
      --  Copy SPRs to GPRs to Stack
         "mfxer           r25" & ASCII.LF & ASCII.HT & -- Next instruction
         "mflr            r26" & ASCII.LF & ASCII.HT & -- address
         "mfcr            r27" & ASCII.LF & ASCII.HT &
         "mfctr           r28" & ASCII.LF & ASCII.HT &
         "mfusprg0        r29" & ASCII.LF & ASCII.HT &
         "mfsrr0          r30" & ASCII.LF & ASCII.HT &
         "stw    r25,  20(r1)" & ASCII.LF & ASCII.HT & --  Store SPRs to stack
         "stw    r26,  16(r1)" & ASCII.LF & ASCII.HT &
         "stw    r27,  12(r1)" & ASCII.LF & ASCII.HT &
         "stw    r28,   8(r1)" & ASCII.LF & ASCII.HT &
         "stw    r29,   4(r1)" & ASCII.LF & ASCII.HT &
         "stw    r30,   0(r1)" & ASCII.LF & ASCII.HT &
         "mtsprg0          r1",                        -- Store kernel stack pt
         Volatile => True);

      --  Setup IVOR8 to point to Switch to Context Kernel handler
      Asm (
           "lwz    r20,      %0" & ASCII.LF & ASCII.HT &
           "mtivor8         r20",
           Inputs   => Address'Asm_Input ("m", IVOR8_CS_To_Kernel),
           Volatile => True);

      Task_Stack_Pointer := Core.Current_Agent_Stack_Pointer;
      if Core.Current_Agent.all in Agent.Tasks.Task_Agent'Class then
         Task_Support.Enable_Oak_Wake_Up_Interrupt;
      end if;

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
         "evldd  r0,  240(r1)" & ASCII.LF & ASCII.HT & -- and restore GPRs
         "evldd  r2,  232(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r3,  224(r1)" & ASCII.LF & ASCII.HT &
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
         "addi   r1, r1, 248"  & ASCII.LF & ASCII.HT &  --  Restore stack
         "rfi",                   --   switch to kernel routine.
         Inputs   => Address'Asm_Input ("r", Task_Stack_Pointer),
         Volatile => True);

   end E200_Context_Switch_To_Task;

   -----------------------------------
   -- E200_Context_Switch_To_Kernel --
   -----------------------------------

   procedure E200_Context_Switch_To_Kernel is
      Task_Stack_Pointer : Address;
   begin

      Enable_SPE_Instructions;

      Asm
        ("stwu   r1, -248(r1)" & ASCII.LF & ASCII.HT & -- Allocate stack space
         "stwcx. r1,  r0, r1"  & ASCII.LF & ASCII.HT & -- Clear memory
         "msync" & ASCII.LF & ASCII.HT &              --  reservation
         "evstdd r0,  240(r1)" & ASCII.LF & ASCII.HT & -- Note that we only
         "evstdd r2,  232(r1)" & ASCII.LF & ASCII.HT & -- allocatespace for the
         "evstdd r3,  224(r1)" & ASCII.LF & ASCII.HT & -- GPR at this access
         "evstdd r4,  216(r1)" & ASCII.LF & ASCII.HT & -- 248 bytes through the
         "evstdd r5,  208(r1)" & ASCII.LF & ASCII.HT & -- offset referencing
         "evstdd r6,  200(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r7,  192(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r8,  184(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r9,  176(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r10, 168(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r11, 160(r1)" & ASCII.LF & ASCII.HT &
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

      --  Setup IVOR8 to point to Switch to Context Task handler
      Asm (
           "lwz     r20,     %0" & ASCII.LF & ASCII.HT &
           "mtivor8         r20",
           Inputs   => (Address'Asm_Input ("m", IVOR8_CS_To_Task)),
           Volatile => True);

      Core.Set_Current_Agent_Stack_Pointer (SP => Task_Stack_Pointer);
      Core_Support_Package.Task_Support.Disable_Oak_Wake_Up_Interrupt;

      Asm
        ("mfsprg0         r1" & ASCII.LF & ASCII.HT & -- load kernel stack ptr
      --  Copy SPRs from Stack to GPRs to SPRs
         "lwz   r25,  20(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r26,  16(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r27,  12(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r28,   8(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r29,   4(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r30,   0(r1)" & ASCII.LF & ASCII.HT &
         "mtxer          r25" & ASCII.LF & ASCII.HT &
         "mtlr           r26" & ASCII.LF & ASCII.HT &
         "mtcr           r27" & ASCII.LF & ASCII.HT &
         "mtctr          r28" & ASCII.LF & ASCII.HT &
         "mtusprg0       r29" & ASCII.LF & ASCII.HT &
         "mtsrr0         r30" & ASCII.LF & ASCII.HT & -- Next instruction addr
      --  Restore GPRs
         "lwz   r0,  148(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r2,  140(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r3,  136(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r4,  132(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r5,  128(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r6,  124(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r7,  120(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r8,  116(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r9,  112(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r10, 108(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r11, 104(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r12, 100(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r13,  96(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r14,  92(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r15,  88(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r16,  84(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r17,  80(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r18,  76(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r19,  72(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r20,  68(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r21,  64(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r22,  60(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r23,  56(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r24,  52(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r25,  48(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r26,  44(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r27,  40(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r28,  36(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r29,  32(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r30,  28(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r31,  24(r1)" & ASCII.LF & ASCII.HT &
         "addi  r1, r1, 148"  & ASCII.LF & ASCII.HT &  --  Restore stack
         "rfi",                     --   switch to task routine.
         Volatile => True);

   end E200_Context_Switch_To_Kernel;

   ----------------------------------
   -- E200_Context_Switch_To_Sleep --
   ----------------------------------

   procedure E200_Context_Switch_To_Sleep is
   begin

      --  Store working register and kernel instruction address,
      --  and load sleep task instruction address

      Asm
        ("stwu  r1, -16(r1)"     & ASCII.LF & ASCII.HT &
         "stw   r9,   8(r1)"     & ASCII.LF & ASCII.HT &
         "stw   r10,  4(r1)"     & ASCII.LF & ASCII.HT &
         "mfsrr0   r9"           & ASCII.LF & ASCII.HT &
         "stw   r9,   0(r1)"     & ASCII.LF & ASCII.HT &
         "lis   r9,  %0@ha"      & ASCII.LF & ASCII.HT &
         "addi r9, r9, %0@l"   & ASCII.LF & ASCII.HT &
         "mtsrr0     r9",
         Inputs   => Address'Asm_Input ("i", Sleep_Task'Address),
         Volatile => True);

      Task_Support.Enable_Oak_Wake_Up_Interrupt;

      Asm ("rfi", Volatile => True);
   end E200_Context_Switch_To_Sleep;

   --  Check that the assembly code for Store_Task_Yielded_Status always uses
   --  r0 and r9.

   procedure Decrementer_Interrupt is
   begin
      Clear_Decrementer_Interrupt;
      Enable_SPE_Instructions;
      Asm
        ("stu    r1, -16(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r10,  0(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r9,   8(r1)",
         Volatile => True);
      Oak.Core.Current_Task.Store_Task_Yield_Status (Agent.Tasks.Forced);
      Asm
        ("evldd  r9,   8(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r10,  0(r1)" & ASCII.LF & ASCII.HT &
         "stu    r1,  16(r1)",
         Volatile => True);
      E200_Context_Switch_To_Kernel;
   end Decrementer_Interrupt;

   procedure Sleep_Interrupt is
   begin
      Clear_Decrementer_Interrupt;
      Task_Support.Disable_Oak_Wake_Up_Interrupt;

      --  Restore kernel instruction address and working registers
      Asm
        ("lwz  r9,  0(r1)" & ASCII.LF & ASCII.HT &
         "mtsrr0       r9" & ASCII.LF & ASCII.HT &
         "lwz  r10, 4(r1)" & ASCII.LF & ASCII.HT &
         "lwz  r9,  8(r1)" & ASCII.LF & ASCII.HT &
         "addi r1, r1, 16" & ASCII.LF & ASCII.HT &
         "rfi",
         Volatile => True);
   end Sleep_Interrupt;

end Oak.Core_Support_Package.Task_Interrupts;
