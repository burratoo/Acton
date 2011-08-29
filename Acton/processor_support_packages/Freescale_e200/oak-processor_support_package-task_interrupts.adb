with System.Machine_Code;                        use System.Machine_Code;
with System;                                     use System;
with Oak.Core;
with Oak.Processor_Support_Package.Task_Support;
use  Oak.Processor_Support_Package.Task_Support;
with Oak.Oak_Task;

with ISA.Power.e200.Processor_Control_Registers;
with ISA.Power.e200.z6.HID;
with ISA.Power.e200.Timer_Registers;
with ISA;

package body Oak.Processor_Support_Package.Task_Interrupts is

   procedure Enable_SPE_Instructions;
   pragma Inline_Always (Enable_SPE_Instructions);

   procedure Initialise_Task_Enviroment is
      use ISA.Power.e200.Timer_Registers;
      use ISA;

      subtype HID0_Type is
        ISA.Power.e200.z6.HID.
        Hardware_Implementation_Dependent_Register_0_Type;
      HID0 : HID0_Type;
      TCR  : Timer_Control_Register_Type;
   begin
      --  Set up the registers for the decrementer and system call interrupt
      --  handlers
      Asm
        ("lis       r14, %0@ha" & ASCII.LF & ASCII.HT &
         "addi r14, r14, %0@l" & ASCII.LF & ASCII.HT &
         "mtivpr    r14" & ASCII.LF & ASCII.HT &
         "lwz       r15, %1" & ASCII.LF & ASCII.HT &
         "mtivor8   r15" & ASCII.LF & ASCII.HT &
         "lwz       r16, %2" & ASCII.LF & ASCII.HT &
         "mtivor10  r16" & ASCII.LF & ASCII.HT &
         "lis       r14, 0" & ASCII.LF & ASCII.HT &
         "mttbl     r14" & ASCII.LF & ASCII.HT &
         "mttbu     r14",
         Inputs   => (Integer'Asm_Input ("i", IVPR),
                      System.Address'Asm_Input ("m", IVOR8_CS_To_Task),
                      System.Address'Asm_Input ("m", IVOR10_Decrementer_Intr)),
         Clobber  => "r14, r15, r16",
         Volatile => True);

      --  Enable the time base
      Asm
        ("mfspr  %0, 1008",
         Outputs  => (HID0_Type'Asm_Output ("=r", HID0)),
         Volatile => True);
      HID0.Time_Base := ISA.Enable;
      Asm
        ("mtspr  1008, %0",
         Inputs   => (HID0_Type'Asm_Input ("r", HID0)),
         Volatile => True);

      --  Set up the decrementer auto-reload
      Asm
        ("mftcr  %0",
         Outputs  => (Timer_Control_Register_Type'Asm_Output ("=r", TCR)),
         Volatile => True);
      TCR.Auto_Reload := Enable;
      --  Write the Time Control Register
      Asm
        ("mttcr   %0"       & ASCII.LF & ASCII.HT &
         "li      r14, 50" & ASCII.LF & ASCII.HT &
         "mtdecar r14",
           Inputs   => (Timer_Control_Register_Type'Asm_Input ("r", TCR)),
         Clobber  => "r14",
         Volatile => True);
   end Initialise_Task_Enviroment;

   procedure Enable_SPE_Instructions is
      use ISA.Power.e200.Processor_Control_Registers;
      MSR : Machine_State_Register_Type;
   begin
      --  Read the Machine State Register
      Asm
        ("mfmsr  %0",
         Outputs  => (Machine_State_Register_Type'Asm_Output ("=r", MSR)),
         Volatile => True);
      MSR.Signal_Processing := ISA.Enable;
      --  Write the Machine State Register
      Asm
        ("mtmsr  %0",
         Inputs   => (Machine_State_Register_Type'Asm_Input ("r", MSR)),
         Volatile => True);
   end Enable_SPE_Instructions;

   ---------------------------------
   -- E200_Context_Switch_To_Task --
   ---------------------------------
   procedure E200_Context_Switch_To_Task is
      Task_Stack_Pointer : Address;
      pragma Suppress (Access_Check);
   begin

      Enable_SPE_Instructions;

      Asm
        ("stwu   r1, -136(r1)" & ASCII.LF & ASCII.HT &  -- Allocate stack space
         "stwcx. r1,  r0,  r1" & ASCII.LF & ASCII.HT &  -- Clear memory
         "msync" & ASCII.LF & ASCII.HT &                -- reservation
         "stw    r0,  132(r1)" & ASCII.LF & ASCII.HT &
         "stw    r2,  128(r1)" & ASCII.LF & ASCII.HT &
         "stw    r3,  124(r1)" & ASCII.LF & ASCII.HT &
         "stw    r4,  120(r1)" & ASCII.LF & ASCII.HT &
         "stw    r5,  116(r1)" & ASCII.LF & ASCII.HT &
         "stw    r6,  112(r1)" & ASCII.LF & ASCII.HT &
         "stw    r7,  108(r1)" & ASCII.LF & ASCII.HT &
         "stw    r8,  104(r1)" & ASCII.LF & ASCII.HT &
         "stw    r9,  100(r1)" & ASCII.LF & ASCII.HT &
         "stw    r10,  96(r1)" & ASCII.LF & ASCII.HT &
         "stw    r11,  92(r1)" & ASCII.LF & ASCII.HT &
         "stw    r12,  88(r1)" & ASCII.LF & ASCII.HT &
         "stw    r13,  84(r1)" & ASCII.LF & ASCII.HT &
      --  We use registers r14-r16 to pass task state information back to the
      --  kernel.
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
         "mtsprg0          r1" & ASCII.LF & ASCII.HT & -- Store kernel stack pt
      --  Setup IVOR8 to point to Switch to Context Kernel handler
         "lwz    r20,      %0" & ASCII.LF & ASCII.HT &
         "mtivor8         r20",
         Inputs   => Address'Asm_Input ("m", IVOR8_CS_To_Kernel),
         Volatile => True);

      Task_Stack_Pointer := Oak.Core.Get_Current_Task_Stack_Pointer;
      Oak.Processor_Support_Package.Task_Support.Enable_Oak_Wake_Up_Interrupt;

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
         "evldd  r9,   24(r1)" & ASCII.LF & ASCII.HT & -- Restore accumulator
         "evmra  r10,      r9" & ASCII.LF & ASCII.HT &
         "stwu   r1,   40(r1)" & ASCII.LF & ASCII.HT & -- Drop stack frame (?)
         "evldd  r0,  216(r1)" & ASCII.LF & ASCII.HT & -- and restore GPRs
         "evldd  r2,  208(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r3,  200(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r4,  192(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r5,  184(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r6,  176(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r7,  168(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r8,  160(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r9,  152(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r10, 144(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r11, 136(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r12, 128(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r13, 120(r1)" & ASCII.LF & ASCII.HT &
      --  Registers r14-r16 are loaded below
         "evldd  r17, 112(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r18, 104(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r19, 96(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r20, 88(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r21, 80(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r22, 72(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r23, 64(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r24, 56(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r25, 48(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r26, 40(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r27, 32(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r28, 24(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r29, 16(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r30,  8(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r31,  0(r1)" & ASCII.LF & ASCII.HT &
         "stwu   r1, 224(r1)" & ASCII.LF & ASCII.HT &  --  Restore stack
      --  Restore r14-r16
         "evldd  r14, 16(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r15,  8(r1)" & ASCII.LF & ASCII.HT &
         "evldd  r16,  0(r1)" & ASCII.LF & ASCII.HT &
         "stwu   r1,  24(r1)" & ASCII.LF & ASCII.HT &  --  Restore stack
         "rfi",                   --   switch to kernel routine.
         Inputs   => Address'Asm_Input ("r", Task_Stack_Pointer),
         Volatile => True);

   end E200_Context_Switch_To_Task;

   -----------------------------------
   -- E200_Context_Switch_To_Kernel --
   -----------------------------------
   procedure E200_Context_Switch_To_Kernel is
      Task_Stack_Pointer : Address;
      pragma Suppress (Access_Check);
   begin

      Enable_SPE_Instructions;

      Asm
        ("stwu   r1, -224(r1)" & ASCII.LF & ASCII.HT & -- Allocate stack space
         "stwcx. r1,  r0, r1"  & ASCII.LF & ASCII.HT & -- Clear memory
         "msync" & ASCII.LF & ASCII.HT &              --  reservation
         "evstdd r0,  216(r1)" & ASCII.LF & ASCII.HT & -- Note that we only
         "evstdd r2,  208(r1)" & ASCII.LF & ASCII.HT & -- allocatespace for the
         "evstdd r3,  200(r1)" & ASCII.LF & ASCII.HT & -- GPR at this access
         "evstdd r4,  192(r1)" & ASCII.LF & ASCII.HT & -- 248 bytes through the
         "evstdd r5,  184(r1)" & ASCII.LF & ASCII.HT & -- offset referencing
         "evstdd r6,  176(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r7,  168(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r8,  160(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r9,  152(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r10, 144(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r11, 136(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r12, 128(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r13, 120(r1)" & ASCII.LF & ASCII.HT &
      --  Registers r14-r16 are saved in functions Yield_Processor_To_Kernel
      --  and ______________ and are then set to the return state of the the
      --  task.
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
         "evstdd r23,  24(r1)" & ASCII.LF & ASCII.HT &
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
      --  Setup IVOR8 to point to Switch to Context Task handler
         "lwz     r20,     %1" & ASCII.LF & ASCII.HT &
         "mtivor8         r20" & ASCII.LF & ASCII.HT &
         "mr     %0,       r1",
         Inputs   => (Address'Asm_Input ("m", IVOR8_CS_To_Task)),
         Outputs  => (Address'Asm_Output ("=r", Task_Stack_Pointer)),
         Volatile => True);

      Oak.Core.Set_Current_Task_Stack_Pointer (SP => Task_Stack_Pointer);
      Oak.Processor_Support_Package.Task_Support.Disable_Oak_Wake_Up_Interrupt;

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
         "lwz   r0,  132(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r2,  128(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r3,  124(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r4,  120(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r5,  116(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r6,  112(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r7,  108(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r8,  104(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r9,  100(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r10,  96(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r11,  92(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r12,  88(r1)" & ASCII.LF & ASCII.HT &
         "lwz   r13,  84(r1)" & ASCII.LF & ASCII.HT &
      --  Registers r14-r16 are used for yeilding task state.
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
         "stwu  r1,  136(r1)" & ASCII.LF & ASCII.HT &  --  Restore stack
         "rfi",                     --   switch to task routine.
         Volatile => True);

   end E200_Context_Switch_To_Kernel;

   procedure Decrementer_Interrupt is
   begin
      Enable_SPE_Instructions;

      Asm
        ("stwu   r1, -24(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r14, 16(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r15, 8(r1)" & ASCII.LF & ASCII.HT &
         "evstdd r16, 0(r1)" & ASCII.LF & ASCII.HT &
         "li     r14, %0" & ASCII.LF & ASCII.HT &
         "li     r15, 0" & ASCII.LF & ASCII.HT &
         "li     r16, 0",
         Inputs   => (Oak.Oak_Task.Task_State'Asm_Input
                      ("i", Oak.Oak_Task.Runnable)),
         Volatile => True);
      E200_Context_Switch_To_Kernel;
   end Decrementer_Interrupt;

   procedure Sleep_Interrupt is
      use ISA;
      use ISA.Power.e200.Timer_Registers;
      TSR : constant Timer_Status_Register_Type :=
        (Next_Watchdog_Time       => Disable,
         Watchdog_Timer_Interrupt => Not_Occurred,
         Watchdog_Timer_Reset     => 0,
         Decrement_Interrupt      => Occurred,
         Fixed_Interval_Interrupt => Not_Occurred);
   begin
      Asm
        ("mttsr %0" & ASCII.LF & ASCII.HT &
         "li    r14, 1" & ASCII.LF & ASCII.HT &
         "rfi",
         Inputs   => (Timer_Status_Register_Type'Asm_Input ("r", TSR)),
         Volatile => True);
   end Sleep_Interrupt;

end Oak.Processor_Support_Package.Task_Interrupts;
