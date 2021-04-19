------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                      ATMEL AVR                                       --
--                                                                                      --
--                        OAK.CORE_SUPPORT_PACKAGE.TASK_SUPPORT                         --
--                                                                                      --
--                       Copyright (C) 2012-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Oak.Oak_Time.Internal; use Oak.Oak_Time.Internal;
with Oak.Core_Support_Package.Time.Clock;
use Oak.Core_Support_Package.Time.Clock;
with System; use System;
with System.Machine_Code; use System.Machine_Code;
with Oak.Core; use Oak.Core;

with AVR; use AVR;
with AVR.Timers; use AVR.Timers;
with Oak.Agent;
with Oak.Agent.Tasks;

package body Oak.Core_Support_Package.Task_Support is

   type Direction is (From_Agent, From_Oak);

   --  Applying the naked attribute to Context_Switch raises
   --  useless warnings about other attributes.

   procedure Context_Switch (Context_Switch_Direction : Direction);
   pragma Machine_Attribute (Context_Switch, "naked");

   pragma Suppress (All_Checks);
   --  Suppress all checks since they get in the way and cause unpredictable
   --  problems.

   --  Clock set to run 1024 Hz
   procedure Initialise_Task_Enviroment is
      Output_Compare : constant := 31;
   begin
      Timer_Counter2_Interrupt_Mask_Register :=
        (Output_Compare_Match_B_Interrupt => Disabled,
         Output_Compare_Match_A_Interrupt => Disabled,
         Overflow_Interrupt               => Disabled);
      Asynchronous_Status_Register.Async_Timer_Counter_2 := Enabled;
      Timer_Counter2_Register := 0;
      Output_Compare2_Register_A := Output_Compare;
      Timer_Counter2_Control_Register_A :=
        (Compare_Match_Output_A_Mode => Normal,
         Compare_Match_Output_B_Mode => Normal,
         Waveform_Generation_Mode_0  => False,
         Waveform_Generation_Mode_1  => True);
      Timer_Counter2_Control_Register_B :=
        (Force_Output_Compare_A     => False,
         Force_Output_Compare_B     => False,
         Waveform_Generation_Mode_2 => False,
         Clock_Select               => No_Scaling);
      while Asynchronous_Status_Register.TC_Control_Register_2B_Update_Busy
      loop
         null;
      end loop;

      Timer_Counter2_Interrupt_Flag_Register :=
        (Output_Compare_B => Raised,
         Output_Compare_A => Raised,
         Overflow         => Raised);
      Timer_Counter2_Interrupt_Mask_Register :=
        (Output_Compare_Match_B_Interrupt => Disabled,
         Output_Compare_Match_A_Interrupt => Enabled,
         Overflow_Interrupt               => Disabled);

      --  Oak.Processor_Support_Package.Interrupts.Initialise_Interrupts;
   end Initialise_Task_Enviroment;

   ------------------------------
   -- Context_Switch_To_Kernel --
   ------------------------------

   procedure Context_Switch_To_Kernel
   is
   begin
      Context_Switch (From_Agent);
   end Context_Switch_To_Kernel;

   ----------------------------
   -- Context_Switch_To_Task --
   ----------------------------

   procedure Context_Switch_To_Task
   is
   begin
      Context_Switch (From_Oak);
   end Context_Switch_To_Task;

   ---------------------------------------
   -- Context_Switch_To_Scheduler_Agent --
   ---------------------------------------

   procedure Context_Switch_To_Scheduler_Agent is
   begin
      Context_Switch (From_Oak);
   end Context_Switch_To_Scheduler_Agent;

   -------------------------------
   -- Yield_Processor_To_Kernel --
   -------------------------------

   procedure Yield_Processor_To_Kernel is
   begin
      --  Ensure that r1 is actually zero.

      Asm ("clr r1", Volatile => True);

      Context_Switch (From_Agent);

   end Yield_Processor_To_Kernel;

   ---------------------------
   -- Set_Oak_Wake_Up_Timer --
   ---------------------------

   procedure Set_Oak_Wake_Up_Timer (Wake_Up_At : Oak_Time.Time) is
   begin
      Set_Interrupt_Time (To_Internal_Time (Wake_Up_At));
   end Set_Oak_Wake_Up_Timer;

   ------------------
   -- Sleep_Kernel --
   ------------------

   procedure Sleep_Agent is
   begin
      loop
         null;
      end loop;
   end Sleep_Agent;

   ----------------------------
   -- Context_Switch_To_Task --
   -----------------------------

   procedure Context_Switch (Context_Switch_Direction : Direction) is
      use Oak.Core_Support_Package;

      Task_Stack_Pointer : Address;
   begin
      --  Store registers to task's stack space.
      Asm
        ("cli"                & ASCII.LF & ASCII.HT &
         "push  r0"           & ASCII.LF & ASCII.HT &
         "push  r1" & ASCII.LF & ASCII.HT &
         "push  r2" & ASCII.LF & ASCII.HT &
         "push  r3" & ASCII.LF & ASCII.HT &
         "push  r4" & ASCII.LF & ASCII.HT &
         "push  r5" & ASCII.LF & ASCII.HT &
         "push  r6" & ASCII.LF & ASCII.HT &
         "push  r7" & ASCII.LF & ASCII.HT &
         "push  r8" & ASCII.LF & ASCII.HT &
         "push  r9" & ASCII.LF & ASCII.HT &
         "push r10" & ASCII.LF & ASCII.HT &
         "push r11" & ASCII.LF & ASCII.HT &
         "push r12" & ASCII.LF & ASCII.HT &
         "push r13" & ASCII.LF & ASCII.HT &
         "push r14" & ASCII.LF & ASCII.HT &
         "push r15" & ASCII.LF & ASCII.HT &
         "push r16" & ASCII.LF & ASCII.HT &
         "push r17" & ASCII.LF & ASCII.HT &
         "push r18" & ASCII.LF & ASCII.HT &
         "push r19" & ASCII.LF & ASCII.HT &
         "push r20" & ASCII.LF & ASCII.HT &
         "push r21" & ASCII.LF & ASCII.HT &
         "push r22" & ASCII.LF & ASCII.HT &
         "push r23" & ASCII.LF & ASCII.HT &
         "push r24" & ASCII.LF & ASCII.HT &
         "push r25" & ASCII.LF & ASCII.HT &
         "push r26" & ASCII.LF & ASCII.HT &
         "push r27" & ASCII.LF & ASCII.HT &
         "push r28" & ASCII.LF & ASCII.HT &
         "push r29"           & ASCII.LF & ASCII.HT &
         "push r30"           & ASCII.LF & ASCII.HT &
         "push r31"           & ASCII.LF & ASCII.HT &
         "in   r0, __SREG__"  & ASCII.LF & ASCII.HT &
         "push r0"            & ASCII.LF & ASCII.HT & -- SREG
         "in   %A0, __SP_L__" & ASCII.LF & ASCII.HT &
         "in   %B0, __SP_H__" & ASCII.LF & ASCII.HT &
         "clr r1", --  Ada code below requires r1 to be zero.
         Outputs  => Address'Asm_Output ("=z", Task_Stack_Pointer),
         Volatile => True);

      case Context_Switch_Direction is
         when From_Agent =>
            Core.Set_Current_Agent_Stack_Pointer (SP => Task_Stack_Pointer);
            Task_Stack_Pointer := Core.Oak_Stack_Pointer;
         when From_Oak =>
            Core.Set_Oak_Stack_Pointer (Task_Stack_Pointer);
            Task_Stack_Pointer := Core.Current_Agent_Stack_Pointer;
      end case;

      --  Load To task's registers

      --  We need to turn on or off the interrupts for tasks based on the
      --  global Interrupts_Disable flag. This requires us to modify the task's
      --  copy of SREG before we load it into its register. This is complicated
      --  by the clock interupt also turning off interrupts. We want the clock
      --  interrupt to turn back on the task's interrupt itself, so as to
      --  prevent ballooning of the stack due to repeated firing of the timer
      --  before the handler has a chance to restore the stack. What we do
      --  then is ensure that the zero register, r1, is zero when the task
      --  voluntary yields (see Yield_Processor_To_Kernel). When the increment
      --  clock interrupt fires and it switches to the kernel, r1 carries the
      --  value 0x80 (see Time.Clock.Increment_Clock). When the complement of
      --  r1 is ANDed with the task's SREG, in the former case it will do
      --  nothing and in the latter case it will ensure that the interrupts are
      --  disabled.

      Asm
        ("out __SP_L__, %A0" & ASCII.LF & ASCII.HT &
         "out __SP_H__, %B0" & ASCII.LF & ASCII.HT &
         "pop r16", -- SREG
         Inputs   => Address'Asm_Input ("z", Task_Stack_Pointer),
         Volatile => True);

      if Context_Switch_Direction = From_Oak
        and then Core.Current_Agent.all in Oak.Agent.Tasks.Task_Agent'Class
        and then not Interrupts_Disabled
      then
            Asm ("sbr r16, 7", Volatile => True);
      else
            Asm ("cbr r16, 7", Volatile => True);
      end if;

      Asm
        ("mov r0, r16"      & ASCII.LF & ASCII.HT &
         "pop r31"          & ASCII.LF & ASCII.HT &
         "pop r30"          & ASCII.LF & ASCII.HT &
         "pop r29"          & ASCII.LF & ASCII.HT &
         "pop r28"          & ASCII.LF & ASCII.HT &
         "pop r27"          & ASCII.LF & ASCII.HT &
         "pop r26"          & ASCII.LF & ASCII.HT &
         "pop r25"          & ASCII.LF & ASCII.HT &
         "pop r24"          & ASCII.LF & ASCII.HT &
         "pop r23"          & ASCII.LF & ASCII.HT &
         "pop r22"          & ASCII.LF & ASCII.HT &
         "pop r21"          & ASCII.LF & ASCII.HT &
         "pop r20"          & ASCII.LF & ASCII.HT &
         "pop r19"          & ASCII.LF & ASCII.HT &
         "pop r18"          & ASCII.LF & ASCII.HT &
         "pop r17"          & ASCII.LF & ASCII.HT &
         "pop r16"          & ASCII.LF & ASCII.HT &
         "pop r15"          & ASCII.LF & ASCII.HT &
         "pop r14"          & ASCII.LF & ASCII.HT &
         "pop r13"          & ASCII.LF & ASCII.HT &
         "pop r12"          & ASCII.LF & ASCII.HT &
         "pop r11"          & ASCII.LF & ASCII.HT &
         "pop r10"          & ASCII.LF & ASCII.HT &
         "pop  r9"          & ASCII.LF & ASCII.HT &
         "pop  r8"          & ASCII.LF & ASCII.HT &
         "pop  r7"          & ASCII.LF & ASCII.HT &
         "pop  r6"          & ASCII.LF & ASCII.HT &
         "pop  r5"          & ASCII.LF & ASCII.HT &
         "pop  r4"          & ASCII.LF & ASCII.HT &
         "pop  r3"          & ASCII.LF & ASCII.HT &
         "pop  r2"          & ASCII.LF & ASCII.HT &
         "pop  r1"          & ASCII.LF & ASCII.HT &
         "com r1"           & ASCII.LF & ASCII.HT & -- Now we have r1, we can
         "and r0, r1"       & ASCII.LF & ASCII.HT & -- apply it to SREG
         "clr r1"           & ASCII.LF & ASCII.HT &
         "out __SREG__, r0" & ASCII.LF & ASCII.HT &
         "pop  r0"          & ASCII.LF & ASCII.HT &
         "ret",
         Volatile => True);

   end Context_Switch;

   procedure Task_Interruptible is
   begin
      Interrupts_Disabled := False;
   end Task_Interruptible;

   procedure Task_Not_Interruptible is
   begin
      Interrupts_Disabled := True;
   end Task_Not_Interruptible;

end Oak.Core_Support_Package.Task_Support;
