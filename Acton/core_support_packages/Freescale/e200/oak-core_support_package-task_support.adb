with ISA;
with ISA.Power.e200.Timer_Registers;

with System;                         use System;
with System.Machine_Code;            use System.Machine_Code;

package body Oak.Core_Support_Package.Task_Support is
   ----------------------------
   -- Context_Switch_To_Task --
   ----------------------------

   procedure Context_Switch_To_Task
   is
   begin
      --  Switch to Task

      Asm ("sc", Volatile => True);
   end Context_Switch_To_Task;

   ------------------------------
   -- Context_Switch_To_Kernel --
   ------------------------------

   procedure Context_Switch_To_Kernel is
   begin
      null;
   end Context_Switch_To_Kernel;

   ---------------------------------------
   -- Context_Switch_To_Scheduler_Agent --
   ---------------------------------------

   procedure Context_Switch_To_Scheduler_Agent is
   begin
      --  Switch to Scheduler Agent

      Asm ("sc", Volatile => True);
   end Context_Switch_To_Scheduler_Agent;

   -------------------------------
   -- Yield_Processor_To_Kernel --
   -------------------------------

   procedure Yield_Processor_To_Kernel is
   begin
      --  Context switch to kernel.

      Asm ("sc", Volatile => True);
   end Yield_Processor_To_Kernel;

   ---------------------------
   -- Set_Oak_Wake_Up_Timer --
   ---------------------------

   procedure Set_Oak_Wake_Up_Timer (Wake_Up_At : in Oak.Oak_Time.Time) is
      use Oak.Oak_Time;
      Decrementer_Value : Time_Span;
   begin
      --  Do have a problem when the TBL overflows into the TBU
      Decrementer_Value := Wake_Up_At - Clock;
      if Decrementer_Value <= Time_Span_Zero then
         Decrementer_Value := Time_Span_Unit;
      end if;

      Asm
        ("mtdec %L0",   --  Load Wake Up Time into decrementer register
         Inputs   => (Time_Span'Asm_Input ("r", Decrementer_Value)),
         Volatile => True);
   end Set_Oak_Wake_Up_Timer;

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

   ------------------
   -- Sleep_Kernel --
   ------------------

   procedure Sleep_Kernel is
   begin
      --  Set System Call and Decremeter interrupts to Context_Switch_To_Sleep
      --  and Sleep_Interrupt respectively.

      Asm
        ("lwz   r14, %0" & ASCII.LF & ASCII.HT &
         "lwz   r15, %1" & ASCII.LF & ASCII.HT &
         "mtivor8   r14" & ASCII.LF & ASCII.HT &
         "mtivor10  r15",
         Inputs   => (Address'Asm_Input ("m", IVOR8_CS_To_Sleep),
                      Address'Asm_Input ("m", IVOR10_Sleep_Intr)),
         Clobber  => "r14, r15",
         Volatile => True);

      --  Switch to Sleep Task.

      Asm ("sc", Volatile => True);

      --  Restore interrupt handlers.

      Asm
        ("lwz   r14, %0" & ASCII.LF & ASCII.HT &
         "lwz   r15, %1" & ASCII.LF & ASCII.HT &
         "mtivor8   r14" & ASCII.LF & ASCII.HT &
         "mtivor10  r15",
         Inputs   => (Address'Asm_Input ("m", IVOR8_CS_To_Task),
                      Address'Asm_Input ("m", IVOR10_Decrementer_Intr)),
         Clobber  => "r14, r15",
         Volatile => True);
   end Sleep_Kernel;

   -----------------
   --  Sleep_Task --
   -----------------

   procedure Sleep_Task is
   begin

      --  On the e200 we do not have a sleep instruction so we just burn
      --  processor cycles looping
      loop
         null;
      end loop;
   end Sleep_Task;

end Oak.Core_Support_Package.Task_Support;
