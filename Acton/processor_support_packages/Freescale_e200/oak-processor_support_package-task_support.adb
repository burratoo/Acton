with System.Machine_Code;            use System.Machine_Code;
with ISA.Power.e200.Timer_Registers;
with ISA;
with System;                         use System;

package body Oak.Processor_Support_Package.Task_Support is
   ----------------------------
   -- Context_Switch_To_Task --
   ----------------------------

   procedure Context_Switch_To_Task
     (Task_Return_State : out Task_Requested_State)
   is
   begin
      --  A context switch will clobber registers r14 - r16, as tasks will
      --  return their state via these registers.
      Asm
        ("sc" &
         ASCII.LF &
         ASCII.HT &     --  Switch to Task
         "mr     %0, r14" &
         ASCII.LF &
         ASCII.HT &
         "mr     %1, r15" &
         ASCII.LF &
         ASCII.HT &
         "mr     %2, r16",
         Outputs  => (Oak.Oak_Task.Task_State'Asm_Output
                         ("=r",
                          Task_Return_State.State),
                      Integer'Asm_Output
                         ("=r",
                          Task_Return_State.Parameter (0)),
                      Integer'Asm_Output
                         ("=r",
                          Task_Return_State.Parameter (1))),
         Volatile => True);
      --  Pull out Task return codes
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
      Asm ("sc",                                --  Switch to Task
        Clobber => "r14, r15, r16", Volatile => True);
   end Context_Switch_To_Scheduler_Agent;

   -------------------------------
   -- Yield_Processor_To_Kernel --
   -------------------------------

   procedure Yield_Processor_To_Kernel is
   begin
      Asm ("stwu   r1, -24(r1)"     & ASCII.LF & ASCII.HT &
           "evstdd r14, 16(r1)"     & ASCII.LF & ASCII.HT &
           "evstdd r15, 8(r1)"      & ASCII.LF & ASCII.HT &
           "evstdd r16, 0(r1)"      & ASCII.LF & ASCII.HT &
           "sc",               -- Context switch to kernel.
          Volatile => True);
   end Yield_Processor_To_Kernel;

   procedure Yield_Processor_To_Kernel
     (Resulting_Task_State : Task_Requested_State) is
   begin
      Asm ("stwu   r1, -24(r1)"     & ASCII.LF & ASCII.HT &
           "evstdd r14, 16(r1)"     & ASCII.LF & ASCII.HT &
           "evstdd r15, 8(r1)"      & ASCII.LF & ASCII.HT &
           "evstdd r16, 0(r1)"      & ASCII.LF & ASCII.HT &
           "mr     r14, %0"         & ASCII.LF & ASCII.HT &
           "mr     r15, %1"         & ASCII.LF & ASCII.HT &
           "mr     r16, %2"         & ASCII.LF & ASCII.HT &
           "sc",               -- Context switch to kernel.
         Inputs   => (Oak.Oak_Task.Task_State'Asm_Input
                         ("r",
                          Resulting_Task_State.State),
                      Integer'Asm_Input
                         ("r",
                          Resulting_Task_State.Parameter (0)),
                      Integer'Asm_Input
                         ("r",
                          Resulting_Task_State.Parameter (1))),
         Volatile => True);
      null;
   end Yield_Processor_To_Kernel;
   ---------------------------
   -- Set_Oak_Wake_Up_Timer --
   ---------------------------

   procedure Set_Oak_Wake_Up_Timer (Wake_Up_At : in Ada.Real_Time.Time) is
      use Ada.Real_Time;
      Decrementer_Value : Ada.Real_Time.Time_Span;
   begin
      --  Do have a problem when the TBL overflows into the TBU
      Decrementer_Value := Wake_Up_At - Clock;
      if Decrementer_Value <= Ada.Real_Time.Time_Span_Zero then
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

      TCR : Time_Control_Register_Type;
   begin
      --  Read the Time Control Register
      Asm
        ("mftcr  %0",
         Outputs  => (Time_Control_Register_Type'Asm_Output ("=r", TCR)),
         Volatile => True);
      TCR.Decrementer_Interrupt := Disable;
      --  Write the Time Control Register
      Asm
        ("mttcr  %0",
         Inputs   => (Time_Control_Register_Type'Asm_Input ("r", TCR)),
         Volatile => True);
   end Disable_Oak_Wake_Up_Interrupt;

   ----------------------------------
   -- Enable_Oak_Wake_Up_Interrupt --
   ----------------------------------

   procedure Enable_Oak_Wake_Up_Interrupt is
      use ISA;
      use ISA.Power.e200.Timer_Registers;

      TCR : Time_Control_Register_Type;
   begin
      --  Read the Time Control Register
      Asm
        ("mftcr  %0",
         Outputs  => (Time_Control_Register_Type'Asm_Output ("=r", TCR)),
         Volatile => True);
      TCR.Decrementer_Interrupt := Enable;
      --  Write the Time Control Register
      Asm
        ("mttcr  %0",
         Inputs   => (Time_Control_Register_Type'Asm_Input ("r", TCR)),
         Volatile => True);
   end Enable_Oak_Wake_Up_Interrupt;

   ------------------
   -- Sleep_Kernel --
   ------------------

   procedure Sleep_Kernel is
      Exit_Condition : Integer := 0;   --  Should change this into a
                                       --  Boolean-like type.
   begin
      --  Set Decremeter Interrupt to jump to the other side of the infinite
      --  loop
      Asm
        ("lwz   r14, %0" &
         ASCII.LF &
         ASCII.HT &
         "mtivor10  r14" &
         ASCII.LF &
         ASCII.HT &
         "li    r14, 0",
         Inputs   => Address'Asm_Input ("m", IVOR10_Sleep_Intr),
         Clobber  => "r14",
         Volatile => True);
      Enable_Oak_Wake_Up_Interrupt;
      while Exit_Condition /= 1 loop
         Asm
           ("mr    %0, r14",
            Outputs  => Integer'Asm_Output ("=r", Exit_Condition),
            Volatile => True);
      end loop;
      Disable_Oak_Wake_Up_Interrupt;
      Asm
        ("lwz   r14, %0" & ASCII.LF & ASCII.HT & "mtivor10  r14",
         Inputs   => Address'Asm_Input ("m", IVOR10_Decrementer_Intr),
         Clobber  => "r14",
         Volatile => True);
   end Sleep_Kernel;

end Oak.Processor_Support_Package.Task_Support;
