with System.Machine_Code;            use System.Machine_Code;
with ISA.Power.e200.Timer_Registers;
with ISA;
with System;                         use System;
with Ada.Unchecked_Conversion;

package body Oak.Processor_Support_Package.Task_Support is
   ----------------------------
   -- Context_Switch_To_Task --
   ----------------------------

   procedure Context_Switch_To_Task
     (Task_Return_State : out OT.Task_Requested_State_Pointer)
   is
      TSR_Address : System.Address;
      --  Suppress aliasing warnings due to unchecked conversion as it is not
      --  relevant.
      pragma Warnings (Off);
      function To_TSR is
        new Ada.Unchecked_Conversion (Source => System.Address,
                                    Target => OT.Task_Requested_State_Pointer);
      pragma Warnings (On);
   begin
      --  A context switch will clobber registers r14 - r17, as tasks will
      --  return their state via these registers.
      Asm
        ("sc"             & ASCII.LF & ASCII.HT &     --  Switch to Task
         "mr     %0, r14",
         Outputs  => System.Address'Asm_Output ("=r", TSR_Address),
         Volatile => True);

      Task_Return_State := To_TSR (TSR_Address);
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
        Clobber => "r14", Volatile => True);
   end Context_Switch_To_Scheduler_Agent;

   -------------------------------
   -- Yield_Processor_To_Kernel --
   -------------------------------

   procedure Yield_Processor_To_Kernel is
   begin
      Asm ("stwu   r1, -8(r1)"     & ASCII.LF & ASCII.HT &
           "evstdd r14, 0(r1)"     & ASCII.LF & ASCII.HT &
           "sc",               -- Context switch to kernel.
          Volatile => True);
   end Yield_Processor_To_Kernel;

   procedure Yield_Processor_To_Kernel
     (Resulting_Task_State : OT.Task_Requested_State) is
   begin
      Asm ("stwu   r1, -8(r1)"     & ASCII.LF & ASCII.HT &
           "evstdd r14, 0(r1)"     & ASCII.LF & ASCII.HT &
           "mr     r14, %0"         & ASCII.LF & ASCII.HT &
           "sc",               -- Context switch to kernel.
           Inputs   => System.Address'Asm_Input
             ("r", Resulting_Task_State'Address),
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
      Exit_Condition : Integer := 0;   --  Should change this into a
                                       --  Boolean-like type.
   begin
      --  Set Decremeter Interrupt to jump to the other side of the infinite
      --  loop
      Asm
        ("lwz   r14, %0" & ASCII.LF & ASCII.HT &
         "mtivor10  r14" & ASCII.LF & ASCII.HT &
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
        ("lwz   r14, %0" & ASCII.LF & ASCII.HT &
         "mtivor10  r14",
         Inputs   => Address'Asm_Input ("m", IVOR10_Decrementer_Intr),
         Clobber  => "r14",
         Volatile => True);
   end Sleep_Kernel;

end Oak.Processor_Support_Package.Task_Support;
