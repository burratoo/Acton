with System;                         use System;
with System.Machine_Code;            use System.Machine_Code;

package body Oak.Core_Support_Package.Task_Support is

   --------------------------------
   -- Initialise_Task_Enviroment --
   --------------------------------

   procedure Initialise_Task_Enviroment is
   begin
      null;
   end Initialise_Task_Enviroment;

   ----------------------------
   -- Context_Switch_To_Task --
   ----------------------------

   procedure Context_Switch_To_Agent
   is
   begin
      --  Switch to Task

      Asm ("sc", Volatile => True);
   end Context_Switch_To_Agent;

   ------------------------------
   -- Context_Switch_To_Kernel --
   ------------------------------

   procedure Context_Switch_To_Kernel is
   begin
      null;
   end Context_Switch_To_Kernel;

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
      Decrementer_Value : Oak_Time.Time_Span;
   begin
      --  Do have a problem when the TBL overflows into the TBU
      Decrementer_Value := Wake_Up_At - Oak_Time.Clock;
      if Decrementer_Value <= Oak_Time.Time_Span_Zero then
         Decrementer_Value := Oak_Time.Time_Span_Unit;
      end if;

      Asm
        ("mtdec %L0",   --  Load Wake Up Time into decrementer register
         Inputs   => (Oak_Time.Time_Span'Asm_Input ("r", Decrementer_Value)),
         Volatile => True);
   end Set_Oak_Wake_Up_Timer;

   ------------------
   --  Sleep_Agent --
   ------------------

   procedure Sleep_Agent is
   begin

      --  On the e200 we do not have a sleep instruction so we just burn
      --  processor cycles looping
      loop
         null;
      end loop;
   end Sleep_Agent;

end Oak.Core_Support_Package.Task_Support;
