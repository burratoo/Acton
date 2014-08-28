------------------------------------------------------------------------------
--                                                                          --
--                         OAK CORE SUPPORT PACKAGE                         --
--                              FREESCALE e200                              --
--                                                                          --
--                  OAK.CORE_SUPPORT_PACKAGE.TASK_SUPPORT                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with System.Machine_Code; use System.Machine_Code;

with Oak.Core_Support_Package.Interrupts;
use  Oak.Core_Support_Package.Interrupts;

with Oak.Core_Support_Package.Clock;
with Oak.Core_Support_Package.Time;

with ISA.ARM.Cortex_M4;             use ISA.ARM.Cortex_M4;
with ISA.ARM.Cortex_M4.Coprocessor; use ISA.ARM.Cortex_M4.Coprocessor;

with Ada.Unchecked_Conversion;

package body Oak.Core_Support_Package.Task_Support is

   --------------------------------
   -- Initialise_Task_Enviroment --
   --------------------------------

   procedure Initialise_Task_Enviroment is
   begin
      Coprocessor.Access_Control_Register.Coprocessor :=
        (10 .. 11 => Full_Access,
         others   => No_Access);
   end Initialise_Task_Enviroment;

   --------------------
   -- Context_Switch --
   --------------------

   procedure Context_Switch is
   begin
      Asm ("svc 0", Volatile => True);
   end Context_Switch;

   -----------------------------
   -- Context_Switch_From_Oak --
   ------------------------------

   --  Offically ARM sucks: it seems like when we have more than one return
   --  value eveything gets sent on the stack. So move the values so that they
   --  are on the stack.

   procedure Context_Switch_From_Oak
     (Reason_For_Oak_To_Run : out Run_Reason;
      Message_Address       : out Address)
   is
   begin
      Asm ("svc 0"      & ASCII.LF & ASCII.HT &
           "mov %0, r4" & ASCII.LF & ASCII.HT &
           "mov %1, r5",
           Outputs  => (Run_Reason'Asm_Output ("=r", Reason_For_Oak_To_Run),
                        Address'Asm_Output ("=r", Message_Address)),
           Volatile => True,
           Clobber  => "r4, r5, r6, r7, r8, r9, r10, r11");
   end Context_Switch_From_Oak;

   ---------------------------
   -- Context_Switch_To_Oak --
   ---------------------------

   procedure Context_Switch_To_Oak
     (Reason_For_Run : in     Run_Reason;
      Message        : in out Oak_Message) is
   begin
      Asm ("mov r4, %0" & ASCII.LF & ASCII.HT &
           "mov r5, %1" & ASCII.LF & ASCII.HT &
           "svc 0",
           Inputs  => (Run_Reason'Asm_Input ("r", Reason_For_Run),
                        Address'Asm_Input ("r", Message'Address)),
           Volatile => True,
           Clobber     => "r4, r5, r6, r7, r8, r9, r10, r11");
   end Context_Switch_To_Oak;

   ------------------------------------------
   -- Context_Switch_Save_Callee_Registers --
   ------------------------------------------

   procedure Context_Switch_Save_Callee_Registers is
   begin
      Asm ("svc 0", Volatile => True,
           Clobber => "r4, r5, r6, r7, r8, r9, r10, r11");
   end Context_Switch_Save_Callee_Registers;

   ------------------------------------------------
   -- Context_Switch_Will_Be_To_Interrupted_Task --
   ------------------------------------------------

   procedure Context_Switch_Will_Be_To_Interrupted_Task is
   begin
      SVC_Vector := Full_Context_Switch_To_Agent_Interrupt'Address;
      SVC_Return_Vector := Request_Context_Switch_To_Oak_Interrupt'Address;
   end Context_Switch_Will_Be_To_Interrupted_Task;

   -----------------------------------------------
   -- Context_Switch_Will_Be_To_Agent --
   -----------------------------------------------

   procedure Context_Switch_Will_Be_To_Agent is
   begin
      SVC_Vector := Request_Context_Switch_To_Agent_Interrupt'Address;
      SVC_Return_Vector := Request_Context_Switch_To_Oak_Interrupt'Address;
   end Context_Switch_Will_Be_To_Agent;

   -----------------------------------------
   -- Context_Switch_Will_Switch_In_Place --
   -----------------------------------------

   procedure Context_Switch_Will_Switch_In_Place is
   begin
      SVC_Vector := In_Place_Context_Switch_To_Agent_Interrupt'Address;
      SVC_Return_Vector := In_Place_Context_Switch_To_Oak_Interrupt'Address;
   end Context_Switch_Will_Switch_In_Place;

   ----------------------------
   -- Enter_Barrier_Function --
   ----------------------------

   procedure Enter_Barrier_Function is
   begin
      null;
      --        Context_Switch_Will_Switch_In_Place;
      --        Context_Switch_Save_Callee_Registers;
   end Enter_Barrier_Function;

   ---------------------------
   -- Exit_Barrier_Function --
   ---------------------------

   procedure Exit_Barrier_Function is
   begin
      null;
      --        Context_Switch;
   end Exit_Barrier_Function;

   ---------------------------
   -- Set_Oak_Wake_Up_Timer --
   ---------------------------

   procedure Set_Oak_Wake_Up_Timer (Wake_Up_At : in Oak.Oak_Time.Time) is
      use Oak.Core_Support_Package.Clock;
      function To_Interal_Oak_Time is new
        Ada.Unchecked_Conversion (Oak.Oak_Time.Time,
                                  Oak.Core_Support_Package.Time.Oak_Time);
   begin
      Update_Alarm (To_Interal_Oak_Time (Wake_Up_At));
   end Set_Oak_Wake_Up_Timer;

   ------------------
   --  Sleep_Agent --
   ------------------

   procedure Sleep_Agent_Run_Loop is
   begin

      --  On the AT91SAM7S we do not have an appropraite sleep state to go into
      --  (takes 20us (or ms) to wake up after system clock is enabled).
      loop
         null;
      end loop;
   end Sleep_Agent_Run_Loop;

end Oak.Core_Support_Package.Task_Support;
