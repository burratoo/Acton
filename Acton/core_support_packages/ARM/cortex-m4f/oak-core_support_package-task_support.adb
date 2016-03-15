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

with Oak.Core_Support_Package.Trace; use Oak.Core_Support_Package.Trace;

with Oak.Core_Support_Package.Clock;
with Oak.Core_Support_Package.Time;

with ISA;                           use ISA;
with ISA.ARM.Cortex_M4;             use ISA.ARM.Cortex_M4;
with ISA.ARM.Cortex_M4.DCB;         use ISA.ARM.Cortex_M4.DCB;
with ISA.ARM.Cortex_M4.DWT;         use ISA.ARM.Cortex_M4.DWT;
with ISA.ARM.Cortex_M4.ITM;         use ISA.ARM.Cortex_M4.ITM;
with ISA.ARM.Cortex_M4.Coprocessor; use ISA.ARM.Cortex_M4.Coprocessor;
with ISA.ARM.Cortex_M4.TPIU;        use ISA.ARM.Cortex_M4.TPIU;

with Interfaces; use Interfaces;

with Ada.Unchecked_Conversion;

package body Oak.Core_Support_Package.Task_Support is

   Kernel_Entry_Stimulus_Port : constant := 10;
   Kernel_Exit_Stimulus_Port  : constant := 11;

   function To_Stimulus_16 is new
     Ada.Unchecked_Conversion (Kernel_Entry_Tracing, Unsigned_16);

   function To_Stimulus_8 is new
     Ada.Unchecked_Conversion (Kernel_Exit_Tracing, Unsigned_8);

   --------------------------------
   -- Initialise_Task_Enviroment --
   --------------------------------

   procedure Initialise_Task_Enviroment is
   begin
      --  Enable FP Unit

      Coprocessor.Access_Control_Register.Coprocessor :=
        (10 .. 11 => Full_Access,
         others   => No_Access);

      --  Setup SWO and debug tracing

      --  Enable access to SWO registers
--        DCB.Debug_Exception_And_Monitor_Control_Register.Trace := Enable;
--        ITM.Lock_Access_Register := 16#C5AC_CE55#;
--
--        --  Disable ITM and stimulus port to ensure nothing is transmitted
--        --  over SWO while setting it up
--
--        ITM.Stimulus_Port_Enable_Register := (others => Disable);
--        ITM.Trace_Control_Register.ITM := Disable;
--
--        --  Setup SWO, DWT and ITM
--
--        TPIU.Selected_Pin_Protocol_Register := (Transmit_Mode => SWO_NRZ);
--     TPIU.Asynchronous_Clock_Prescaler_Register := (SWO_Prescaler => 16#1B#);
--
--        ITM.Trace_Privilege_Register := (others => Unprivileged_Allowed);
--        DWT.Control_Register :=
--          (Number_Of_Comparitors => 4,
--           Exception_Trace       => Disable,
--           Sync_Counter_Tap_At   => Clock_Divide_By_256M,
--           Cycle_Count           => Enable);
--
--        TPIU.Formatter_And_Flush_Control_Register :=
--          (Triggers_Inserted     => True,
--           Continuous_Formatting => Disable);
--
--        ITM.Trace_Control_Register :=
--          (ITM_Busy                   => False,
--           Trace_Bus_Id               => 1,
--           Global_Timestamp_Frequency => Disable,
--           Local_Timestamp_Prescaler  => None,
--           Timestamp_Clock_Source     => System_Clock,
--           Forward_DWT_Packets        => Disable,
--           Synchronization_Packets    => Disable,
--           Local_Timestamp_Generation => Enable,
--           ITM                        => Enable);
--
--    ITM.Stimulus_Port_Enable_Register (Kernel_Entry_Stimulus_Port) := Enable;
--    ITM.Stimulus_Port_Enable_Register (Kernel_Exit_Stimulus_Port) := Enable;
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

   --------------------------
   -- Entered_Kernel_Trace --
   --------------------------

   procedure Entered_Kernel_Trace (Reason  : Run_Reason;
                                   Request : Agent_State) is
   begin
      ITM.Stimulus_Port_Register_16 (Kernel_Entry_Stimulus_Port).Data :=
        To_Stimulus_16 ((Reason  => Reason,
                         Request => Request));
   end Entered_Kernel_Trace;

   -------------------------
   -- Exited_Kernel_Trace --
   -------------------------

   procedure Exited_Kernel_Trace (To_Agent : Oak_Agent_Id) is
   begin
      ITM.Stimulus_Port_Register_8 (Kernel_Exit_Stimulus_Port).Data :=
        To_Stimulus_8 ((To_Agent => To_Agent));
   end Exited_Kernel_Trace;

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
      loop
         Asm ("wfi", Volatile => True);
      end loop;
   end Sleep_Agent_Run_Loop;

end Oak.Core_Support_Package.Task_Support;
