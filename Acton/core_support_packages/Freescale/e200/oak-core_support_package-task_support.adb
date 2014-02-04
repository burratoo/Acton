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

with System;                         use System;
with System.Machine_Code;            use System.Machine_Code;

with Oak.Core_Support_Package.Interrupts;
use  Oak.Core_Support_Package.Interrupts;

package body Oak.Core_Support_Package.Task_Support is

   --------------------------------
   -- Initialise_Task_Enviroment --
   --------------------------------

   procedure Initialise_Task_Enviroment is
   begin
      null;
   end Initialise_Task_Enviroment;

   --------------------
   -- Context_Switch --
   --------------------

   procedure Context_Switch is
   begin
      Asm ("sc", Volatile => True);
   end Context_Switch;

   -----------------------------
   -- Context_Switch_From_Oak --
   ------------------------------

   procedure Context_Switch_From_Oak
     (Reason_For_Oak_To_Run : out    Run_Reason;
      Message               : out Message_Access)
   is
   begin
      Asm ("sc",
           Outputs  => (Run_Reason'Asm_Output ("=r", Reason_For_Oak_To_Run),
                        Message_Access'Asm_Output ("=r", Message)),
           Clobber  => "r30",
           Volatile => True);
   end Context_Switch_From_Oak;

   ------------------------------------------
   -- Context_Switch_Save_Callee_Registers --
   ------------------------------------------

   procedure Context_Switch_Save_Callee_Registers is
   begin
      Asm ("sc", Volatile => True,
           Clobber => "r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, "
           & "r25, r26, r27, r28, r29, r30, r31, cc, ctr, lr");
   end Context_Switch_Save_Callee_Registers;

   procedure Context_Switch_Save_Callee_Registers
     (Message : in out Message_Access) is
   begin
      Asm ("sc",
           Volatile => True,
           Outputs => Message_Access'Asm_Output ("=r", Message),
           Clobber => "r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, "
           & "r25, r26, r27, r28, r29, r30, r31, cc, ctr, lr");
   end Context_Switch_Save_Callee_Registers;

   ------------------------------------------------
   -- Context_Switch_Will_Be_To_Interrupted_Task --
   ------------------------------------------------

   procedure Context_Switch_Will_Be_To_Interrupted_Task is
   begin
      Asm
        ("mtivor8   %0"        & ASCII.LF & ASCII.HT &
         "mtsprg2   %1",
         Inputs   => (System.Address'Asm_Input
                      ("r", Full_Context_Switch_To_Agent_Interrupt'Address),
                      System.Address'Asm_Input ("r",
                         Request_Context_Switch_To_Oak_Interrupt'Address)),
         Volatile => True);
   end Context_Switch_Will_Be_To_Interrupted_Task;

   -----------------------------------------------
   -- Context_Switch_Will_Be_To_Agent --
   -----------------------------------------------

   procedure Context_Switch_Will_Be_To_Agent is
   begin
      Asm
        ("mtivor8   %0"        & ASCII.LF & ASCII.HT &
         "mtsprg2   %1",
         Inputs   =>
           (System.Address'Asm_Input
                ("r", Request_Context_Switch_To_Agent_Interrupt'Address),
            System.Address'Asm_Input
              ("r", Request_Context_Switch_To_Oak_Interrupt'Address)),
         Volatile => True);
   end Context_Switch_Will_Be_To_Agent;

   -----------------------------------------
   -- Context_Switch_Will_Switch_In_Place --
   -----------------------------------------

   procedure Context_Switch_Will_Switch_In_Place is
   begin
      Asm
        ("mtivor8   %0"        & ASCII.LF & ASCII.HT &
         "mtsprg2   %1",
         Inputs   =>
           (System.Address'Asm_Input
                ("r", In_Place_Context_Switch_To_Agent_Interrupt'Address),
            System.Address'Asm_Input
              ("r", In_Place_Context_Switch_To_Oak_Interrupt'Address)),
         Volatile => True);
   end Context_Switch_Will_Switch_In_Place;

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

   procedure Sleep_Agent_Run_Loop is
   begin

      --  On the e200 we do not have a sleep instruction so we just burn
      --  processor cycles looping
      loop
         null;
      end loop;
   end Sleep_Agent_Run_Loop;

end Oak.Core_Support_Package.Task_Support;
