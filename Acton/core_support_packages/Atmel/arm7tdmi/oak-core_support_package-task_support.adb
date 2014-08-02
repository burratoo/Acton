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

with Oak.Processor_Support_Package.Time;

with Ada.Unchecked_Conversion;
with Oak.Core_Support_Package.Time;
with Interfaces; use Interfaces;

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
      Asm ("swi 0", Volatile => True);
   end Context_Switch;

   -----------------------------
   -- Context_Switch_From_Oak --
   ------------------------------

   --  Offically ARM sucks: it seems like when we have more than one return
   --  value eveything gets sent on the stack. So inline it.
   --  Follow up: It works!

   procedure Context_Switch_From_Oak
     (Reason_For_Oak_To_Run : out Run_Reason;
      Message_Address       : out Address)
   is
   begin
      Asm ("swi 0"      & ASCII.LF & ASCII.HT &
           "mov %0, r0" & ASCII.LF & ASCII.HT &
           "mov %1, r1",
           Outputs  => (Run_Reason'Asm_Output ("=r", Reason_For_Oak_To_Run),
                        Address'Asm_Output ("=r", Message_Address)),
           Volatile => True,
           Clobber  => "r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10");
   end Context_Switch_From_Oak;

   ---------------------------
   -- Context_Switch_To_Oak --
   ---------------------------

   procedure Context_Switch_To_Oak
     (Reason_For_Run : in     Run_Reason;
      Message        : in out Oak_Message) is
      pragma Unreferenced (Reason_For_Run, Message);
   begin
      Asm ("swi 0", Volatile => True,
           Clobber     => "r2, r3, r4, r5, r6, r7, r8, r9, r10"); -- , r11");
   end Context_Switch_To_Oak;

   ------------------------------------------
   -- Context_Switch_Save_Callee_Registers --
   ------------------------------------------

   procedure Context_Switch_Save_Callee_Registers is
   begin
      Asm ("swi 0", Volatile => True,
           Clobber => "r4, r5, r6, r7, r8, r9, r10, r12"); -- , r11");
   end Context_Switch_Save_Callee_Registers;

   ------------------------------------------------
   -- Context_Switch_Will_Be_To_Interrupted_Task --
   ------------------------------------------------

   procedure Context_Switch_Will_Be_To_Interrupted_Task is
   begin
      SWI_Vector        := Full_Context_Switch_To_Agent_Interrupt'Address;
      SWI_Return_Vector := Request_Context_Switch_To_Oak_Interrupt'Address;
   end Context_Switch_Will_Be_To_Interrupted_Task;

   -----------------------------------------------
   -- Context_Switch_Will_Be_To_Agent --
   -----------------------------------------------

   procedure Context_Switch_Will_Be_To_Agent is
   begin
      SWI_Vector        := Request_Context_Switch_To_Agent_Interrupt'Address;
      SWI_Return_Vector := Request_Context_Switch_To_Oak_Interrupt'Address;
   end Context_Switch_Will_Be_To_Agent;

   -----------------------------------------
   -- Context_Switch_Will_Switch_In_Place --
   -----------------------------------------

   procedure Context_Switch_Will_Switch_In_Place is
   begin
      SWI_Vector        := In_Place_Context_Switch_To_Agent_Interrupt'Address;
      SWI_Return_Vector := In_Place_Context_Switch_To_Oak_Interrupt'Address;
   end Context_Switch_Will_Switch_In_Place;

   ----------------------
   -- Copy_Oak_Message --
   ----------------------

   procedure Copy_Oak_Message (Destination, Source : in Address) is
      type Message_Array is
        array (1 .. Oak_Message'Object_Size / Storage_Unit / 4)
        of Unsigned_32;
      type Memptr is access Message_Array;
      function To_Memptr is
        new Ada.Unchecked_Conversion (Address, Memptr);
      Dest_P : constant Memptr := To_Memptr (Destination);
      Src_P  : constant Memptr := To_Memptr (Source);
   begin
      for J in Message_Array'Range loop
         Dest_P (J) := Src_P (J);
      end loop;
   end Copy_Oak_Message;

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
      use Oak.Processor_Support_Package.Time;
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
