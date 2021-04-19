------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    ARM CORTEX M4F                                    --
--                                                                                      --
--                                ISA.ARM.CORTEX_M4.ITM                                 --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with System;     use System;
with Interfaces; use Interfaces;

package ISA.ARM.Cortex_M4.ITM with Preelaborate is

   --------------------------
   -- ITM Memory Addresses --
   --------------------------

   ITM_Base_Address    : constant := 16#E000_0000#;
   STIM_Offset_Address : constant := 16#000#;
   TER_Offset_Address  : constant := 16#E00#;
   TPR_Offset_Address  : constant := 16#E40#;
   TCR_Offset_Address  : constant := 16#E80#;
   LAR_Offset_Address  : constant := 16#FB0#;
   LSR_Offset_Address  : constant := 16#FB4#;

   -----------------------
   -- Hardware Features --
   -----------------------

   type Stimulus_Port is mod 2 ** 5;
   type Stimulus_Group is mod 2 ** 5;

   ---------------
   -- ITM Types --
   ---------------

   type Stimulus_Write_32 is record
      Data : Unsigned_32;
   end record;

   type Stimulus_Write_8 is record
      Data : Unsigned_8;
   end record with Size => 32;

   type Stimulus_Write_16 is record
      Data : Unsigned_16;
   end record with Size => 32;

   type Stimulus_Write_Set_32 is array (Stimulus_Port) of Stimulus_Write_32;

   type Stimulus_Write_Set_8 is array (Stimulus_Port) of Stimulus_Write_8;

   type Stimulus_Write_Set_16 is array (Stimulus_Port) of Stimulus_Write_16;

   type Stimulus_Read is record
      FIFO_Ready : Boolean;
   end record;

   type Stimulus_Read_Set is array (Stimulus_Port) of Stimulus_Read;

   type Trace_Enable_Set is array (Stimulus_Port) of Enable_Type
     with Pack;

   type Access_Control is (Unprivileged_Allowed, Privileged_Only);
   type Access_Control_Set is array (Stimulus_Group) of Access_Control
     with Pack;

   type Trace_Bus_Identifier is mod 2 ** 7;
   type GTSFREQ is (Disable, After_128_Cycles, After_8192_Cycles,
                    Every_Packet);
   --  Note the cycles are approximate
   type Timestamp_Prescaler is (None, Divide_By_4, Divide_By_16,
                                Divide_By_64);
   type ITM_Clock_Source is (System_Clock, TPIU_Asynchronous_Clock);

   type Trace_Control is record
      ITM_Busy                   : Boolean;
      Trace_Bus_Id               : Trace_Bus_Identifier;
      Global_Timestamp_Frequency : GTSFREQ;
      Local_Timestamp_Prescaler  : Timestamp_Prescaler;
      Timestamp_Clock_Source     : ITM_Clock_Source;
      Forward_DWT_Packets        : Enable_Type;
      Synchronization_Packets    : Enable_Type;
      Local_Timestamp_Generation : Enable_Type;
      ITM                        : Enable_Type;
   end record with Size => 32;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Stimulus_Write_32 use record
      Data at 0 range 0 .. 31;
   end record;

   for Stimulus_Write_8 use record
      Data at 0 range 0 .. 7;
   end record;

   for Stimulus_Write_16 use record
      Data at 0 range 0 .. 15;
   end record;

   for Stimulus_Read use record
      FIFO_Ready at 0 range 0 .. 0;
   end record;

   for Trace_Control use record
      ITM_Busy                   at 0 range 23 .. 23;
      Trace_Bus_Id               at 0 range 16 .. 22;
      Global_Timestamp_Frequency at 0 range 10 .. 11;
      Local_Timestamp_Prescaler  at 0 range  8 .. 9;
      Timestamp_Clock_Source     at 0 range  4 .. 4;
      Forward_DWT_Packets        at 0 range  3 .. 3;
      Synchronization_Packets    at 0 range  2 .. 2;
      Local_Timestamp_Generation at 0 range  1 .. 1;
      ITM                        at 0 range  0 .. 0;
   end record;

   -------------------
   -- ITM Registers --
   -------------------

   Stimulus_Port_Register_32 : Stimulus_Write_Set_32
     with Address =>
       System'To_Address (ITM_Base_Address + STIM_Offset_Address);

   Stimulus_Port_Register_8 : Stimulus_Write_Set_8
     with Address =>
       System'To_Address (ITM_Base_Address + STIM_Offset_Address);

   Stimulus_Port_Register_16 : Stimulus_Write_Set_16
     with Address =>
       System'To_Address (ITM_Base_Address + STIM_Offset_Address);

   Stimulus_Port_Status_Register : Stimulus_Read_Set
     with Address =>
       System'To_Address (ITM_Base_Address + STIM_Offset_Address);

   Stimulus_Port_Enable_Register : Trace_Enable_Set
     with Address => System'To_Address (ITM_Base_Address + TER_Offset_Address);

   Trace_Privilege_Register : Access_Control_Set
     with Address => System'To_Address (ITM_Base_Address + TPR_Offset_Address);

   Trace_Control_Register : Trace_Control
     with Address => System'To_Address (ITM_Base_Address + TCR_Offset_Address);

   Lock_Access_Register : Unsigned_32
     with Address => System'To_Address (ITM_Base_Address + LAR_Offset_Address);

   Lock_Status_Register : Unsigned_32
     with Address => System'To_Address (ITM_Base_Address + LSR_Offset_Address);
end ISA.ARM.Cortex_M4.ITM;
