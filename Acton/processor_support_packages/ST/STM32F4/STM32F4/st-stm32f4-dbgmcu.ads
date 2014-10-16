with System; use System;

package ST.STM32F4.DBGMCU with Preelaborate is

   -----------------------------
   -- DBGMCU Memory Addresses --
   -----------------------------

   DBGMCU_Base_Address : constant := 16#E004_2000#;
   CR_Offset_Address   : constant := 16#4#;

   -----------------------
   -- Hardware Features --
   -----------------------

   ------------------
   -- DBGMCU Types --
   ------------------

   type Trace_Modes is (Asynchronous, Synchronous_1,
                        Synchronous_2, Synchronous_4);

   type Configuration is record
      Trace_Mode         : Trace_Modes;
      Trace              : Enable_Type;
      Debug_Standby_Mode : Boolean;
      Debug_Stop_Mode    : Boolean;
      Debug_Sleep_Mode   : Boolean;
   end record with Size => 32;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Configuration use record
      Trace_Mode         at 0 range 6 .. 7;
      Trace              at 0 range 5 .. 5;
      Debug_Standby_Mode at 0 range 2 .. 2;
      Debug_Stop_Mode    at 0 range 1 .. 1;
      Debug_Sleep_Mode   at 0 range 0 .. 0;
   end record;

   ----------------------
   -- DBGMCU Registers --
   ----------------------

   Configuration_Register : Configuration
     with Address =>
       System'To_Address (DBGMCU_Base_Address + CR_Offset_Address);
end ST.STM32F4.DBGMCU;
