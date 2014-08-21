with System; use System;

package ST.STM32F4.Flash with Preelaborate is

   ----------------------------
   -- Flash Memory Addresses --
   ----------------------------

   FLASH_Base_Address : constant := 16#4002_3C00#;
   ACR_Offset_Address : constant := 16#00#;

   -----------------------
   -- Hardware Features --
   -----------------------

   -----------------
   -- Flash Types --
   -----------------
   type Latency_Type is mod 2 ** 3;

   type Access_Control is record
      Data_Cache_Reset        : Reset_Type;
      Instruction_Cache_Reset : Reset_Type;
      Data_Cache              : Enable_Type;
      Instruction_Cache       : Enable_Type;
      Prefetch                : Enable_Type;
      Wait_States             : Latency_Type;
   end record;

   ------------------------------
   -- Hardware Representations --
   ------------------------------

   for Access_Control use record
      Data_Cache_Reset        at 0 range 12 .. 12;
      Instruction_Cache_Reset at 0 range 11 .. 11;
      Data_Cache              at 0 range 10 .. 10;
      Instruction_Cache       at 0 range  9 .. 9;
      Prefetch                at 0 range  8 .. 8;
      Wait_States             at 0 range  0 .. 2;
   end record;

   ---------------------
   -- Flash Registers --
   ---------------------

   Access_Control_Register : Access_Control
     with Address =>
       System'To_Address (FLASH_Base_Address + ACR_Offset_Address);
end ST.STM32F4.Flash;
