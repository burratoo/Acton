with System;
with System.Storage_Elements; use System.Storage_Elements;

package MPC5554.Flash with Preelaborate is

   ----------------------------------------------------------------------------
   --  Memory Addresses
   ----------------------------------------------------------------------------
   Flash_Base_Address    : constant Integer_Address := 16#C3F8_8000#;
   MCR_Offset_Address    : constant Integer_Address := 16#0000#;
   LMLR_Offset_Address   : constant Integer_Address := 16#0004#;
   HLR_Offset_Address    : constant Integer_Address := 16#0008#;
   SLMLR_Offset_Address  : constant Integer_Address := 16#000C#;
   LMSR_Offset_Address   : constant Integer_Address := 16#0010#;
   HSR_Offset_Address    : constant Integer_Address := 16#0014#;
   AR_Offset_Address     : constant Integer_Address := 16#0018#;
   BIUCR_Offset_Address  : constant Integer_Address := 16#001C#;
   BIUAPR_Offset_Address : constant Integer_Address := 16#0020#;

   ----------------------------------------------------------------------------
   --  Hardware Features
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   --  Flash Types
   ----------------------------------------------------------------------------

   --  Module Configuration Register (FLASH_MCR)

   type PEAS_Type is (Main_Space_Enabled, Shadow_Space_Enabled);
   type Execute_Type is (Not_Executing, Executing);

   type Module_Configuration_Type is record
      ECC_Error_Event              : Occurred_Type;
      Read_While_Write_Error_Event : Occurred_Type;
      Progream_Erase_Access_Space  : PEAS_Type;
      Done                         : Yes_No_Type;
      Program_Erase                : Success_Type;
      Stop_Mode                    : Enable_Type;
      Program                      : Execute_Type;
      Program_Suspend              : Yes_No_Type;
      Erase                        : Execute_Type;
      Erase_Suspend                : Yes_No_Type;
      High_Voltage                 : Enable_Type;
   end record;

   --  Low-/Mid-Address Space Block Locking Register (FLASH_LMLR)

   type Lock_Enable_Type is (Not_Editable, Editable);
   type Block_Lock_Array is array (Register_Elements range <>)
     of Lock_Enable_Type with Pack;

   type Low_Mid_Address_Space_Block_Locking_Type is record
      Locks     : Lock_Enable_Type;
      Shadow_Lock : Enable_Type;
      Mid_Address_Locks : Block_Lock_Array (0 .. 3);
      Low_Address_Locks : Block_Lock_Array (0 .. 15);
   end record;

   --  High-Address Space Block Locking Register (FLASH_LMLR)

   type High_Address_Space_Block_Locking_Type is record
      Locks     : Lock_Enable_Type;
      High_Address_Locks : Block_Lock_Array (0 .. 27);
   end record;

   --  Secondary Low-/Mid-Address Space Block Locking Register (FLASH_SLMLR)

   type Secondary_Low_Mid_Address_Space_Block_Locking_Type is record
      Locks     : Lock_Enable_Type;
      Shadow_Lock : Enable_Type;
      Mid_Address_Locks : Block_Lock_Array (0 .. 3);
      Low_Address_Locks : Block_Lock_Array (0 .. 15);
   end record;

   --  Low-/Mid-Address Address Space Block Select Register (FLASH_LMSR)

   type Select_Type is (Not_Selected, Selected);
   type Block_Select_Array is array (Register_Elements range <>)
     of Select_Type with Pack;

   type Low_Mid_Address_Space_Block_Select_Type is record
      Mid_Address_Blocks : Block_Select_Array (0 .. 3);
      Low_Address_Blocks : Block_Select_Array (0 .. 15);
   end record;

   --  High-Address Address Space Block Select Register (FLASH_HSR)

   type High_Address_Space_Block_Select_Type is record
      High_Address_Blocks : Block_Select_Array (0 .. 27);
   end record;

   --  Flash Bus Interface Unit Control Register (FLASH_BIUCR)
   type Hold_Cycles is (Reserved,
                        One_Hold_Cycle,
                        Two_Hold_Cycle,
                        Three_Hold_Cycle,
                        Four_Hold_Cycle,
                        Five_Hold_Cycle,
                        Six_Hold_Cycle,
                        No_Pipelining);
   type Wait_States is range 0 .. 7;
   subtype Write_Wait_States is Wait_States range 1 .. 3;
   type Prefetch_Type is (No_Prefetching, Prefetch_On_Burst, Prefetch_On_Any);
   type Prefetch_Limit_Type is range 0 .. 6;

   type Flash_Bus_Interface_Unit_Control_Type is record
      Master_Prefetch : Enable_Array (0 .. 3);
      Address_Pipeline_Control : Hold_Cycles;
      Write_Wait_State_Control : Write_Wait_States;
      Read_Wait_State_Control  : Wait_States;
      Data_Prefetch            : Prefetch_Type;
      Instruction_Prefetch     : Prefetch_Type;
      Prefetch_Limit           : Prefetch_Limit_Type;
      FBIU_Line_Read_Buffers   : Enable_Type;
   end record;

   --  Flash Bus Interface Unit Access Protection Register (FLASH_BIUAPR)

   type Access_Protection_Type is (No_Access, Read_Access, Write_Access,
                                   Read_Write_Access);
   type Access_Protection_Array is array (Integer range <>)
     of Access_Protection_Type with Pack;

   type Flash_Bus_Interface_Unit_Access_Protection_Type is record
      Master_Access_Protection : Access_Protection_Array (0 .. 3);
   end record;

   ----------------------------------------------------------------------------
   --  Hardware Respresentations
   ----------------------------------------------------------------------------

   for PEAS_Type use (Main_Space_Enabled => 0, Shadow_Space_Enabled => 1);
   for Execute_Type use (Not_Executing => 0, Executing => 1);

   for Module_Configuration_Type use record
      ECC_Error_Event              at 0 range 16 .. 16;
      Read_While_Write_Error_Event at 0 range 17 .. 17;
      Progream_Erase_Access_Space  at 0 range 20 .. 20;
      Done                         at 0 range 21 .. 21;
      Program_Erase                at 0 range 22 .. 22;
      Stop_Mode                    at 0 range 25 .. 25;
      Program                      at 0 range 27 .. 27;
      Program_Suspend              at 0 range 28 .. 28;
      Erase                        at 0 range 29 .. 29;
      Erase_Suspend                at 0 range 30 .. 30;
      High_Voltage                 at 0 range 31 .. 31;
   end record;

   for Lock_Enable_Type use (Not_Editable => 0, Editable => 1);

   for Low_Mid_Address_Space_Block_Locking_Type use record
      Locks     at 0 range 0 .. 0;
      Shadow_Lock at 0 range 11 .. 11;
      Mid_Address_Locks at 0 range 12 .. 15;
      Low_Address_Locks at 0 range 16 .. 31;
   end record;

   for High_Address_Space_Block_Locking_Type use record
      Locks     at 0 range 0 .. 0;
      High_Address_Locks at 0 range 4 .. 31;
   end record;

   for Secondary_Low_Mid_Address_Space_Block_Locking_Type use record
      Locks     at 0 range 0 .. 0;
      Shadow_Lock at 0 range 11 .. 11;
      Mid_Address_Locks at 0 range 12 .. 15;
      Low_Address_Locks at 0 range 16 .. 31;
   end record;

   for Select_Type use (Not_Selected => 0, Selected => 1);

   for Low_Mid_Address_Space_Block_Select_Type use record
      Mid_Address_Blocks at 0 range 12 .. 15;
      Low_Address_Blocks at 0 range 16 .. 31;
   end record;

   for High_Address_Space_Block_Select_Type use record
      High_Address_Blocks at 0 range 4 .. 31;
   end record;

   for Flash_Bus_Interface_Unit_Control_Type use record
      Master_Prefetch at 0 range 12 .. 15;
      Address_Pipeline_Control at 0 range 16 .. 18;
      Write_Wait_State_Control at 0 range 19 .. 20;
      Read_Wait_State_Control  at 0 range 21 .. 23;
      Data_Prefetch            at 0 range 24 .. 25;
      Instruction_Prefetch     at 0 range 26 .. 27;
      Prefetch_Limit           at 0 range 28 .. 30;
      FBIU_Line_Read_Buffers   at 0 range 31 .. 31;
   end record;

   for Access_Protection_Type use (No_Access => 0, Read_Access => 1,
                                   Write_Access => 2, Read_Write_Access => 3);

   for Flash_Bus_Interface_Unit_Access_Protection_Type use record
      Master_Access_Protection at 0 range 24 .. 31;
   end record;
   ----------------------------------------------------------------------------
   --  Flash Registers
   ----------------------------------------------------------------------------

   Module_Configuration_Register : Module_Configuration_Type
     with Address => System'To_Address (Flash_Base_Address +
                                          MCR_Offset_Address);

   Low_Mid_Address_Space_Block_Locking_Register :
     Low_Mid_Address_Space_Block_Locking_Type
       with Address => System'To_Address (Flash_Base_Address +
                                            LMLR_Offset_Address);

   High_Address_Space_Block_Locking_Register :
     High_Address_Space_Block_Locking_Type
       with Address => System'To_Address (Flash_Base_Address +
                                            HLR_Offset_Address);

   Secondary_Low_Mid_Address_Space_Block_Locking_Type_Register :
     Secondary_Low_Mid_Address_Space_Block_Locking_Type
       with Address => System'To_Address (Flash_Base_Address +
                                            SLMLR_Offset_Address);

   Low_Mid_Address_Space_Block_Select_Register :
     Low_Mid_Address_Space_Block_Select_Type
       with Address => System'To_Address (Flash_Base_Address +
                                            LMSR_Offset_Address);

   High_Address_Space_Block_Select_Register :
     High_Address_Space_Block_Select_Type
       with Address => System'To_Address (Flash_Base_Address +
                                            HSR_Offset_Address);

   Address_Register : System.Address
   with Address => System'To_Address (Flash_Base_Address +
                                        AR_Offset_Address);

   Flash_Bus_Interface_Unit_Control_Register :
     Flash_Bus_Interface_Unit_Control_Type
       with Address => System'To_Address (Flash_Base_Address +
                                            BIUCR_Offset_Address);
end MPC5554.Flash;
