with System;
with System.Storage_Elements; use System.Storage_Elements;

package MPC5554.INTC with Preelaborate is

   ----------------------------------------------------------------------------
   --  Memory Addresses
   ----------------------------------------------------------------------------
   INTC_Base_Address              : constant Integer_Address := 16#FFF4_8000#;
   CPR_Offset_Address             : constant Integer_Address := 16#0008#;
   Interrupt_Acknowledge_Addresss : constant Integer_Address := 16#0010#;
   End_Of_Interrupt_Address       : constant Integer_Address := 16#0018#;
   Priority_Select_Offset_Address : constant Integer_Address := 16#0040#;

   ----------------------------------------------------------------------------
   --  Hardware Features
   ----------------------------------------------------------------------------
   type INTC_ID_Type is range 0 .. 307;
   Priority_Select_Register_Size : constant Integer := 8;

   ----------------------------------------------------------------------------
   --  INTC Types
   ---------------------------------------------------------------------------

   --  Common Types
   type MPC5554_Interrupt_Priority is range 0 .. 15
     with Size => Priority_Select_Register_Size;

   --  INTC Module Configuration Register
   type VTES_Type is (Four_Bytes, Eight_Bytes);
   type HVEN_Type is (Software_Vector_Mode, Hardware_Vector_Mode);

   type Module_Config_Type is record
      Vector_Table_Entry_Size : VTES_Type;
      Hardware_Vector_Enable  : HVEN_Type;
   end record;

   --  INTC Interrupt Acknowledge Register
   type Interrupt_Acknowledge_Type is record
      Interrupt_Vector : INTC_ID_Type;
   end record;

   --  INTC End-of-Interrupt Register
   type End_Interrupt_Type is (End_Interrupt);

   ----------------------------------------------------------------------------
   --  Hardware Respresentations
   ----------------------------------------------------------------------------

   for VTES_Type use (Four_Bytes => 0, Eight_Bytes => 1);
   for HVEN_Type use (Software_Vector_Mode => 0, Hardware_Vector_Mode => 1);
   for Module_Config_Type use record
      Vector_Table_Entry_Size at 0 range 26 .. 26;
      Hardware_Vector_Enable  at 0 range 31 .. 31;
   end record;

   for Interrupt_Acknowledge_Type use record
      Interrupt_Vector at 0 range 21 .. 29;
   end record;

   for End_Interrupt_Type use (End_Interrupt => 0);

   ----------------------------------------------------------------------------
   --  INTC Registers
   ----------------------------------------------------------------------------
   Module_Config_Register : Module_Config_Type;
   for Module_Config_Register'Address use
     System'To_Address (INTC_Base_Address);

   Current_Priority_Register : MPC5554_Interrupt_Priority;
   for Current_Priority_Register'Size use 32;
   for Current_Priority_Register'Address use
     System'To_Address (INTC_Base_Address + CPR_Offset_Address);

   Interrupt_Acknowledge_Register : System.Address;
   for Interrupt_Acknowledge_Register'Size use 32;
   for Interrupt_Acknowledge_Register'Address use
     System'To_Address (INTC_Base_Address + Interrupt_Acknowledge_Addresss);

   Interrupt_Acknowledge_Component : Interrupt_Acknowledge_Type;
   for Interrupt_Acknowledge_Component'Size use 32;
   for Interrupt_Acknowledge_Component'Address use
     System'To_Address (INTC_Base_Address + Interrupt_Acknowledge_Addresss);

   End_Of_Interrupt_Register      : End_Interrupt_Type;
   for End_Of_Interrupt_Register'Size use 32;
   for End_Of_Interrupt_Register'Address use
     System'To_Address (INTC_Base_Address + End_Of_Interrupt_Address);

   Priority_Select_Register_Array :
     array (INTC_ID_Type) of aliased MPC5554_Interrupt_Priority;
   for Priority_Select_Register_Array'Address use
     System'To_Address (INTC_Base_Address + Priority_Select_Offset_Address);
   type Priority_Select_Pointer is access all MPC5554_Interrupt_Priority;
end MPC5554.INTC;
