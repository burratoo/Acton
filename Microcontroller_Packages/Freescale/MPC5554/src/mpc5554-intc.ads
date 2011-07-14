with System.Storage_Elements; use System.Storage_Elements;

package MPC5554.INTC is

   ----------------------------------------------------------------------------
   --  Memory Addresses
   ----------------------------------------------------------------------------
   INTC_Base_Address              : constant Integer_Address := 16#FFF4_8000#;
   CPR_Offset_Address             : constant Integer_Address := 16#0008#;
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
   type Priority_Type is range 0 .. 15;
   for Priority_Type'Size use Priority_Select_Register_Size;

   --  INTC Module Configuration Register
   type HVEN_Type is (Software_Vector_Mode, Hardware_Vector_Mode);

   type Module_Config_Type is record
      Hardware_Vector_Enable : HVEN_Type;
   end record;

   ----------------------------------------------------------------------------
   --  Hardware Respresentations
   ----------------------------------------------------------------------------

   for HVEN_Type use (Software_Vector_Mode => 0, Hardware_Vector_Mode => 1);
   for Module_Config_Type use record
      Hardware_Vector_Enable at 0 range 31 .. 31;
   end record;

   ----------------------------------------------------------------------------
   --  INTC Registers
   ----------------------------------------------------------------------------
   Module_Config_Register : Module_Config_Type;
   for Module_Config_Register'Address use To_Address (INTC_Base_Address);

   Current_Priority_Register : Priority_Type;
   for Current_Priority_Register'Size use 32;
   for Current_Priority_Register'Address use
     To_Address (INTC_Base_Address + CPR_Offset_Address);

   Priority_Select_Register_Array :
     array (INTC_ID_Type) of aliased Priority_Type;
   for Priority_Select_Register_Array'Address use
     To_Address (INTC_Base_Address + Priority_Select_Offset_Address);
   type Priority_Select_Pointer is access all Priority_Type;
end MPC5554.INTC;
