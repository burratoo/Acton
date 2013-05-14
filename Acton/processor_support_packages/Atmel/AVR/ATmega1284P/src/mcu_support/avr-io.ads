with System.Storage_Elements; use System.Storage_Elements;

package AVR.IO with Preelaborate is

   ----------------------------------------------------------------------------
   --  Memory Address
   ----------------------------------------------------------------------------

   PINA_Address  : constant Integer_Address := 16#20#;
   DDRA_Address  : constant Integer_Address := 16#21#;
   PORTA_Address : constant Integer_Address := 16#22#;
   PINB_Address  : constant Integer_Address := 16#23#;
   DDRB_Address  : constant Integer_Address := 16#24#;
   PORTB_Address : constant Integer_Address := 16#25#;
   PINC_Address  : constant Integer_Address := 16#26#;
   DDRC_Address  : constant Integer_Address := 16#27#;
   PORTC_Address : constant Integer_Address := 16#28#;

   ----------------------------------------------------------------------------
   --  IO Types
   ----------------------------------------------------------------------------

   type Pin_Numbers is range 0 .. 7;
   type Direction_Type is (Input, Output);

   type IO_Port_Type is array (Pin_Numbers) of Boolean with Pack;
   type Direction_Port_Type is array (Pin_Numbers) of Direction_Type
     with Pack;

   ----------------------------------------------------------------------------
   --  Hardware Respresentations
   ----------------------------------------------------------------------------

   for Direction_Type use (Input => 0, Output => 1);

   ----------------------------------------------------------------------------
   --  IO Registers
   ----------------------------------------------------------------------------

   Port_A_Data_Register : IO_Port_Type;
   for Port_A_Data_Register'Address use
     System'To_Address (PORTA_Address);

   Port_A_Data_Direction_Register : Direction_Port_Type;
   for Port_A_Data_Direction_Register'Address use
     System'To_Address (DDRA_Address);

   Port_A_Input_Pin_Address : IO_Port_Type;
   for Port_A_Input_Pin_Address'Address use
     System'To_Address (PINA_Address);

   Port_B_Data_Register : IO_Port_Type;
   for Port_B_Data_Register'Address use
     System'To_Address (PORTB_Address);

   Port_B_Data_Direction_Register : Direction_Port_Type;
   for Port_B_Data_Direction_Register'Address use
     System'To_Address (DDRB_Address);

   Port_B_Input_Pin_Address : IO_Port_Type;
   for Port_B_Input_Pin_Address'Address use
     System'To_Address (PINB_Address);

   Port_C_Data_Register : IO_Port_Type;
   for Port_C_Data_Register'Address use
     System'To_Address (PORTC_Address);

   Port_C_Data_Direction_Register : Direction_Port_Type;
   for Port_C_Data_Direction_Register'Address use
     System'To_Address (DDRC_Address);

   Port_C_Input_Pin_Address : IO_Port_Type;
   for Port_C_Input_Pin_Address'Address use
     System'To_Address (PINC_Address);
end AVR.IO;
