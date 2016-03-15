package ISA.Power.e200.L1_Cache_Registers with Pure is

   type Cache_Write_Type is (Write_Through, Copy_Back);

   type L1_Cache_Control_And_Status_Register_Type is record
      Way_Instruction             : Disable_Type;
      Way_Data                    : Disable_Type;
      Additional_Ways_Instruction : Disable_Type;
      Additional_Ways_Data        : Disable_Type;
      Cache_Write_Mode            : Cache_Write_Type;
      Push_Buffer                 : Disable_Type;
      Store_Buffer                : Disable_Type;
      Streaming                   : Disable_Type;
      Cache_Parity_Checking       : Enable_Type;
      Cache_Unable_To_Lock        : Boolean;
      Cache_Lock_Overflow         : Boolean;
      Cache_Lock_Bits_Clear       : Boolean;
      Cache_Operation_Aborted     : Boolean;
      Cache_Invalidate            : Boolean;
      Cache_Enable                : Boolean;
   end record;

   for Cache_Write_Type use (Write_Through => 0, Copy_Back => 1);

   for L1_Cache_Control_And_Status_Register_Type use record
      Way_Instruction             at 0 range 0 .. 3;
      Way_Data                    at 0 range 4 .. 7;
      Additional_Ways_Instruction at 0 range 8 .. 8;
      Additional_Ways_Data        at 0 range 9 .. 9;
      Cache_Write_Mode            at 0 range 11 .. 11;
      Push_Buffer                 at 0 range 12 .. 12;
      Store_Buffer                at 0 range 13 .. 13;
      Streaming                   at 0 range 14 .. 14;
      Cache_Parity_Checking       at 0 range 15 .. 15;
      Cache_Unable_To_Lock        at 0 range 21 .. 21;
      Cache_Lock_Overflow         at 0 range 22 .. 22;
      Cache_Lock_Bits_Clear       at 0 range 23 .. 23;
      Cache_Operation_Aborted     at 0 range 29 .. 29;
      Cache_Invalidate            at 0 range 30 .. 30;
      Cache_Enable                at 0 range 31 .. 31;
   end record;

   function Get_L1_Cache_Control_And_Status_Register
     return L1_Cache_Control_And_Status_Register_Type with Inline;

   procedure Set_L1_Cache_Control_And_Status_Register
     (R : L1_Cache_Control_And_Status_Register_Type) with Inline;

end ISA.Power.e200.L1_Cache_Registers;
