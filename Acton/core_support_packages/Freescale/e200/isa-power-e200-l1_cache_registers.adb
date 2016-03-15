with System.Machine_Code; use System.Machine_Code;

package body ISA.Power.e200.L1_Cache_Registers is
   function Get_L1_Cache_Control_And_Status_Register
     return L1_Cache_Control_And_Status_Register_Type is
      R : L1_Cache_Control_And_Status_Register_Type;
   begin
      Asm
        ("mfspr %0, 1010",
         Outputs   => L1_Cache_Control_And_Status_Register_Type'Asm_Output
           ("=r", R),
         Volatile => True);
      return R;
   end Get_L1_Cache_Control_And_Status_Register;

   procedure Set_L1_Cache_Control_And_Status_Register
     (R : L1_Cache_Control_And_Status_Register_Type) is
   begin
      Asm
        ("msync"      & ASCII.LF & ASCII.HT &
         "isync"      & ASCII.LF & ASCII.HT &
         "mtspr 1010, %0",
         Inputs   => L1_Cache_Control_And_Status_Register_Type'Asm_Input
           ("r", R),
         Volatile => True);
   end Set_L1_Cache_Control_And_Status_Register;

end ISA.Power.e200.L1_Cache_Registers;
