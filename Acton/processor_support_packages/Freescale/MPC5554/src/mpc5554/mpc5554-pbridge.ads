with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

package MPC5554.PBRIDGE with Preelaborate is

   ----------------------------------------------------------------------------
   --  Memory Addresses
   ----------------------------------------------------------------------------
   PBRIDGE_A_Base_Address  : constant Integer_Address := 16#C3F0_0000#;
   A_MPCR_Offset_Address   : constant Integer_Address := 16#0000#;
   A_PACR0_Offset_Address  : constant Integer_Address := 16#0020#;
   A_OPACR0_Offset_Address : constant Integer_Address := 16#0040#;
   A_OPACR1_Offset_Address : constant Integer_Address := 16#0044#;
   A_OPACR2_Offset_Address : constant Integer_Address := 16#0048#;

   PBRIDGE_B_Base_Address  : constant Integer_Address := 16#FFF0_0000#;
   B_MPCR_Offset_Address   : constant Integer_Address := 16#0000#;
   B_PACR0_Offset_Address  : constant Integer_Address := 16#0020#;
   B_PACR2_Offset_Address  : constant Integer_Address := 16#0028#;
   B_OPACR0_Offset_Address : constant Integer_Address := 16#0040#;
   B_OPACR1_Offset_Address : constant Integer_Address := 16#0044#;
   B_OPACR2_Offset_Address : constant Integer_Address := 16#0048#;
   B_OPACR3_Offset_Address : constant Integer_Address := 16#004C#;

   ----------------------------------------------------------------------------
   --  Hardware Features
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   --  PBRIDGE Types
   ---------------------------------------------------------------------------

   --  Common Types
   type Trust_Type is (Not_Trusted, Trusted);

   --  PBRIDGE A and B Master Privilege Control Register (PBRIDGE_x_MPCR)
   type Privilege_Type is (Forced, Not_Forced);

   type MPCR_Access_Field_Type is record
      Buffer_Writes   : Enable_Type;
      Trusted_Reads   : Trust_Type;
      Trusted_Writes  : Trust_Type;
      Privilege_Level : Privilege_Type;
   end record;

   type Master_Privilege_Control_Type is record
      CPU   : MPCR_Access_Field_Type;
      Nexus : MPCR_Access_Field_Type;
      eDMA  : MPCR_Access_Field_Type;
      EBI   : MPCR_Access_Field_Type;
   end record with Size => Standard'Word_Size;

   --  PBRIDGE A and B Peripheral Access Control Registers (PBRIDGE_x_OPACR)
   --  and Off-Platform Peripheral Access Control Registers (PBRIDGE_x_OPACR)
   type Buffer_Type is (Not_Bufferable, Bufferable);
   type PACR_OPACR_Access_Field_Type is record
      Buffer_Writes      : Buffer_Type;
      Supervisor_Protect : Enable_Type;
      Write_Protect      : Enable_Type;
      Trusted_Protect    : Enable_Type;
   end record;

   type A_PACR0_Type is record
      PBRIDGE_A : PACR_OPACR_Access_Field_Type;
   end record with Size => Standard'Word_Size;

   type A_OPACR0_Type is record
      FMPLL, EBI_Control, Flash_Control, SIU : PACR_OPACR_Access_Field_Type;
   end record with Size => Standard'Word_Size;

   type A_OPACR1_Type is record
      eMIOS : PACR_OPACR_Access_Field_Type;
   end record with Size => Standard'Word_Size;

   type A_OPACR2_Type is record
      eTPU, eTPU_PRAM, eTPU_PRAM_Mirror, eTPU_SCM :
        PACR_OPACR_Access_Field_Type;
   end record with Size => Standard'Word_Size;

   type B_PACR0_Type is record
      PBRIDGE_B, XBAR : PACR_OPACR_Access_Field_Type;
   end record with Size => Standard'Word_Size;

   type B_PACR2_Type is record
      ESCM, eDMA, INTC : PACR_OPACR_Access_Field_Type;
   end record with Size => Standard'Word_Size;

   type B_OPACR0_Type is record
      eQADC, DSPI_A, DSPI_B, DSPI_C, DSPI_D : PACR_OPACR_Access_Field_Type;
   end record with Size => Standard'Word_Size;

   type B_OPACR1_Type is record
      eSCI_A, eSCI_B : PACR_OPACR_Access_Field_Type;
   end record with Size => Standard'Word_Size;

   type B_OPACR2_Type is record
      FlexCAN_A, FlexCAN_B, FlexCAN_C : PACR_OPACR_Access_Field_Type;
   end record with Size => Standard'Word_Size;

   type B_OPACR3_Type is record
      BAM : PACR_OPACR_Access_Field_Type;
   end record with Size => Standard'Word_Size;
   ----------------------------------------------------------------------------
   --  Hardware Respresentations
   ----------------------------------------------------------------------------
   for Trust_Type use (Not_Trusted => 0, Trusted => 1);
   for Privilege_Type use (Forced => 0, Not_Forced => 1);

   for MPCR_Access_Field_Type use record
      Buffer_Writes   at 0 range 0 .. 0;
      Trusted_Reads   at 0 range 1 .. 1;
      Trusted_Writes  at 0 range 2 .. 2;
      Privilege_Level at 0 range 3 .. 3;
   end record;

   for Master_Privilege_Control_Type use record
      CPU   at 0 range 0 .. 3;
      Nexus at 0 range 4 .. 7;
      eDMA  at 0 range 8 .. 11;
      EBI   at 0 range 12 .. 15;
   end record;

   for Buffer_Type use (Not_Bufferable => 0, Bufferable => 1);
   for PACR_OPACR_Access_Field_Type use record
      Buffer_Writes      at 0 range 0 .. 0;
      Supervisor_Protect at 0 range 1 .. 1;
      Write_Protect      at 0 range 2 .. 2;
      Trusted_Protect    at 0 range 3 .. 3;
   end record;

   for A_PACR0_Type use record
      PBRIDGE_A at 0 range 0 .. 3;
   end record;

   for A_OPACR0_Type use record
      FMPLL         at 0 range 0 .. 3;
      EBI_Control   at 0 range 4 .. 7;
      Flash_Control at 0 range 8 .. 11;
      SIU           at 0 range 16 .. 19;
   end record;

   for A_OPACR1_Type use record
      eMIOS at 0 range 0 .. 3;
   end record;

   for A_OPACR2_Type use record
      eTPU             at 0 range 0 .. 3;
      eTPU_PRAM        at 0 range 8 .. 11;
      eTPU_PRAM_Mirror at 0 range 12 .. 15;
      eTPU_SCM         at 0 range 16 .. 19;
   end record;

   for B_PACR0_Type use record
      PBRIDGE_B at 0 range 0 .. 3;
      XBAR      at 0 range 4 .. 7;
   end record;

   for B_PACR2_Type use record
      ESCM at 0 range 0 .. 3;
      eDMA at 0 range 4 .. 7;
      INTC at 0 range 8 .. 11;
   end record;

   for B_OPACR0_Type use record
      eQADC  at 0 range 0 .. 3;
      DSPI_A at 0 range 16 .. 19;
      DSPI_B at 0 range 20 .. 23;
      DSPI_C at 0 range 24 .. 27;
      DSPI_D at 0 range 28 .. 31;
   end record;

   for B_OPACR1_Type use record
      eSCI_A at 0 range 16 .. 19;
      eSCI_B at 0 range 20 .. 23;
   end record;

   for B_OPACR2_Type use record
      FlexCAN_A at 0 range 0 .. 3;
      FlexCAN_B at 0 range 4 .. 7;
      FlexCAN_C at 0 range 8 .. 11;
   end record;

   for B_OPACR3_Type use record
      BAM at 0 range 28 .. 31;
   end record;

   ----------------------------------------------------------------------------
   --  EBI Registers
   ----------------------------------------------------------------------------
   A_Master_Privilege_Control_Register : Master_Privilege_Control_Type;
   for A_Master_Privilege_Control_Register'Address use
     System'To_Address (PBRIDGE_A_Base_Address + A_MPCR_Offset_Address);

   A_Peripheral_Access_Control_Register_0 : A_PACR0_Type;
   for A_Peripheral_Access_Control_Register_0'Address use
     System'To_Address (PBRIDGE_A_Base_Address + A_PACR0_Offset_Address);

   A_Off_Platform_Peripheral_Access_Control_Register_0 : A_OPACR0_Type;
   for A_Off_Platform_Peripheral_Access_Control_Register_0'Address use
     System'To_Address (PBRIDGE_A_Base_Address + A_OPACR0_Offset_Address);

   A_Off_Platform_Peripheral_Access_Control_Register_1 : A_OPACR1_Type;
   for A_Off_Platform_Peripheral_Access_Control_Register_1'Address use
     System'To_Address (PBRIDGE_A_Base_Address + A_OPACR1_Offset_Address);

   A_Off_Platform_Peripheral_Access_Control_Register_2 : A_OPACR2_Type;
   for A_Off_Platform_Peripheral_Access_Control_Register_2'Address use
     System'To_Address (PBRIDGE_A_Base_Address + A_OPACR2_Offset_Address);

   B_Master_Privilege_Control_Register : Master_Privilege_Control_Type;
   for B_Master_Privilege_Control_Register'Address use
     System'To_Address (PBRIDGE_B_Base_Address + B_MPCR_Offset_Address);

   B_Peripheral_Access_Control_Register_0 : B_PACR0_Type;
   for B_Peripheral_Access_Control_Register_0'Address use
     System'To_Address (PBRIDGE_B_Base_Address + B_PACR0_Offset_Address);

   B_Peripheral_Access_Control_Register_2 : B_PACR2_Type;
   for B_Peripheral_Access_Control_Register_2'Address use
     System'To_Address (PBRIDGE_B_Base_Address + B_PACR2_Offset_Address);

   B_Off_Platform_Peripheral_Access_Control_Register_0 : B_OPACR0_Type;
   for B_Off_Platform_Peripheral_Access_Control_Register_0'Address use
     System'To_Address (PBRIDGE_B_Base_Address + B_OPACR0_Offset_Address);

   B_Off_Platform_Peripheral_Access_Control_Register_1 : B_OPACR1_Type;
   for B_Off_Platform_Peripheral_Access_Control_Register_1'Address use
     System'To_Address (PBRIDGE_B_Base_Address + B_OPACR1_Offset_Address);

   B_Off_Platform_Peripheral_Access_Control_Register_2 : B_OPACR2_Type;
   for B_Off_Platform_Peripheral_Access_Control_Register_2'Address use
     System'To_Address (PBRIDGE_B_Base_Address + B_OPACR2_Offset_Address);

   B_Off_Platform_Peripheral_Access_Control_Register_3 : B_OPACR3_Type;
   for B_Off_Platform_Peripheral_Access_Control_Register_3'Address use
     System'To_Address (PBRIDGE_B_Base_Address + B_OPACR3_Offset_Address);

end MPC5554.PBRIDGE;
