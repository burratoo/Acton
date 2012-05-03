with System.Storage_Elements; use System.Storage_Elements;

package MPC5554.EBI with Preelaborate is

   ----------------------------------------------------------------------------
   --  Memory Addresses
   ----------------------------------------------------------------------------
   EBI_Base_Address    : constant Integer_Address := 16#C3F8_4000#;
   MCR_Offset_Address  : constant Integer_Address := 16#0000#;
   TESR_Offset_Address : constant Integer_Address := 16#0008#;
   BMCR_Offset_Address : constant Integer_Address := 16#000C#;
   BR0_Offset_Address  : constant Integer_Address := 16#0010#;
   OR0_Offset_Address  : constant Integer_Address := 16#0014#;
   BR1_Offset_Address  : constant Integer_Address := 16#0018#;
   OR1_Offset_Address  : constant Integer_Address := 16#001C#;
   BR2_Offset_Address  : constant Integer_Address := 16#0020#;
   OR2_Offset_Address  : constant Integer_Address := 16#0024#;
   BR3_Offset_Address  : constant Integer_Address := 16#0028#;
   OR3_Offset_Address  : constant Integer_Address := 16#002C#;

   ----------------------------------------------------------------------------
   --  Hardware Features
   ----------------------------------------------------------------------------
   type Sixteen_Bit_Address_Type is range 0 .. 16#FFFF#;
   ----------------------------------------------------------------------------
   --  EBI Types
   ---------------------------------------------------------------------------

   --  Common Types
   type Error_Type is (No_Error, Error);
   type DBM_Type is (S32_Bit, S16_Bit);
   --  EBI Module Configuration Register (EBI_MCR)
   type SIZE_Type is (Four_Byte, One_Byte, Two_Byte);
   type EARP_Type is (MCU, Equal, External);

   type Module_Configuration_Type is record
      Size_Enable                           : Enable_Type;
      Size                                  : SIZE_Type;
      Automatic_CLKOUT_Gating_Enable        : Enable_Type;
      External_Master_Mode                  : Enable_Type;
      External_Arbitration                  : Enable_Type;
      External_Arbitration_Requrest_Priorty : EARP_Type;
      Module_Disable_Mode                   : Enable_Type;
      Data_Bus_Mode                         : DBM_Type;
   end record;

   --  EBI Error Transfer Status Register (EBI_TESR)
   type Transfer_Error_Status_Type is record
      Transfer_Error_Ack_Flag  : Error_Type;
      Bus_Monitor_Timeout_Flag : Error_Type;
   end record;

   --  EBI Bus Monitor Control Register (EBI_BMCR)
   type BMT_Type is range 0 .. 255;

   type Bus_Monitor_Control_Type is record
      Bus_Monitor_Timing : BMT_Type;
      Bus_Monitor_Enable : Enable_Type;
   end record;

   --  EBI Base Register 0 - 3 (EBI_BRn)

   type PS_Type is (S32_Bit, S16_Bit);
   type BL_Type is (Eight_Word, Four_Word);
   type WEBS_Type is (Write_Enable, Byte_Enable);
   type Valid_Type is (Invalid, Valid);
   type TBDIP_Type is (Burst, Before);

   type Base_Register_Type is record
      Base_Address                  : Sixteen_Bit_Address_Type;
      Port_Size                     : PS_Type;
      Burst_Length                  : BL_Type;
      Write_Enable_Byte_Select      : WEBS_Type;
      Toggle_Burst_Data_In_Progress : TBDIP_Type;
      Burst_Inhibit                 : Enable_Type;
      Valid_Bit                     : Valid_Type;
   end record;

   --  EBI Option Register (EBI_ORn)
   type AM_Type is (
      AM_512M,
      AM_256M,
      AM_128M,
      AM_64M,
      AM_32M,
      AM_16M,
      AM_8M,
      AM_4M,
      AM_2M,
      AM_1M,
      AM_512k,
      AM_256K,
      AM_128K,
      AM_64K);
   type SCY_Type is range 0 .. 2#111#;
   type BSCY_Type is range 0 .. 3;

   type Option_Register_Type is record
      Address_Mask           : AM_Type;
      Cycle_Length           : SCY_Type;
      Burst_Beat_Length_Type : BSCY_Type;
   end record;

   ----------------------------------------------------------------------------
   --  Hardware Respresentations
   ----------------------------------------------------------------------------
   for Error_Type use (No_Error => 0, Error => 1);

   for SIZE_Type use
     (Four_Byte => 2#00#,
      One_Byte  => 2#01#,
      Two_Byte  => 2#10#);

   for EARP_Type use (MCU => 2#00#, Equal => 2#01#, External => 2#10#);
   for DBM_Type use (S32_Bit => 0, S16_Bit => 1);

   for Module_Configuration_Type use record
      Size_Enable                           at 0 range 5 .. 5;
      Size                                  at 0 range 6 .. 7;
      Automatic_CLKOUT_Gating_Enable        at 0 range 16 .. 16;
      External_Master_Mode                  at 0 range 17 .. 17;
      External_Arbitration                  at 0 range 18 .. 18;
      External_Arbitration_Requrest_Priorty at 0 range 19 .. 20;
      Module_Disable_Mode                   at 0 range 25 .. 25;
      Data_Bus_Mode                         at 0 range 31 .. 31;
   end record;

   for Transfer_Error_Status_Type use record
      Transfer_Error_Ack_Flag  at 0 range 30 .. 30;
      Bus_Monitor_Timeout_Flag at 0 range 31 .. 31;
   end record;

   for Bus_Monitor_Control_Type use record
      Bus_Monitor_Timing at 0 range 16 .. 23;
      Bus_Monitor_Enable at 0 range 24 .. 24;
   end record;

   for PS_Type use (S32_Bit => 0, S16_Bit => 1);
   for BL_Type use (Eight_Word => 0, Four_Word => 1);
   for WEBS_Type use (Write_Enable => 0, Byte_Enable => 1);
   for Valid_Type use (Invalid => 0, Valid => 1);
   for TBDIP_Type use (Burst => 0, Before => 1);

   for Base_Register_Type use record
      Base_Address                  at 0 range 0 .. 16;
      Port_Size                     at 0 range 20 .. 20;
      Burst_Length                  at 0 range 25 .. 25;
      Write_Enable_Byte_Select      at 0 range 26 .. 26;
      Toggle_Burst_Data_In_Progress at 0 range 27 .. 27;
      Burst_Inhibit                 at 0 range 30 .. 30;
      Valid_Bit                     at 0 range 31 .. 31;
   end record;

   for AM_Type use
     (AM_512M => 16#E000#,
      AM_256M => 16#F000#,
      AM_128M => 16#F800#,
      AM_64M  => 16#FC00#,
      AM_32M  => 16#FE00#,
      AM_16M  => 16#FF00#,
      AM_8M   => 16#FF80#,
      AM_4M   => 16#FFC0#,
      AM_2M   => 16#FFE0#,
      AM_1M   => 16#FFF0#,
      AM_512k => 16#FFF8#,
      AM_256K => 16#FFFC#,
      AM_128K => 16#FFFE#,
      AM_64K  => 16#FFFF#);

   for Option_Register_Type use record
      Address_Mask           at 0 range 0 .. 16;
      Cycle_Length           at 0 range 24 .. 27;
      Burst_Beat_Length_Type at 0 range 29 .. 30;
   end record;
   ----------------------------------------------------------------------------
   --  EBI Registers
   ----------------------------------------------------------------------------

   Module_Configuration_Register : Module_Configuration_Type;
   for Module_Configuration_Register'Address use
     System'To_Address (EBI_Base_Address + MCR_Offset_Address);

   Transfer_Error_Status_Register : Transfer_Error_Status_Type;
   for Transfer_Error_Status_Register'Address use
     System'To_Address (EBI_Base_Address + TESR_Offset_Address);

   Bus_Monitor_Control_Register : Bus_Monitor_Control_Type;
   for Bus_Monitor_Control_Register'Address use
     System'To_Address (EBI_Base_Address + BMCR_Offset_Address);

   Base_Register_Bank_0 : Base_Register_Type;
   for Base_Register_Bank_0'Address use
     System'To_Address (EBI_Base_Address + BR0_Offset_Address);
   Option_Register_Bank_0 : Option_Register_Type;
   for Option_Register_Bank_0'Address use
     System'To_Address (EBI_Base_Address + OR0_Offset_Address);

   Base_Register_Bank_1 : Base_Register_Type;
   for Base_Register_Bank_1'Address use
     System'To_Address (EBI_Base_Address + BR1_Offset_Address);
   Option_Register_Bank_1 : Option_Register_Type;
   for Option_Register_Bank_1'Address use
     System'To_Address (EBI_Base_Address + OR1_Offset_Address);

   Base_Register_Bank_2 : Base_Register_Type;
   for Base_Register_Bank_2'Address use
     System'To_Address (EBI_Base_Address + BR2_Offset_Address);
   Option_Register_Bank_2 : Option_Register_Type;
   for Option_Register_Bank_2'Address use
     System'To_Address (EBI_Base_Address + OR2_Offset_Address);

   Base_Register_Bank_3 : Base_Register_Type;
   for Base_Register_Bank_3'Address use
     System'To_Address (EBI_Base_Address + BR3_Offset_Address);
   Option_Register_Bank_3 : Option_Register_Type;
   for Option_Register_Bank_3'Address use
     System'To_Address (EBI_Base_Address + OR3_Offset_Address);

end MPC5554.EBI;
