with System;
with System.Storage_Elements; use System.Storage_Elements;

package MPC5554.eDMA with Preelaborate is

   ----------------------------------------------------------------------------
   --  Memory Addresses
   ----------------------------------------------------------------------------
   eDMA_Base_Address    : constant Integer_Address := 16#FFF4_4000#;
   CR_Offset_Address    : constant Integer_Address := 16#0000#;
   ESR_Offset_Address   : constant Integer_Address := 16#0004#;
   ERQR_Offset_Address  : constant Integer_Address := 16#0008#;
   EEIR_Offset_Address  : constant Integer_Address := 16#0010#;
   SERQR_Offset_Address : constant Integer_Address := 16#0018#;
   CERQR_Offset_Address : constant Integer_Address := 16#0019#;
   SEEIR_Offset_Address : constant Integer_Address := 16#001A#;
   CEEIR_Offset_Address : constant Integer_Address := 16#001B#;
   CIRQR_Offset_Address : constant Integer_Address := 16#001C#;
   CER_Offset_Address   : constant Integer_Address := 16#001D#;
   SSBR_Offset_Address  : constant Integer_Address := 16#001E#;
   CDSBR_Offset_Address : constant Integer_Address := 16#001F#;
   IRQR_Offset_Address  : constant Integer_Address := 16#0020#;
   ER_Offset_Address    : constant Integer_Address := 16#0028#;
   CPR_Offset_Address   : constant Integer_Address := 16#0100#;
   TCD_Offset_Address   : constant Integer_Address := 16#1000#;

   ----------------------------------------------------------------------------
   --  Hardware Features
   ----------------------------------------------------------------------------
   type DMA_Channel_Number_With_Set_All is range 0 .. 127;

   DMA_Size : constant DMA_Channel_Number_With_Set_All := 64;
   subtype DMA_Channel_Number is DMA_Channel_Number_With_Set_All range
      0 .. (DMA_Size - 1);

   ----------------------------------------------------------------------------
   --  SIU Types
   ---------------------------------------------------------------------------

   --  Common Types
   type DMA_Enable_Bit_Array is
     array (DMA_Channel_Number'Range) of Enable_Type
     with Pack, Size => DMA_Size;

   type DMA_Occured_Bit_Array is
     array (DMA_Channel_Number'Range) of Occurred_Type
     with Pack, Size => DMA_Size;

   type Active_Type is (Clear, Active);
   type DMA_Active_Bit_Array is
     array (DMA_Channel_Number'Range) of Enable_Type
     with Pack, Size => DMA_Size;

   --  eDMA Control Register (EDMA_CR)
   type Group_Priority_Type is range 0 .. 3;
   type Arbitration_Type is (Fixed, Round_Robin);

   type Control_Type is record
      Channel_Group_3_Priority, Channel_Group_2_Priority,
Channel_Group_1_Priority, Channel_Group_0_Priority : Group_Priority_Type;
      Group_Arbitration, Channel_Arbitration : Arbitration_Type;
      Debug : Enable_Type;
   end record;

   --  eDMA Error Status Register
   type Error_Status_Type is record
      Valid_Error               : Occurred_Type;
      Group_Priority_Error      : Occurred_Type;
      Channel_Priority_Error    : Occurred_Type;
      Error_Channel_Number      : DMA_Channel_Number;
      Source_Address_Error      : Occurred_Type;
      Source_Offset_Error       : Occurred_Type;
      Destination_Address_Error : Occurred_Type;
      Destination_Offset_Error  : Occurred_Type;
      NBYTES_CITER_Error        : Occurred_Type;
      Scatter_Gather_Error      : Occurred_Type;
      Source_Bus_Error          : Occurred_Type;
      Destination_Bus_Error     : Occurred_Type;
   end record;

   --  eDMAM Channel n Priority Registers (EDMA_CPRn)
   type Arbitration_Priority_Type is mod 2 ** 4;

   type Channel_Priority_Type is record
      Channel_Preemption     : Enable_Type;
      Current_Group_Priority : Group_Priority_Type;
      Arbitration_Priority   : Arbitration_Priority_Type;
   end record;

   --  Transfer Control Descriptor
   type Modulo_Type is mod 2 ** 5;
   type SSIZE_Type is (Size_8, Size_16, Size_32, Size_64, Size_256);
   type Signed_Offset_Type is mod 2 ** 16;
   type Num_Bytes_Type is mod 2 ** 32;
   type Count_Type is mod 2 ** 15;
   subtype Short_Count is Count_Type range 0 .. 2 ** 6 - 1;
   type BW_Type is (Stall_No, Stall_4, Stall_8);

   type TCD_Type (Minor_Channel_To_Channel_Linking : Enable_Type := Disable) is
      record
         Source_Address                            : System.Address;
         Source_Address_Modulo                     : Modulo_Type;
         Source_Data_Transfer_Size                 : SSIZE_Type;
         Destination_Address_Modulo                : Modulo_Type;
         Destination_Data_Transfer_Size            : SSIZE_Type;
         Source_Address_Offset                     : Signed_Offset_Type;
         Minor_Byte_Transfer_Count                 : Num_Bytes_Type;
         Last_Source_Address_Adjustment            : Num_Bytes_Type;
         Destination_Address                       : System.Address;
         Destination_Addres_Offset                 : Signed_Offset_Type;
         Last_Destination_Address_Adjustment       : Num_Bytes_Type;
         Starting_Minor_Channel_To_Channel_Linking : Enable_Type;
         Bandwidth_Control                         : BW_Type;
         Link_Channel_Number                       : DMA_Channel_Number;
         Channel_Done                              : Yes_No_Type;
         Channel_Active                            : Yes_No_Type;
         Major_Channel_To_Channel_Linking          : Enable_Type;
         Scatter_Gather                            : Enable_Type;
         Hardware_Request                          : Disable_Type;
         Interrupt_Major_Half_Complete             : Enable_Type;
         Interrupt_Major_Complete                  : Enable_Type;
         Channel_Start                             : Yes_No_Type;
         case Minor_Channel_To_Channel_Linking is
            when Disable =>
               Current_Major_Iteration_Count  : Count_Type;
               Starting_Major_Iteration_Count : Count_Type;
            when Enable =>
               Minor_Link_Channel_Number            : DMA_Channel_Number;
               Current_Major_Iteration_Count_Short  : Short_Count;
               Starting_Link_Channel_Number         : DMA_Channel_Number;
               Starting_Major_Iteration_Count_Short : Short_Count;
         end case;
      end record;

   ----------------------------------------------------------------------------
   --  Hardware Respresentations
   ----------------------------------------------------------------------------

   for Active_Type use (Clear => 0, Active => 1);

   for Arbitration_Type use (Fixed => 0, Round_Robin => 1);
   for Control_Type use record
      Channel_Group_3_Priority at 0 range 16 .. 17;
      Channel_Group_2_Priority at 0 range 18 .. 19;
      Channel_Group_1_Priority at 0 range 20 .. 21;
      Channel_Group_0_Priority at 0 range 22 .. 23;
      Group_Arbitration        at 0 range 28 .. 28;
      Channel_Arbitration      at 0 range 29 .. 29;
      Debug                    at 0 range 30 .. 30;
   end record;

   for Error_Status_Type use record
      Valid_Error               at 0 range 0 .. 0;
      Group_Priority_Error      at 0 range 16 .. 16;
      Channel_Priority_Error    at 0 range 17 .. 17;
      Error_Channel_Number      at 0 range 18 .. 23;
      Source_Address_Error      at 0 range 24 .. 24;
      Source_Offset_Error       at 0 range 25 .. 25;
      Destination_Address_Error at 0 range 26 .. 26;
      Destination_Offset_Error  at 0 range 27 .. 27;
      NBYTES_CITER_Error        at 0 range 28 .. 28;
      Scatter_Gather_Error      at 0 range 29 .. 29;
      Source_Bus_Error          at 0 range 30 .. 30;
      Destination_Bus_Error     at 0 range 31 .. 31;
   end record;

   for Channel_Priority_Type use record
      Channel_Preemption     at 0 range 0 .. 0;
      Current_Group_Priority at 0 range 2 .. 3;
      Arbitration_Priority   at 0 range 4 .. 7;
   end record;

   for SSIZE_Type use
     (Size_8   => 2#000#,
      Size_16  => 2#001#,
      Size_32  => 2#010#,
      Size_64  => 2#011#,
      Size_256 => 2#101#);
   for BW_Type use (Stall_No => 2#00#, Stall_4 => 2#10#, Stall_8 => 2#11#);

   for TCD_Type use record
      Source_Address                            at 0 range 0 .. 31;
      Source_Address_Modulo                     at 4 range 0 .. 4;
      Source_Data_Transfer_Size                 at 4 range 5 .. 7;
      Destination_Address_Modulo                at 4 range 8 .. 12;
      Destination_Data_Transfer_Size            at 4 range 13 .. 15;
      Source_Address_Offset                     at 4 range 16 .. 31;
      Minor_Byte_Transfer_Count                 at 8 range 0 .. 31;
      Last_Source_Address_Adjustment            at 16#C# range 0 .. 31;
      Destination_Address                       at 16#10# range 0 .. 31;
      Minor_Channel_To_Channel_Linking          at 16#14# range 0 .. 0;
      Current_Major_Iteration_Count             at 16#14# range 1 .. 15;
      Minor_Link_Channel_Number                 at 16#14# range 1 .. 6;
      Current_Major_Iteration_Count_Short       at 16#14# range 7 .. 15;
      Destination_Addres_Offset                 at 16#14# range 16 .. 31;
      Last_Destination_Address_Adjustment       at 16#18# range 0 .. 31;
      Starting_Minor_Channel_To_Channel_Linking at 16#1C# range 0 .. 0;
      Starting_Major_Iteration_Count            at 16#1C# range 1 .. 15;
      Starting_Link_Channel_Number              at 16#1C# range 1 .. 6;
      Starting_Major_Iteration_Count_Short      at 16#1C# range 7 .. 15;
      Bandwidth_Control                         at 16#1C# range 16 .. 17;
      Link_Channel_Number                       at 16#1C# range 18 .. 23;
      Channel_Done                              at 16#1C# range 24 .. 24;
      Channel_Active                            at 16#1C# range 25 .. 25;
      Major_Channel_To_Channel_Linking          at 16#1C# range 26 .. 26;
      Scatter_Gather                            at 16#1C# range 27 .. 27;
      Hardware_Request                          at 16#1C# range 28 .. 28;
      Interrupt_Major_Half_Complete             at 16#1C# range 29 .. 29;
      Interrupt_Major_Complete                  at 16#1C# range 30 .. 30;
      Channel_Start                             at 16#1C# range 31 .. 31;
   end record;
   ----------------------------------------------------------------------------
   --  SIU Registers
   ----------------------------------------------------------------------------

   pragma Warnings (Off, "*alignment*");

   Control_Register : Control_Type;
   for Control_Register'Address use
     System'To_Address (eDMA_Base_Address + CR_Offset_Address);

   Error_Status_Register : Error_Status_Type;
   for Error_Status_Register'Address use
     System'To_Address (eDMA_Base_Address + ESR_Offset_Address);

   Enable_Request_Register : DMA_Enable_Bit_Array;
   for Enable_Request_Register'Address use
     System'To_Address (eDMA_Base_Address + ERQR_Offset_Address);

   Enable_Error_Interrupt_Register : DMA_Enable_Bit_Array;
   for Enable_Error_Interrupt_Register'Address use
     System'To_Address (eDMA_Base_Address + EEIR_Offset_Address);

   Set_Enable_Request_Register : DMA_Channel_Number_With_Set_All;
   for Set_Enable_Request_Register'Address use
     System'To_Address (eDMA_Base_Address + SERQR_Offset_Address);

   Clear_Enable_Request_Register : DMA_Channel_Number_With_Set_All;
   for Clear_Enable_Request_Register'Address use
     System'To_Address (eDMA_Base_Address + CERQR_Offset_Address);

   Set_Enable_Error_Interrupt_Register : DMA_Channel_Number_With_Set_All;
   for Set_Enable_Error_Interrupt_Register'Address use
     System'To_Address (eDMA_Base_Address + SEEIR_Offset_Address);

   Clear_Enable_Error_Interrupt_Register : DMA_Channel_Number_With_Set_All;
   for Clear_Enable_Error_Interrupt_Register'Address use
     System'To_Address (eDMA_Base_Address + CEEIR_Offset_Address);

   Clear_Interrupt_Request_Register : DMA_Channel_Number_With_Set_All;
   for Clear_Interrupt_Request_Register'Address use
     System'To_Address (eDMA_Base_Address + CIRQR_Offset_Address);

   Clear_Error_Register : DMA_Channel_Number_With_Set_All;
   for Clear_Error_Register'Address use
     System'To_Address (eDMA_Base_Address + CER_Offset_Address);

   Set_Start_Bit_Register : DMA_Channel_Number_With_Set_All;
   for Set_Start_Bit_Register'Address use
     System'To_Address (eDMA_Base_Address + SSBR_Offset_Address);

   Clear_DONE_Status_Bit_Register : DMA_Channel_Number_With_Set_All;
   for Clear_DONE_Status_Bit_Register'Address use
     System'To_Address (eDMA_Base_Address + CDSBR_Offset_Address);

   Interrupt_Request_Register : DMA_Active_Bit_Array;
   for Interrupt_Request_Register'Address use
     System'To_Address (eDMA_Base_Address + IRQR_Offset_Address);

   Error_Register : DMA_Occured_Bit_Array;
   for Error_Register'Address use
     System'To_Address (eDMA_Base_Address + ER_Offset_Address);

   Channel_Prioirty_Register_Array :
     array (DMA_Channel_Number) of aliased Channel_Priority_Type;
   for Channel_Prioirty_Register_Array'Address use
     System'To_Address (eDMA_Base_Address + CPR_Offset_Address);
   type Channel_Prioirty_Register_Pointer is access all Channel_Priority_Type;

   Transfer_Control_Descriptor : array (DMA_Channel_Number) of  TCD_Type;
   for Transfer_Control_Descriptor'Address use
     System'To_Address (eDMA_Base_Address + TCD_Offset_Address);
   pragma Import (Ada, Transfer_Control_Descriptor);
   type Transfer_Control_Descriptor_Pointer is access all TCD_Type;
end MPC5554.eDMA;
