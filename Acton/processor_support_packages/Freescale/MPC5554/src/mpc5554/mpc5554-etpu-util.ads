with MPC5554;                 use MPC5554;
with MPC5554.eTPU;            use MPC5554.eTPU;

package MPC5554.eTPU.Util is

   type eTPU_Config_Type is record
      MISC_Enable               : Enable_Type;
      MISC_Value                : MISC_Value_Type;
      Engine_Configuration_A    : Engine_Configuration_Type;
      Time_Base_Configuration_A : Time_Base_Configuration_Type;
      STAC_A                    : STAC_Bus_Configuration_Type;
      Engine_Configuration_B    : Engine_Configuration_Type;
      Time_Base_Configuration_B : Time_Base_Configuration_Type;
      STAC_B                    : STAC_Bus_Configuration_Type;
   end record;

   subtype Byte_Quanity_Type is Shared_Data_Offset;

   procedure Initialize
     (Config        : in eTPU_Config_Type;
      Code          : in Shared_Code_Type;
      Global_Values : in Shared_Data_Type_32);

   procedure Timer_Start;

   procedure Malloc
     (Number_Of_Bytes : in Byte_Quanity_Type;
      Data_Offset     : out Shared_Data_Offset);

   procedure Malloc2
     (Engine          : in eTPU_Engine;
      Channel         : in eTPU_ID_Type;
      Number_Of_Bytes : in Byte_Quanity_Type);

   procedure Interrupt_Enable
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type);

   procedure Interrupt_Disable
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type);

   procedure DMA_Enable
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type);

   procedure DMA_Disable
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type);

   procedure Channel_Enable
     (Engine   : in eTPU_Engine;
      Channel  : in eTPU_ID_Type;
      Priority : in Priority_Type);

   procedure Channel_Disable
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type);

   function Get_Pending_Host_Request
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type)
      return    HSR_Type;

   procedure Request_Channel_Service
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      HSR     : in HSR_Type);

   --  Memory Accessors
   function Local_Data_Adress
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type)
      return    Shared_Data_Offset;

   procedure Set_Local_32_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset;
      Data    : in Unsigned_32);

   function Get_Local_32_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset)
      return    Unsigned_32;

   procedure Set_Local_24_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset;
      Data    : in Unsigned_32);

   function Get_Local_24_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset)
      return    Unsigned_32;

   function Get_Local_24_Signed_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset)
      return    Integer_32;

   procedure Set_Local_16_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset;
      Data    : in Unsigned_16);

   function Get_Local_16_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset)
      return    Unsigned_16;

   procedure Set_Local_8_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset;
      Data    : in Unsigned_8);

   function Get_Local_8_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset)
      return    Unsigned_8;

   procedure Set_Global_32_Data
     (Offset : in Shared_Data_Offset;
      Data   : in Unsigned_32);

   function Get_Global_32_Data
     (Offset : in Shared_Data_Offset)
      return   Unsigned_32;

   procedure Set_Global_24_Data
     (Offset : in Shared_Data_Offset;
      Data   : in Integer_32);

   function Get_Global_24_Data
     (Offset : in Shared_Data_Offset)
      return   Unsigned_32;

   function Get_Global_24_Signed_Data
     (Offset : in Shared_Data_Offset)
      return   Integer_32;

   procedure Set_Global_16_Data
     (Offset : in Shared_Data_Offset;
      Data   : in Unsigned_16);

   function Get_Global_16_Data
     (Offset : in Shared_Data_Offset)
      return   Unsigned_16;

   procedure Set_Global_8_Data
     (Offset : in Shared_Data_Offset;
      Data   : in Unsigned_8);

   function Get_Global_8_Data
     (Offset : in Shared_Data_Offset)
      return   Unsigned_8;
private
   Free_Parameter_RAM_Offset : Shared_Data_Offset;
   Extension_Mask            : constant Unsigned_32 := 16#FF_FFFF#;

end MPC5554.eTPU.Util;
