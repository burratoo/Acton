package body MPC5554.eTPU.Util is

   procedure Initialize
     (Config        : in eTPU_Config_Type;
      Code          : in Shared_Code_Type;
      Global_Values : in Shared_Data_Type_32)
   is
   begin

      --  1. Load microcode
      eTPU.Engine_Configuration_Register_A.Module_Disable_Internal_Stop :=
        Stop_Clocks; --  Stop eTPU_A
      eTPU.Engine_Configuration_Register_B.Module_Disable_Internal_Stop :=
        Stop_Clocks; --  Stop eTPU_B
      eTPU.Module_Configuration_Register.SCM_Visability                 :=
        Visable_To_Slave; --  enable CPU writes to eTPU code memory

      --  Copy microcode

      for I in Code'Range loop
         eTPU.Shared_Code_Memory (I) := Code (I);
      end loop;

      --  Clear rest of the program memory
      for I in (Code'Last + 1) .. eTPU.Shared_Code_Offset'Last loop
         eTPU.Shared_Code_Memory (I) := 0;
      end loop;

      eTPU.Module_Configuration_Register.SCM_Visability :=
        Not_Visable_To_Slave; --  disable CPU writes to eTPU code memory

      --  Configure MISC
      eTPU.MISC_Compare_Register                         := Config.MISC_Value;
      eTPU.Module_Configuration_Register.SCM_MISC_Enable :=
        Config.MISC_Enable;

      --  Configure Engine A
      eTPU.Engine_Configuration_Register_A    :=
        Config.Engine_Configuration_A;
      eTPU.Time_Base_Configuration_Register_A :=
        Config.Time_Base_Configuration_A;
      eTPU.STAC_Bus_Configuration_Register_A  := Config.STAC_A;

      --  Configure Engine B
      eTPU.Engine_Configuration_Register_B    :=
        Config.Engine_Configuration_B;
      eTPU.Time_Base_Configuration_Register_B :=
        Config.Time_Base_Configuration_B;
      eTPU.STAC_Bus_Configuration_Register_B  := Config.STAC_B;

      --  3. Copy initial global values to parameter RAM
      for I in Global_Values'Range loop
         eTPU.Shared_Data_Memory_32 (I) := Global_Values (I);
      end loop;
      Free_Parameter_RAM_Offset := (Global_Values'Last + 1) * 4;

   end Initialize;

   procedure Timer_Start is
   begin
      eTPU.Module_Configuration_Register.Global_Time_Base_Enable := Enable;
   end Timer_Start;

   procedure Malloc
     (Number_Of_Bytes : in Byte_Quanity_Type;
      Data_Offset     : out Shared_Data_Offset)
   is
      Bytes_Given, New_Free_Offset : Byte_Quanity_Type;
   begin
      --  round up to the nearest 8 byte block
      Bytes_Given := Shift_Left (Shift_Right (Number_Of_Bytes, 3), 1);

      New_Free_Offset := Bytes_Given + Free_Parameter_RAM_Offset;
      Data_Offset               := Free_Parameter_RAM_Offset;
      Free_Parameter_RAM_Offset := New_Free_Offset;
   end Malloc;

   procedure Malloc2
     (Engine          : in eTPU_Engine;
      Channel         : in eTPU_ID_Type;
      Number_Of_Bytes : in Byte_Quanity_Type)
   is
      Data_Offset : Shared_Data_Offset;
   begin
      if eTPU.Channel (Engine) (Channel).Configuration_Register.
           Parameter_Base_Address =
         0
      then
         Malloc (Number_Of_Bytes, Data_Offset);
         eTPU.Channel (Engine) (Channel).Configuration_Register.
           Parameter_Base_Address := Data_Offset / 8;
      end if;
   end Malloc2;

   function Local_Data_Adress
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type)
      return    Shared_Data_Offset
   is
   begin
      return eTPU.Channel (Engine) (Channel).Configuration_Register.
        Parameter_Base_Address *
             8;
   end Local_Data_Adress;

   procedure Interrupt_Enable
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type)
   is
   begin
      eTPU.Channel (Engine) (Channel).Configuration_Register.Interrupt_Enable
         := Enable;
   end Interrupt_Enable;

   procedure Interrupt_Disable
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type)
   is
   begin
      eTPU.Channel (Engine) (Channel).Configuration_Register.Interrupt_Enable
         := Disable;
   end Interrupt_Disable;

   procedure DMA_Enable
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type)
   is
   begin
      eTPU.Channel (Engine) (Channel).Configuration_Register.
        Data_Transfer_Request_Enable := Enable;
   end DMA_Enable;

   procedure DMA_Disable
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type)
   is
   begin
      eTPU.Channel (Engine) (Channel).Configuration_Register.
        Data_Transfer_Request_Enable := Disable;
   end DMA_Disable;

   procedure Channel_Enable
     (Engine   : in eTPU_Engine;
      Channel  : in eTPU_ID_Type;
      Priority : in Priority_Type)
   is
   begin
      eTPU.Channel (Engine) (Channel).Configuration_Register.Priority  :=
        Priority;
   end Channel_Enable;

   procedure Channel_Disable
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type)
   is
   begin
      eTPU.Channel (Engine) (Channel).Configuration_Register.Priority  :=
        Disabled;
   end Channel_Disable;

   function Get_Pending_Host_Request
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type)
      return    HSR_Type
   is
   begin
      return
        eTPU.Channel (Engine) (Channel).Host_Service_Request_Register.
        Host_Service_Request;
   end Get_Pending_Host_Request;

   procedure Request_Channel_Service
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      HSR     : in HSR_Type)
   is
   begin
      eTPU.Channel (Engine) (Channel).Host_Service_Request_Register.
        Host_Service_Request := HSR;
   end Request_Channel_Service;

   procedure Set_Local_32_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset;
      Data    : in Unsigned_32)
   is
      Memory_Offset : Shared_Data_Offset;
   begin
      Memory_Offset                         :=
        (eTPU.Channel (Engine) (Channel).Configuration_Register.
        Parameter_Base_Address *
         8 +
         Offset) /
        4;
      Shared_Data_Memory_32 (Memory_Offset) := Data;
   end Set_Local_32_Data;

   function Get_Local_32_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset)
      return    Unsigned_32
   is
      Memory_Offset : Shared_Data_Offset;
   begin
      Memory_Offset :=
        (eTPU.Channel (Engine) (Channel).Configuration_Register.
        Parameter_Base_Address *
         8 +
         Offset) /
        4;
      return Shared_Data_Memory_32 (Memory_Offset);
   end Get_Local_32_Data;

   procedure Set_Local_24_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset;
      Data    : in Unsigned_32)
   is
      Memory_Offset : Shared_Data_Offset;
   begin
      Memory_Offset                                  :=
        (eTPU.Channel (Engine) (Channel).Configuration_Register.
        Parameter_Base_Address *
         8 +
         Offset +
         1) /
        4;
      Shared_Data_Extended_Memory_32 (Memory_Offset) := Integer_32 (Data);
   end Set_Local_24_Data;

   function Get_Local_24_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset)
      return    Unsigned_32
   is
      Memory_Offset : Shared_Data_Offset;
   begin
      Memory_Offset :=
        (eTPU.Channel (Engine) (Channel).Configuration_Register.
        Parameter_Base_Address *
         8 +
         Offset -
         1) /
        4;
      return (Shared_Data_Memory_32 (Memory_Offset) and Extension_Mask);
   end Get_Local_24_Data;

   function Get_Local_24_Signed_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset)
      return    Integer_32
   is
      Memory_Offset : Shared_Data_Offset;
   begin
      Memory_Offset :=
        (eTPU.Channel (Engine) (Channel).Configuration_Register.
        Parameter_Base_Address *
         8 +
         Offset -
         1) /
        4;
      return Shared_Data_Extended_Memory_32 (Memory_Offset);
   end Get_Local_24_Signed_Data;

   procedure Set_Local_16_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset;
      Data    : in Unsigned_16)
   is
      Memory_Offset : Shared_Data_Offset;
   begin
      Memory_Offset                         :=
        (eTPU.Channel (Engine) (Channel).Configuration_Register.
        Parameter_Base_Address *
         8 +
         Offset -
         1) /
        2;
      Shared_Data_Memory_16 (Memory_Offset) := Data;
   end Set_Local_16_Data;

   function Get_Local_16_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset)
      return    Unsigned_16
   is
      Memory_Offset : Shared_Data_Offset;
   begin
      Memory_Offset :=
        (eTPU.Channel (Engine) (Channel).Configuration_Register.
        Parameter_Base_Address *
         8 +
         Offset) /
        2;
      return Shared_Data_Memory_16 (Memory_Offset);
   end Get_Local_16_Data;

   procedure Set_Local_8_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset;
      Data    : in Unsigned_8)
   is
      Memory_Offset : Shared_Data_Offset;
   begin
      Memory_Offset                        :=
        (eTPU.Channel (Engine) (Channel).Configuration_Register.
        Parameter_Base_Address *
         8 +
         Offset);
      Shared_Data_Memory_8 (Memory_Offset) := Data;
   end Set_Local_8_Data;

   function Get_Local_8_Data
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Offset  : in Shared_Data_Offset)
      return    Unsigned_8
   is
      Memory_Offset : Shared_Data_Offset;
   begin
      Memory_Offset :=
        (eTPU.Channel (Engine) (Channel).Configuration_Register.
        Parameter_Base_Address *
         8 +
         Offset);
      return Shared_Data_Memory_8 (Memory_Offset);
   end Get_Local_8_Data;

   procedure Set_Global_32_Data
     (Offset : in Shared_Data_Offset;
      Data   : in Unsigned_32)
   is
   begin
      Shared_Data_Memory_32 (Offset / 4) := Data;
   end Set_Global_32_Data;

   function Get_Global_32_Data
     (Offset : in Shared_Data_Offset)
      return   Unsigned_32
   is
   begin
      return Shared_Data_Memory_32 (Offset / 4);
   end Get_Global_32_Data;

   procedure Set_Global_24_Data
     (Offset : in Shared_Data_Offset;
      Data   : in Integer_32)
   is
   begin
      Shared_Data_Extended_Memory_32 ((Offset - 1) / 4)   := Data;
   end Set_Global_24_Data;

   function Get_Global_24_Data
     (Offset : in Shared_Data_Offset)
      return   Unsigned_32
   is
   begin
      return (Shared_Data_Memory_32 ((Offset - 1) / 4) and Extension_Mask);
   end Get_Global_24_Data;

   function Get_Global_24_Signed_Data
     (Offset : in Shared_Data_Offset)
      return   Integer_32
   is
   begin
      return Shared_Data_Extended_Memory_32 ((Offset - 1) / 4);
   end Get_Global_24_Signed_Data;

   procedure Set_Global_16_Data
     (Offset : in Shared_Data_Offset;
      Data   : in Unsigned_16)
   is
   begin
      Shared_Data_Memory_16 (Offset / 2) := Data;
   end Set_Global_16_Data;

   function Get_Global_16_Data
     (Offset : in Shared_Data_Offset)
      return   Unsigned_16
   is
   begin
      return Shared_Data_Memory_16 (Offset / 2);
   end Get_Global_16_Data;

   procedure Set_Global_8_Data
     (Offset : in Shared_Data_Offset;
      Data   : in Unsigned_8)
   is
   begin
      Shared_Data_Memory_8 (Offset) := Data;
   end Set_Global_8_Data;

   function Get_Global_8_Data
     (Offset : in Shared_Data_Offset)
      return   Unsigned_8
   is
   begin
      return Shared_Data_Memory_8 (Offset);
   end Get_Global_8_Data;

end MPC5554.eTPU.Util;
