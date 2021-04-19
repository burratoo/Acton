------------------------------------------------------------------------------------------
--                                                                                      --
--                            OAK PROCESSOR SUPPORT PACKAGE                             --
--                                  FREESCALE MPC5544                                   --
--                                                                                      --
--                                  MPC5554.ETPU.PWM                                   --
--                                                                                      --
--                       Copyright (C) 2010-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with MPC5554.eTPU.PWM.AUTO; use MPC5554.eTPU.PWM.AUTO;
with MPC5554.eTPU.Util;

package body MPC5554.eTPU.PWM is

   -----------------
   -- Initization --
   -----------------
   procedure Initization
     (Engine             : in eTPU_Engine;
      Channel            : in eTPU_ID_Type;
      Priority           : in Priority_Type;
      Frequency          : in Frequency_Unit;
      Duty               : in Duty_Unit;
      Polarity           : in PWM_Polarity_Type;
      Timebase           : in Timebase_Type;
      Timebase_Frequency : in Frequency_Unit)
   is
      Channel_Period : Period_Unit;
      PWM_FM         : FM_Type;
   begin
      eTPU.Util.Channel_Disable (Engine => Engine, Channel => Channel);
      eTPU.Util.Malloc2
        (Engine          => Engine,
         Channel         => Channel,
         Number_Of_Bytes => PWM_Num_Parms);

      Channel_Period := Timebase_Frequency / Frequency;

      eTPU.Util.Set_Local_24_Data
        (Engine  => Engine,
         Channel => Channel,
         Offset  => PWM_Period_Offset,
         Data    => Channel_Period);
      eTPU.Util.Set_Local_24_Data
        (Engine  => Engine,
         Channel => Channel,
         Offset  => PWM_Active_Offset,
         Data    => (Channel_Period * Duty) / Duty_Scale);

      eTPU.Channel (Engine) (Channel).Configuration_Register.Priority  :=
        Priority;
      eTPU.Channel (Engine) (Channel).Configuration_Register.
        Entry_Table_Condition_Select                                   :=
        PWM_Table_Select;
      eTPU.Channel (Engine) (Channel).Configuration_Register.Function_Select
         := PWM_Function_Number;

      case Polarity is
         when PWM_Active_Low =>
            PWM_FM := 0;
         when PWM_Active_High =>
            PWM_FM := 1;
      end case;

      case Timebase is
         when eTPU_TCR1 =>
            null;
         when eTPU_TCR2 =>
            PWM_FM := PWM_FM + 2;
      end case;

      eTPU.Channel (Engine) (Channel).Status_Register.Function_Mode  :=
        PWM_FM;

      --  Write to HSR starts the channel running
      eTPU.Util.Request_Channel_Service
        (Engine  => Engine,
         Channel => Channel,
         HSR     => PWM_Init);

   end Initization;

   -----------------
   -- Update_Duty --
   -----------------

   procedure Update_Duty
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Duty    : in Duty_Unit)
   is
      Period : Period_Unit;
   begin
      Period :=
         eTPU.Util.Get_Local_24_Data
           (Engine  => Engine,
            Channel => Channel,
            Offset  => PWM_Period_Offset);
      eTPU.Util.Set_Local_24_Data
        (Engine  => Engine,
         Channel => Channel,
         Offset  => PWM_Active_Offset,
         Data    => Period * Duty / Duty_Scale);
   end Update_Duty;

   -----------------------------
   -- Update_Duty_Immediately --
   -----------------------------

   procedure Update_Duty_Immediately
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Duty    : in Duty_Unit)
   is
   begin
      Update_Duty (Engine => Engine, Channel => Channel, Duty => Duty);
      eTPU.Util.Request_Channel_Service
        (Engine  => Engine,
         Channel => Channel,
         HSR     => PWM_IMM_Update);
   end Update_Duty_Immediately;

   ----------------
   -- Update_PWM --
   ----------------

   procedure Update_PWM
     (Engine             : in eTPU_Engine;
      Channel            : in eTPU_ID_Type;
      Frequency          : in Frequency_Unit;
      Duty               : in Duty_Unit;
      Timebase_Frequency : in Frequency_Unit)
   is
      Channel_Period : Period_Unit;
   begin
      Channel_Period := Timebase_Frequency / Frequency;

      eTPU.Util.Set_Local_24_Data
        (Engine  => Engine,
         Channel => Channel,
         Offset  => PWM_Period_Offset,
         Data    => Channel_Period);
      eTPU.Util.Set_Local_24_Data
        (Engine  => Engine,
         Channel => Channel,
         Offset  => PWM_Active_Offset,
         Data    => (Channel_Period * Duty) / Duty_Scale);
      eTPU.Util.Request_Channel_Service
        (Engine  => Engine,
         Channel => Channel,
         HSR     => PWM_CO_Update);
   end Update_PWM;

   -------------------
   -- Get_Frequency --
   -------------------

   function Get_Frequency
     (Engine             : in eTPU_Engine;
      Channel            : in eTPU_ID_Type;
      Timebase_Frequency : in Frequency_Unit)
      return               Frequency_Unit
   is
      Channel_Period : Period_Unit;
   begin
      Channel_Period :=
         eTPU.Util.Get_Local_24_Data
           (Engine  => Engine,
            Channel => Channel,
            Offset  => PWM_Period_Offset);
      return (Timebase_Frequency / Channel_Period);
   end Get_Frequency;

end MPC5554.eTPU.PWM;
