with MPC5554;           use MPC5554;

package MPC5554.eTPU.PWM with Preelaborate is
   subtype Time_Unit is Unsigned_32;
   subtype Frequency_Unit is Time_Unit range 0 .. 100 * 10 ** 5;
   subtype Duty_Unit is Time_Unit range 0 .. 10000;

   type PWM_Polarity_Type is (PWM_Active_Low, PWM_Active_High);
   type Timebase_Type is (eTPU_TCR1, eTPU_TCR2);

   procedure Initization
     (Engine             : in eTPU_Engine;
      Channel            : in eTPU_ID_Type;
      Priority           : in Priority_Type;
      Frequency          : in Frequency_Unit;
      Duty               : in Duty_Unit;
      Polarity           : in PWM_Polarity_Type;
      Timebase           : in Timebase_Type;
      Timebase_Frequency : in Frequency_Unit);

   procedure Update_Duty
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Duty    : in Duty_Unit);

   procedure Update_Duty_Immediately
     (Engine  : in eTPU_Engine;
      Channel : in eTPU_ID_Type;
      Duty    : in Duty_Unit);

   procedure Update_PWM
     (Engine             : in eTPU_Engine;
      Channel            : in eTPU_ID_Type;
      Frequency          : in Frequency_Unit;
      Duty               : in Duty_Unit;
      Timebase_Frequency : in Frequency_Unit);

   function Get_Frequency
     (Engine             : in eTPU_Engine;
      Channel            : in eTPU_ID_Type;
      Timebase_Frequency : in Frequency_Unit)
      return               Frequency_Unit;

private
   subtype Period_Unit is Time_Unit range 1 .. 16#7F_FFFF#;
   Duty_Scale : constant Duty_Unit := 10000;
end MPC5554.eTPU.PWM;
