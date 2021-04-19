------------------------------------------------------------------------------------------
--                                                                                      --
--                            OAK PROCESSOR SUPPORT PACKAGE                             --
--                                  FREESCALE MPC5544                                   --
--                                                                                      --
--                                MPC5554.ETPU.PWM.AUTO                                 --
--                                                                                      --
--                       Copyright (C) 2010-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with MPC5554;           use MPC5554;
with MPC5554.eTPU;      use MPC5554.eTPU;
with MPC5554.eTPU.Util; use MPC5554.eTPU.Util;

package MPC5554.eTPU.PWM.AUTO with Preelaborate is

   --  Function Configuration Information
   PWM_Function_Number : constant Function_Number   := 1;
   PWM_Table_Select    : constant ETCS_Type         := Alternative;
   PWM_Num_Parms       : constant Byte_Quanity_Type := 16#0018#;

   --  Host Service Request Definitions
   PWM_Init       : constant HSR_Type := 7;
   PWM_IMM_Update : constant HSR_Type := 3;
   PWM_CO_Update  : constant HSR_Type := 5;

   --  Parameter Definitions
   PWM_Period_Offset    : constant Shared_Data_Offset := 16#0001#;
   PWM_Active_Offset    : constant Shared_Data_Offset := 16#0005#;
   PWM_Co_Period_Offset : constant Shared_Data_Offset := 16#0009#;
   PWM_Co_Active_Offset : constant Shared_Data_Offset := 16#000D#;

end MPC5554.eTPU.PWM.AUTO;
