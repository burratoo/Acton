------------------------------------------------------------------------------------------
--                                                                                      --
--                            ACTON PROCESSOR SUPPORT PACKAGE                           --
--                                                                                      --
--                                        ATMEL                                         --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package Atmel with Pure is
   type Enable_Type is (Disable, Enable);
   for Enable_Type use (Disable => 0, Enable => 1);

   type Enabled_Type is (Disabled, Enabled);
   for Enabled_Type use (Disabled => 0, Enabled => 1);

   type Disable_Type is (Enable, Disable);
   for Disable_Type use (Enable => 0, Disable => 1);

   type Enable_No_Change_Type is (No_Change, Enable);
   for Enable_No_Change_Type use (No_Change => 0, Enable => 1);

   type Disable_No_Change_Type is (No_Change, Disable);
   for Disable_No_Change_Type use (No_Change => 0, Disable => 1);

   type Occured_Type is (Not_Occurred, Occurred);
   for Occured_Type use (Not_Occurred => 0, Occurred => 1);

   type Reset_Type is (No_Change, Reset);
   for Reset_Type use (No_Change => 0, Reset => 1);

   type Wide_Enable_Type is (No_Change, Enable, Disable);
   for Wide_Enable_Type use (No_Change => 2#00#, Enable => 2#01#,
                             Disable   => 2#10#);

   type Pin_Status is (Low, High);
   for Pin_Status use (Low => 0, High => 1);

   type Edge_Type is (Positve_Edge, Negative_Edge);
   for Edge_Type use (Positve_Edge => 0, Negative_Edge => 1);
end Atmel;
