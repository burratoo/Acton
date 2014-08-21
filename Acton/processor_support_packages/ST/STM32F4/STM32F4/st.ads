package ST with Pure is
   type Enable_Type is (Disable, Enable);
   for Enable_Type use (Disable => 0, Enable => 1);

   type Enabled_Type is (Disabled, Enabled);
   for Enabled_Type use (Disabled => 0, Enabled => 1);

   type Decision_Type is (No, Yes);
   for Decision_Type use (No => 0, Yes => 1);

   type Occurred_Type is (Not_Occurred, Occurred);
   type Clear_Type is (Do_Not_Clear, Clear);

   type Reset_Type is (Do_Not_Reset, Reset);
   for Reset_Type use (Do_Not_Reset => 0, Reset => 1);
end ST;
