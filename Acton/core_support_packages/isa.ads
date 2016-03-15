package ISA with Pure is

   type Enable_Type is (Disable, Enable);
   for Enable_Type use (Disable => 0, Enable => 1);

   type Disable_Type is (Enable, Disable);
   for Disable_Type use (Enable => 0, Disable => 1);

   type Enabled_Type is (Disabled, Enabled);
   for Enabled_Type use (Disabled => 0, Enabled => 1);

   type Decision_Type is (No, Yes);
   for Decision_Type use (No => 0, Yes => 1);

end ISA;
