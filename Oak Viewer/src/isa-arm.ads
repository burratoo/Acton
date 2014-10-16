package ISA.ARM with Pure is

   type Enable_No_Change_Type is (No_Change, Enable);
   for Enable_No_Change_Type use (No_Change => 0, Enable => 1);

   type Disable_No_Change_Type is (No_Change, Disable);
   for Disable_No_Change_Type use (No_Change => 0, Disable => 1);

end ISA.ARM;
