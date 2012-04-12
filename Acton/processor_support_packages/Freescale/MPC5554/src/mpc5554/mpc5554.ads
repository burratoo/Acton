package MPC5554 is
   pragma Pure;

   type Enable_Type is (Disable, Enable);
   for Enable_Type use (Disable => 0, Enable => 1);

   type Disable_Type is (Enable, Disable);
   for Disable_Type use (Enable => 0, Disable => 1);

   type Pin_State_Type is (Low, High);
   for Pin_State_Type use (Low => 0, High => 1);

   type Occurred_Type is (Not_Occurred, Occurred);
   for Occurred_Type use (Not_Occurred => 0, Occurred => 1);

   type Yes_No_Type is (No, Yes);
   for Yes_No_Type use (No => 0, Yes => 1);

end MPC5554;
