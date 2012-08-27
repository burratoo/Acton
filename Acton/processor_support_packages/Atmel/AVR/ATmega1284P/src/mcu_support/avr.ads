package AVR with Pure is

   type Enable_Type is (Disabled, Enabled);
   for Enable_Type use (Disabled => 0, Enabled => 1);

   type Interrupt_Flag is (Not_Raised, Raised);
   for Interrupt_Flag use (Not_Raised => 0, Raised => 1);
end AVR;
