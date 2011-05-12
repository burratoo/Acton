package MPC5554.Init is
   procedure Setup;
   pragma Export (C, Setup, "cfg_mpc5500ada");

   procedure Setup_External_Clocks;
   procedure Setup_External_Bus;
   procedure Setup_PBRIDGE;
   procedure Setup_XBAR;

end MPC5554.Init;
