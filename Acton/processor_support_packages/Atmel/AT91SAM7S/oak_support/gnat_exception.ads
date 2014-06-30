with System;

package GNAT_Exception is
   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer)
     with Export, Convention => C,
     External_Name => "__gnat_last_chance_handler";

end GNAT_Exception;
