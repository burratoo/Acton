package body GNAT_Exception is
   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      pragma Unreferenced (Msg, Line);
   begin
      loop
         null;
      end loop;
   end Last_Chance_Handler;
end GNAT_Exception;
