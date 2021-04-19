------------------------------------------------------------------------------------------
--                                                                                      --
--                                    OAK COMPONENTS                                    --
--                                                                                      --
--                                    OAK.INTERRUPTS                                    --
--                                                                                      --
--                       Copyright (C) 2012-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Oak.Brokers.Protected_Objects; use Oak.Brokers.Protected_Objects;

package body Oak.Interrupts is

   procedure Attach_Handler (Handler : in Interrupt_Handler_Pair) is
   begin
      Oak.Processor_Support_Package.Interrupts.Attach_Handler
        (Interrupt => Handler.Interrupt,
         Handler   => Handler.Handler,
         Priority  =>
           Ceiling_Priority
             (Protected_Object_From_Access
                  (Parameterless_Access (Handler.Handler))));
   end Attach_Handler;

end Oak.Interrupts;
