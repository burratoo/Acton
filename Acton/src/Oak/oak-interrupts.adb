with System; use System;

package body Oak.Interrupts is

   procedure Attach_Handlers
     (Handlers          : access Interrupt_Handler_Array;
      Handler_PO        : access Protected_Agent'Class;
      Current_Agent     : in Agent_Handler;
      Next_Agent_To_Run : out Agent_Handler)
   is
      P : constant Interrupt_Priority := Handler_PO.Normal_Priority;
   begin
      for Handler of Handlers.all loop
         Oak.Processor_Support_Package.Interrupts.Attach_Handler
           (Interrupt => Handler.Interrupt,
            Handler   => Handler.Handler,
            Priority  => P);
      end loop;
      Next_Agent_To_Run := Current_Agent;
   end Attach_Handlers;

end Oak.Interrupts;
