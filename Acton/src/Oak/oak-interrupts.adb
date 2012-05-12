with Oak.Agent.Tasks;
with Oak.Agent.Tasks.Protected_Object;

with System; use System;

package body Oak.Interrupts is

   procedure Attach_Handlers
     (Handlers        : in Interrupt_Handlers_Access;
      Handler_PO      : access
        Agent.Tasks.Protected_Object.Protected_Agent'Class;
      T               : access Agent.Tasks.Task_Agent'Class;
      Chosen_Task     : out Agent.Tasks.Task_Handler)
   is
      P : constant Interrupt_Priority :=
            Agent.Tasks.Normal_Priority
              (Oak.Agent.Tasks.Task_Agent'Class (Handler_PO.all));
   begin
      for Handler of Handlers.all loop
         Oak.Processor_Support_Package.Interrupts.Attach_Handler
           (Interrupt => Handler.Interrupt,
            Handler   => Handler.Handler,
            Priority  => P);
      end loop;
      Chosen_Task := Oak.Agent.Tasks.Task_Handler (T);
   end Attach_Handlers;

end Oak.Interrupts;
