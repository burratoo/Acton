with Oak.Oak_Task.Data_Access; use Oak.Oak_Task.Data_Access;
with System; use System;
use Oak.Oak_Task;

package body Oak.Interrupts is

   procedure Attach_Handlers (Handlers        : Interrupt_Handlers_Access;
                              Handler_PO      : Oak_Task.Oak_Task_Handler;
                              T               : Oak_Task.Oak_Task_Handler;
                              Chosen_Task     : out Oak_Task.Oak_Task_Handler)
   is
      P : constant Interrupt_Priority := Get_Normal_Priority (Handler_PO);
   begin
      for J in Handlers'Range loop
         Oak.Processor_Support_Package.Interrupts.Attach_Handler
           (Interrupt => Handlers (J).Interrupt,
            Handler   => Handlers (J).Handler,
            Priority  => P);
      end loop;
      Chosen_Task := T;
   end Attach_Handlers;

end Oak.Interrupts;
