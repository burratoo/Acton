with ARPART.Tasks;
with Oak.Oak_Task; use Oak.Oak_Task;

package body ARPART.Interrupts is
   procedure Attach_Handlers (PO        : in Oak.Oak_Task.Oak_Task_Handler;
                              Handlers  : in Interrupt_Handler_Array)
   is
      Handler_Store : aliased Interrupt_Handler_Array := Handlers;
      Message : constant Oak_Task_Message :=
                  (Message_Type => Attach_Interrupt_Handlers,
                   Attach_Handlers   => Handler_Store'Unchecked_Access,
                   Attach_Handler_PO => PO);
   begin
      Tasks.Yield_Processor_To_Kernel (Task_Message => Message);
   end Attach_Handlers;

end ARPART.Interrupts;
