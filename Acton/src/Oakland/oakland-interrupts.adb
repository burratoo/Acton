with Oakland.Tasks;
with Oak.Message; use Oak.Message;
with Oak.States;  use Oak.States;
with Oak.Agent.Protected_Objects; use Oak.Agent.Protected_Objects;
with Oak.Interrupts; use Oak.Interrupts;

package body Oakland.Interrupts is
   procedure Attach_Handlers
     (PO        : not null access Protected_Agent'Class;
      Handlers  : in Interrupt_Handler_Array)
   is
      Handler_Store : aliased Interrupt_Handler_Array := Handlers;
      Message : constant Oak_Message :=
                  (Message_Type => Attach_Interrupt_Handlers,
                   Attach_Handlers   => Handler_Store'Unchecked_Access,
                   Attach_Handler_PO => PO);
   begin
      Tasks.Yield_Processor_To_Kernel (Task_Message => Message);
   end Attach_Handlers;

end Oakland.Interrupts;
