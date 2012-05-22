with Oak.Agent.Tasks.Protected_Objects; use Oak.Agent.Tasks.Protected_Objects;
with Oak.Interrupts;  use Oak.Interrupts;

package ARPART.Interrupts with Preelaborate is
   procedure Attach_Handlers
     (PO        : not null access Protected_Agent'Class;
      Handlers  : in Interrupt_Handler_Array);
end ARPART.Interrupts;
