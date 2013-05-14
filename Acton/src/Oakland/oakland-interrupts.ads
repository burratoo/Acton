with Oak.Interrupts;
limited with Oak.Agent.Tasks.Protected_Objects;

package Oakland.Interrupts with Preelaborate is
   procedure Attach_Handlers
     (PO        : not null access
        Oak.Agent.Tasks.Protected_Objects.Protected_Agent'Class;
      Handlers  : in Oak.Interrupts.Interrupt_Handler_Array);
end Oakland.Interrupts;
