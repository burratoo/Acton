with Oak.Interrupts; use Oak.Interrupts;
with Oak.Oak_Task;

package ARPART.Interrupts with Preelaborate is
   procedure Attach_Handlers (PO        : in Oak.Oak_Task.Oak_Task_Handler;
                              Handlers  : in Interrupt_Handler_Array);
end ARPART.Interrupts;
