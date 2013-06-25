with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;

limited with Oak.Agent.Tasks;
limited with Oak.Agent.Tasks.Protected_Objects;

package Oak.Interrupts with Preelaborate is

   type Interrupt_Handler_Pair is record
      Interrupt : Oak_Interrupt_Id;
      Handler   : Parameterless_Handler;
   end record;

   type Interrupt_Handler_Array
     is array
     (Oak_Interrupt_Id range <>) of Interrupt_Handler_Pair;

   procedure Attach_Handlers
     (Handlers        : access Interrupt_Handler_Array;
      Handler_PO      : access
        Agent.Tasks.Protected_Objects.Protected_Agent'Class;
      T               : access Agent.Tasks.Task_Agent'Class;
      Chosen_Task     : out Agent.Tasks.Task_Handler);

end Oak.Interrupts;
