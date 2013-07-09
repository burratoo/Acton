with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;

with Oak.Agent; use Oak.Agent;
with Oak.Agent.Tasks.Protected_Objects; use Oak.Agent.Tasks.Protected_Objects;

package Oak.Interrupts with Preelaborate is

   type Interrupt_Handler_Pair is record
      Interrupt : Oak_Interrupt_Id;
      Handler   : Parameterless_Handler;
   end record;

   type Interrupt_Handler_Array
     is array
     (Oak_Interrupt_Id range <>) of Interrupt_Handler_Pair;

   procedure Attach_Handlers
     (Handlers          : access Interrupt_Handler_Array;
      Handler_PO        : access Protected_Agent'Class;
      Current_Agent     : in Agent_Handler;
      Next_Agent_To_Run : out Agent_Handler);

end Oak.Interrupts;
