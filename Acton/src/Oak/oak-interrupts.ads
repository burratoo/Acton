limited with Oak.Oak_Task;
with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;

package Oak.Interrupts is

   pragma Preelaborate;

   type Interrupt_Handler_Pair is record
      Interrupt : Oak_Interrupt_Id;
      Handler   : Parameterless_Handler;
   end record;

   type Interrupt_Handler_Array
     is array
     (Oak_Interrupt_Id range <>) of Interrupt_Handler_Pair;

   type Interrupt_Handlers_Access is access all Interrupt_Handler_Array;

   procedure Attach_Handlers (Handlers        : Interrupt_Handlers_Access;
                              Handler_PO      : Oak_Task.Oak_Task_Handler;
                              T               : Oak_Task.Oak_Task_Handler;
                              Chosen_Task     : out Oak_Task.Oak_Task_Handler);
end Oak.Interrupts;
