with Ada.Unchecked_Conversion;

with Ada.Task_Identification; use Ada.Task_Identification;
with Oak.Agent.Tasks; use Oak.Agent.Tasks;
with Oakland.Tasks;   use Oakland.Tasks;

package body Ada.Cyclic_Tasks is

   function To_Task_Handler is
     new Unchecked_Conversion (Task_Id, Task_Handler);

   procedure Release_Task (T : in Ada.Task_Identification.Task_Id) is
      Message : constant Oak_Task_Message :=
                  (Message_Type    => Release_Task,
                   Task_To_Release => To_Task_Handler (T));
   begin
      if T /= Null_Task_Id then
         Yield_Processor_To_Kernel (Task_Message => Message);
      else
         raise Program_Error;
      end if;
   end Release_Task;

end Ada.Cyclic_Tasks;
