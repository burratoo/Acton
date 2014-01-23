with Ada.Unchecked_Conversion;

with Ada.Task_Identification; use Ada.Task_Identification;
with Oak.Agent; use Oak.Agent;
with Oak.Message; use Oak.Message;
with Oak.States; use Oak.States;

with Oakland.Tasks;   use Oakland.Tasks;

package body Ada.Cyclic_Tasks is

   function To_Task_Handler is
     new Unchecked_Conversion
       (Ada.Task_Identification.Task_Id, Oak.Agent.Task_Id);

   procedure Release_Task (T : in Ada.Task_Identification.Task_Id) is
      Message : Oak_Message :=
                  (Message_Type    => Release_Task,
                   Task_To_Release => To_Task_Handler (T));
   begin
      if T /= Null_Task_Id then
         Yield_Processor_To_Kernel (With_Message => Message);
      else
         raise Program_Error;
      end if;
   end Release_Task;

end Ada.Cyclic_Tasks;
