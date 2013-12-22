with Oakland.Tasks;
with Oak.States;  use Oak.States;

package body Ada.Execution_Server.Ops is

   procedure Add_Server_To_Scheduler
     (Server : in Execution_Server'Class) is
   begin
      Oakland.Tasks.Yield_Processor_To_Kernel
        (Task_Message =>
           (Message_Type => Adding_Agent,
            Agent_To_Add => Server.Scheduler_Agent_Handler));
   end Add_Server_To_Scheduler;
end Ada.Execution_Server.Ops;
