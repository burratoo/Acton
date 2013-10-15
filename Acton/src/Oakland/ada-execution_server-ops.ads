with Oak.Agent.Schedulers;

package Ada.Execution_Server.Ops is

   procedure Add_Server_To_Scheduler
     (Server : in Execution_Server'Class);

   function Scheduler_Agent_Handler (Server : in Execution_Server'Class)
      return not null access Oak.Agent.Schedulers.Scheduler_Agent'Class;

private
   function Scheduler_Agent_Handler (Server : in Execution_Server'Class)
     return not null access Oak.Agent.Schedulers.Scheduler_Agent'Class
   is (Server.Scheduler_Agent_Handler);
end Ada.Execution_Server.Ops;
