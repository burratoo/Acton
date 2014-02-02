with Acton.Scheduler_Agents.Priority_Server;
use Acton.Scheduler_Agents.Priority_Server;

with Oak.Scheduler;            use Oak.Scheduler;
with Oak.Oak_Time.Conversion;  use Oak.Oak_Time.Conversion;
with Oak.Core; use Oak.Core;
with Oak.Agent.Kernel; use Oak.Agent.Kernel;
with Oakland.Tasks; use Oakland.Tasks;
with Oak.States; use Oak.States;
with Oak.Message; use Oak.Message;

package body Ada.Execution_Server.Priority_Server is

   procedure Add_Execution_Server
     (Server            : in out Priority_Server;
      Budget            : in Real_Time.Time_Span;
      Priority          : in System.Any_Priority;
      Period            : in Real_Time.Time_Span;
      Phase             : in Real_Time.Time_Span;
      Relative_Deadline : in Real_Time.Time_Span := Real_Time.Time_Span_Last;
      CPU               : in System.Multiprocessors.CPU_Range :=
        System.Multiprocessors.Not_A_Specific_CPU)
   is
      Message : Oak_Message;
   begin
      New_Scheduler_Agent
        (Agent             => Server.Scheduler,
         Min_Priority      => Priority,
         Max_Priority      => Priority,
         Budget            => To_Oak_Time_Span (Budget),
         Period            => To_Oak_Time_Span (Period),
         Phase             => To_Oak_Time_Span (Phase),
         Relative_Deadline => To_Oak_Time_Span (Relative_Deadline),
         CPU               => CPU);
      Message := (Message_Type => Adding_Agent,
                  Agent_To_Add => Server.Scheduler);
      Yield_Processor_To_Kernel (With_Message => Message);
   end Add_Execution_Server;

end Ada.Execution_Server.Priority_Server;
