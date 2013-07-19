with Acton.Scheduler_Agents.Priority_Server;
use Acton.Scheduler_Agents.Priority_Server;
with Ada.Execution_Server.Ops; use Ada.Execution_Server.Ops;
with Oak.Scheduler;            use Oak.Scheduler;
with Oak.Oak_Time.Conversion;  use Oak.Oak_Time.Conversion;

package body Ada.Execution_Server.Priority_Server is
   procedure Initialise_Execution_Server
     (Server            : in out Priority_Server;
      Budget            : in Real_Time.Time_Span;
      Priority          : in System.Any_Priority;
      Period            : in Real_Time.Time_Span;
      Phase             : in Real_Time.Time_Span;
      Relative_Deadline : in Real_Time.Time_Span := Real_Time.Time_Span_Last;
      CPU               : in System.Multiprocessors.CPU_Range :=
        System.Multiprocessors.Not_A_Specific_CPU)  is
   begin
      Server.Scheduler_Agent_Handler := Server.Server_Object'Unchecked_Access;
      Server.Server_Object.Initialise_Scheduler_Agent
        (Budget            => To_Oak_Time_Span (Budget),
         Priority          => Priority,
         Period            => To_Oak_Time_Span (Period),
         Phase             => To_Oak_Time_Span (Phase),
         Relative_Deadline => To_Oak_Time_Span (Relative_Deadline),
         CPU               => CPU);
      Add_Server_To_Scheduler (Server);
   end Initialise_Execution_Server;

end Ada.Execution_Server.Priority_Server;
