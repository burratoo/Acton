with Acton.Scheduler_Agents.Priority_Server;
with System;

package Ada.Execution_Server.Priority_Server is
   type Priority_Server is new Execution_Server with private;

   procedure Initialise_Execution_Server
     (Server            : in out Priority_Server;
      Budget            : in Real_Time.Time_Span;
      Priority          : in System.Any_Priority;
      Period            : in Real_Time.Time_Span;
      Phase             : in Real_Time.Time_Span;
      Relative_Deadline : in Real_Time.Time_Span := Real_Time.Time_Span_Last;
      CPU               : in System.Multiprocessors.CPU_Range :=
        System.Multiprocessors.Not_A_Specific_CPU);

private
   type Priority_Server is new Execution_Server with record
      Server_Object : aliased
        Acton.Scheduler_Agents.Priority_Server.Priority_Server
          (System.Any_Priority'First, System.Any_Priority'Last);
   end record;
end Ada.Execution_Server.Priority_Server;
