with Ada.Real_Time;
with System;
with System.Multiprocessors;
with Oak.Agent;
with Oak.Agent.Schedulers;

package Ada.Execution_Server is

   type Execution_Server is abstract tagged limited private;

   procedure Add_Execution_Server
     (Server            : in out Execution_Server;
      Budget            : in Real_Time.Time_Span;
      Priority          : in System.Any_Priority;
      Period            : in Real_Time.Time_Span;
      Phase             : in Real_Time.Time_Span;
      Relative_Deadline : in Real_Time.Time_Span := Real_Time.Time_Span_Last;
      CPU               : in System.Multiprocessors.CPU_Range :=
        System.Multiprocessors.Not_A_Specific_CPU);

   procedure Remove_Execution_Server (Server : in out Execution_Server);

   procedure Initialise_Execution_Server
     (Server            : in out Execution_Server;
      Budget            : in Real_Time.Time_Span;
      Priority          : in System.Any_Priority;
      Period            : in Real_Time.Time_Span;
      Phase             : in Real_Time.Time_Span;
      Relative_Deadline : in Real_Time.Time_Span := Real_Time.Time_Span_Last;
      CPU               : in System.Multiprocessors.CPU_Range :=
        System.Multiprocessors.Not_A_Specific_CPU) is abstract;

private
   type Execution_Server is abstract tagged limited record
      Scheduler_Agent_Handler : access
        Oak.Agent.Schedulers.Scheduler_Agent'Class;
   end record;

end Ada.Execution_Server;
