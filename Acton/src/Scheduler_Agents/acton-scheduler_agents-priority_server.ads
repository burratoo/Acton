with Oak.Agent;              use Oak.Agent;
with Oak.Agent.Schedulers;   use Oak.Agent.Schedulers;
with Oak.Oak_Time;           use Oak.Oak_Time;
with System;                 use System;
with System.Multiprocessors; use System.Multiprocessors;

package Acton.Scheduler_Agents.Priority_Server with Preelaborate is
   type Priority_Server is new Scheduler_Agent with private
     with Preelaborable_Initialization;

   procedure Initialise_Scheduler_Agent
     (Agent : in out Priority_Server);

   procedure Initialise_Scheduler_Agent
     (Agent             : in out Priority_Server;
      Budget            : in Time_Span;
      Priority          : in Any_Priority;
      Period            : in Time_Span;
      Phase             : in Time_Span;
      Relative_Deadline : in Time_Span;
      CPU               : in CPU_Range);

   procedure Run_Loop (Self : in out Priority_Server) with No_Return;

   Stack_Size : constant := 1 * 1024;
   Agent_Name : constant String := "Priority_Server";

private
   type Priority_Server is new Scheduler_Agent with record
      Runnable_Queue        : access Oak_Agent'Class;
      Sleeping_Queue        : access Oak_Agent'Class;

      Period                : Time_Span;
      Phase                 : Time_Span;
      Relative_Deadline     : Time_Span;
      Execution_Budget      : Time_Span;

      Next_Wake_Time        : Time;
      Budget_Exhausted_Time : Time;
   end record;

end Acton.Scheduler_Agents.Priority_Server;
