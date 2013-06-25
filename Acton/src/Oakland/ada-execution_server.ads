with Ada.Finalization;
with Ada.Real_Time;
with System;
with System.Multiprocessors;

private with Oak.Agent.Tasks;

package Ada.Execution_Server is

   type Server_Info is record
      Budget   : Ada.Real_Time.Time_Span;
      Priority : System.Any_Priority;
      Period   : Ada.Real_Time.Time_Span;
      Phase    : Ada.Real_Time.Time_Span;
      CPU      : System.Multiprocessors.CPU_Range :=
        System.Multiprocessors.Not_A_Specific_CPU;
   end record;

   type Execution_Server
     (Info  : access Server_Info) is abstract tagged limited private;

private
   package AF renames Ada.Finalization;

   type Execution_Server
     (Info  : access Server_Info)
     is abstract new AF.Limited_Controlled with record
      Server : Oak.Agent.Tasks.Task_Agent;
   end record;

end Ada.Execution_Server;
