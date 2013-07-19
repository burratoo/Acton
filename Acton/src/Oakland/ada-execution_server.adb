package body Ada.Execution_Server is
   procedure Add_Execution_Server
     (Server            : in out Execution_Server;
      Budget            : in Real_Time.Time_Span;
      Priority          : in System.Any_Priority;
      Period            : in Real_Time.Time_Span;
      Phase             : in Real_Time.Time_Span;
      Relative_Deadline : in Real_Time.Time_Span := Real_Time.Time_Span_Last;
      CPU               : in System.Multiprocessors.CPU_Range :=
        System.Multiprocessors.Not_A_Specific_CPU) is
   begin
      raise Program_Error;
   end Add_Execution_Server;

   procedure Remove_Execution_Server (Server : in out Execution_Server) is
   begin
      raise Program_Error;
--        Remove_Agent_From_Scheduler (Server.Server_Object'Unchecked_Access);
   end Remove_Execution_Server;
end Ada.Execution_Server;
