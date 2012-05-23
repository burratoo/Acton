with Oak.Agent.Tasks; use Oak.Agent.Tasks;
with Oak.Oak_Time;   use Oak.Oak_Time;

package ARPART.Tasks with Preelaborate is

   procedure Activate_Tasks (Chain : Activation_Chain_Access);
   procedure Complete_Activation;
   procedure Complete_Task;

   procedure Change_Cycle_Period (New_Period : in Time_Span);

   procedure Change_Relative_Deadline (New_Deadline : in Time_Span);

   procedure Cycle_Completed;

   procedure Yield_Processor_To_Kernel (Task_Message : in Oak_Task_Message);
end ARPART.Tasks;
