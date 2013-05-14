with Oak.Oak_Time;   use Oak.Oak_Time;
with Oak.Agent.Tasks;

package Oakland.Tasks with Preelaborate is

   procedure Activate_Tasks
     (Chain : Oak.Agent.Tasks.Activation_Chain_Access);
   procedure Complete_Activation;
   procedure Complete_Task;

   procedure Change_Cycle_Period (New_Period : in Time_Span);

   procedure Change_Relative_Deadline (New_Deadline : in Time_Span);

   procedure Cycle_Completed;

   procedure Yield_Processor_To_Kernel
     (Task_Message : in Oak.Agent.Tasks.Oak_Task_Message);
end Oakland.Tasks;
