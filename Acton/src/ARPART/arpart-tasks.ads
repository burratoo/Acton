with Oak.Oak_Task.Activation;
with Ada.Real_Time;
with Oak.Oak_Task;

package ARPART.Tasks with Preelaborate is

   package Oak_Activation renames Oak.Oak_Task.Activation;
   package OT renames Oak.Oak_Task;

   procedure Activate_Tasks (Chain : Oak.Oak_Task.Activation_Chain_Access);
   procedure Complete_Activation;
   procedure Complete_Task;

   procedure Change_Cycle_Period (New_Period : in Ada.Real_Time.Time_Span);

   procedure Change_Relative_Deadline
     (New_Deadline : in Ada.Real_Time.Time_Span);

   procedure Cycle_Completed;

   procedure Yield_Processor_To_Kernel (Task_Message : OT.Oak_Task_Message);
end ARPART.Tasks;
