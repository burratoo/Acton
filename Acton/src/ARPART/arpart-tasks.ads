with Oak.Oak_Task.Activation;
with Ada.Real_Time;
with Oak.Oak_Task;

package ARPART.Tasks is
   package Oak_Activation renames Oak.Oak_Task.Activation;

   procedure Activate_Tasks (Chain : Oak.Oak_Task.Activation_Chain_Access);
   procedure Complete_Activation;
   procedure Complete_Task;

   procedure Change_Cycle_Period (New_Period : in Ada.Real_Time.Time_Span);

   procedure Change_Relative_Deadline
     (New_Deadline : in Ada.Real_Time.Time_Span);

   procedure Cycle_Completed;

end ARPART.Tasks;
