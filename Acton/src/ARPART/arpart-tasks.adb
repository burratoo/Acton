with Oak.Processor_Support_Package.Task_Support;
with Oak.Oak_Task.Data_Access;
with Oak.Core;
use Oak.Oak_Task;

package body ARPART.Tasks is

   package OTS renames Oak.Processor_Support_Package.Task_Support;

   -----------------------------
   -- Engage_Activation       --
   -----------------------------
   -- Called by the Activator --
   -----------------------------

   procedure Activate_Tasks (Chain : Oak.Oak_Task.Activation_Chain_Access) is
      Self     : constant access Oak_Task       :=
         Oak.Core.Get_Current_Task;
      AP_State : constant Task_Requested_State
        := (State => Activation_Pending);
      AC_State : constant Task_Requested_State :=
        (State      => Activation_Complete);
   begin
      Data_Access.Set_Activation_List (T => Self, Chain => Chain);
      OTS.Yield_Processor_To_Kernel (Resulting_Task_State => AP_State);

      if Data_Access.Get_State (Self) = Activation_Successful then
         OTS.Yield_Processor_To_Kernel (Resulting_Task_State => AC_State);
      else
         raise Tasking_Error with "Failure during activation";
      end if;

   end Activate_Tasks;

   -----------------------------
   -- Complete_Activation     --
   -----------------------------
   -- Called by the Activatee --
   -----------------------------

   procedure Complete_Activation is
      Self     : constant access Oak_Task := Oak.Core.Get_Current_Task;
      AS_State : constant Task_Requested_State :=
        (State      => Activation_Successful);
   begin
      OTS.Yield_Processor_To_Kernel (Resulting_Task_State => AS_State);
      if Data_Access.Get_State (Self) = Activation_Successful then
         OTS.Yield_Processor_To_Kernel (Resulting_Task_State => AS_State);
      else
         --  Need to include a cleanup routine here, though for a Ravenscar
         --  Profile system that isn't required as no tasks will be running on
         --  the system.
         raise Tasking_Error with "Failure during activation";
      end if;
   end Complete_Activation;

   --  Trival complete task.
   procedure Complete_Task is
      State : constant Task_Requested_State :=
        (State      => Sleeping,
         Wake_Up_At => Ada.Real_Time.Time_Last);
   begin
      OTS.Yield_Processor_To_Kernel (Resulting_Task_State => State);
   end Complete_Task;

   procedure Change_Cycle_Period (New_Period : in Ada.Real_Time.Time_Span) is
      State : constant Task_Requested_State :=
        (State         => Change_Cycle_Period,
         New_Cycle_Period => New_Period);
   begin
      OTS.Yield_Processor_To_Kernel (Resulting_Task_State => State);
   end Change_Cycle_Period;

   procedure Change_Relative_Deadline
     (New_Deadline : in Ada.Real_Time.Time_Span)
   is
      State : constant Task_Requested_State :=
        (State         => Change_Relative_Deadline,
         New_Deadline_Span => New_Deadline);
   begin
      OTS.Yield_Processor_To_Kernel (Resulting_Task_State => State);
   end Change_Relative_Deadline;

   procedure Cycle_Completed is
      State : constant Task_Requested_State :=
        (State => Cycle_Completed);
   begin
      OTS.Yield_Processor_To_Kernel (Resulting_Task_State => State);
   end Cycle_Completed;

end ARPART.Tasks;
