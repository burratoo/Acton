with Oak.Processor_Support_Package.Task_Support;
with Oak.Oak_Task.Data_Access;
with Oak.Core;
use Oak.Oak_Task;

package body ARPART.Tasks is

   -----------------------------
   -- Engage_Activation       --
   -----------------------------
   -- Called by the Activator --
   -----------------------------

   procedure Activate_Tasks (Chain : Oak.Oak_Task.Activation_Chain_Access) is
      Self     : constant access Oak_Task       :=
         Oak.Core.Get_Current_Task;
      Activation_Pending_Message : constant Oak_Task_Message
        := (Message_Type => Activation_Pending);
      Activation_Complete_Message : constant Oak_Task_Message :=
        (Message_Type      => Activation_Complete);
   begin
      Data_Access.Set_Activation_List (T => Self, Chain => Chain);
      Yield_Processor_To_Kernel
        (Task_Message => Activation_Pending_Message);

      if Data_Access.Get_State (Self) = Activation_Successful then
         Yield_Processor_To_Kernel
           (Task_Message => Activation_Complete_Message);
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
      Activation_Successful_Message : constant Oak_Task_Message :=
        (Message_Type      => Activation_Successful);
   begin
      Yield_Processor_To_Kernel
        (Task_Message => Activation_Successful_Message);
      if Data_Access.Get_State (Self) = Activation_Successful then
         Yield_Processor_To_Kernel
           (Task_Message => Activation_Successful_Message);
      else
         --  Need to include a cleanup routine here, though for a Ravenscar
         --  Profile system that isn't required as no tasks will be running on
         --  the system.
         raise Tasking_Error with "Failure during activation";
      end if;
   end Complete_Activation;

   --  Trival complete task.
   procedure Complete_Task is
      Message : constant Oak_Task_Message :=
        (Message_Type => Sleeping,
         Wake_Up_At   => Ada.Real_Time.Time_Last);
   begin
      Yield_Processor_To_Kernel (Task_Message => Message);
   end Complete_Task;

   procedure Change_Cycle_Period (New_Period : in Ada.Real_Time.Time_Span) is
      Message : constant Oak_Task_Message :=
        (Message_Type         => Change_Cycle_Period,
         New_Cycle_Period => New_Period);
   begin
      Yield_Processor_To_Kernel (Task_Message => Message);
   end Change_Cycle_Period;

   procedure Change_Relative_Deadline
     (New_Deadline : in Ada.Real_Time.Time_Span)
   is
      Message : constant Oak_Task_Message :=
        (Message_Type         => Change_Relative_Deadline,
         New_Deadline_Span => New_Deadline);
   begin
      Yield_Processor_To_Kernel (Task_Message => Message);
   end Change_Relative_Deadline;

   procedure Cycle_Completed is
      Message : constant Oak_Task_Message :=
        (Message_Type => Cycle_Completed);
   begin
      Yield_Processor_To_Kernel (Task_Message => Message);
   end Cycle_Completed;

   procedure Yield_Processor_To_Kernel
     (Task_Message : OT.Oak_Task_Message) is
   begin
      OT.Data_Access.Store_Oak_Task_Message
        (For_Task     => Oak.Core.Get_Current_Task,
         Message      => Task_Message);
      Oak.Processor_Support_Package.Task_Support.Yield_Processor_To_Kernel;
   end Yield_Processor_To_Kernel;
end ARPART.Tasks;
