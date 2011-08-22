with Oak.Processor_Support_Package.Task_Support;
with Oak.Oak_Task.Context;
with Oak.Oak_Task.Data_Access;
with Oak.Core;

package body ARPART.Tasks is

   package OT renames Oak.Oak_Task;
   package OTS renames Oak.Processor_Support_Package.Task_Support;

   use type Oak.Oak_Task.Task_State;

   -----------------------------
   -- Engage_Activation       --
   -----------------------------
   -- Called by the Activator --
   -----------------------------

   procedure Activate_Tasks (Chain : Oak.Oak_Task.Activation_Chain_Access) is
      Self     : constant access OT.Oak_Task       :=
         Oak.Core.Get_Current_Task;
      AP_State : constant OTS.Task_Requested_State :=
        (State      => OT.Activation_Pending,
         Type_State => OTS.Other,
         Parameter  => OTS.Empty_Parameter);
      AC_State : constant OTS.Task_Requested_State :=
        (State      => OT.Activation_Complete,
         Type_State => OTS.Other,
         Parameter  => OTS.Empty_Parameter);
   begin
      OT.Data_Access.Set_Activation_List (T     => Self,
                                          Chain => Chain);
      OTS.Yield_Processor_To_Kernel (Resulting_Task_State => AP_State);

      if OT.Data_Access.Get_State (Self) = OT.Activation_Successful then
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
      Self     : constant access OT.Oak_Task       :=
         Oak.Core.Get_Current_Task;
      AS_State : constant OTS.Task_Requested_State :=
        (State      => OT.Activation_Successful,
         Type_State => OTS.Other,
         Parameter  => OTS.Empty_Parameter);
   begin
      OTS.Yield_Processor_To_Kernel (Resulting_Task_State => AS_State);
      if OT.Data_Access.Get_State (Self) = OT.Activation_Successful then
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
      State : constant OTS.Task_Requested_State :=
        (State      => OT.Sleeping,
         Type_State => OTS.Sleeping,
         Wake_Up_At => Ada.Real_Time.Time_Last);
   begin
      OTS.Yield_Processor_To_Kernel (Resulting_Task_State => State);
   end Complete_Task;

   procedure Change_Cycle_Period (New_Period : in Ada.Real_Time.Time_Span) is
      State : constant OTS.Task_Requested_State :=
        (State         => OT.Change_Cycle_Period,
         Type_State    => OTS.Change_Time_Span,
         New_Time_Span => New_Period);
   begin
      OTS.Yield_Processor_To_Kernel (Resulting_Task_State => State);
   end Change_Cycle_Period;

   procedure Change_Relative_Deadline
     (New_Deadline : in Ada.Real_Time.Time_Span)
   is
      State : constant OTS.Task_Requested_State :=
        (State         => OT.Change_Relative_Deadline,
         Type_State    => OTS.Change_Time_Span,
         New_Time_Span => New_Deadline);
   begin
      OTS.Yield_Processor_To_Kernel (Resulting_Task_State => State);
   end Change_Relative_Deadline;

   procedure Cycle_Completed is
   begin
      Oak.Oak_Task.Context.Task_Cycle_Completed;
   end Cycle_Completed;

end ARPART.Tasks;
