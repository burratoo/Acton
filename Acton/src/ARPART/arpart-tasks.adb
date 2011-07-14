with Oak.Processor_Support_Package.Task_Support;
with Oak.Oak_Task;
with Oak.Oak_Task.Data_Access;
with Oak.Core;
with Ada.Real_Time;

package body ARPART.Tasks is

   package OT renames Oak.Oak_Task;
   package OTS renames Oak.Processor_Support_Package.Task_Support;

   use type Oak.Oak_Task.Task_State;

   -----------------------------
   -- Engage_Activation       --
   -----------------------------
   -- Called by the Activator --
   -----------------------------

   procedure Engage_Activation is
      Self     : constant access OT.Oak_Task       :=
         Oak.Core.Get_Current_Task;
      AP_State : constant OTS.Task_Requested_State :=
        (Type_State => OTS.Other,
         State      => OT.Activation_Pending,
         Parameter  => OTS.Empty_Parameter);
      AC_State : constant OTS.Task_Requested_State :=
        (Type_State => OTS.Other,
         State      => OT.Activation_Complete,
         Parameter  => OTS.Empty_Parameter);
      TP : access OT.Oak_Task := OT.Data_Access.Get_Activation_List (Self);
   begin
      while TP /= null and then OT.Data_Access.Is_Elaborated (TP) loop
         TP := OT.Data_Access.Get_Activation_List (TP);
      end loop;

      if TP /= null and then OT.Data_Access.Is_Elaborated (TP) = False then
         raise Program_Error with "task bodies not elaborated";
      end if;

      OTS.Yield_Processor_To_Kernel (Resulting_Task_State => AP_State);

      if OT.Data_Access.Get_State (Self) = OT.Activation_Successful then
         OTS.Yield_Processor_To_Kernel (Resulting_Task_State => AC_State);
      else
         raise Tasking_Error with "Failure during activation";
      end if;

   end Engage_Activation;

   -----------------------------
   -- Complete_Activation     --
   -----------------------------
   -- Called by the Activatee --
   -----------------------------

   procedure Complete_Activation is
      Self     : constant access OT.Oak_Task       :=
         Oak.Core.Get_Current_Task;
      AS_State : constant OTS.Task_Requested_State :=
        (Type_State => OTS.Other,
         State      => OT.Activation_Successful,
         Parameter  => OTS.Empty_Parameter);
   begin
      OTS.Yield_Processor_To_Kernel (Resulting_Task_State => AS_State);
      if OT.Data_Access.Get_State (Self) = OT.Activation_Successful then
         OTS.Yield_Processor_To_Kernel (Resulting_Task_State => AS_State);
      else
         --  Need to include a cleanup routine here, though
         --  for a Ravenscar Profile system that isn't required as no
         --  tasks will be running on the system.
         raise Tasking_Error with "Failure during activation";
      end if;
   end Complete_Activation;

   --  Trival complete task.
   procedure Complete_Task is
      State : constant OTS.Task_Requested_State :=
                (Type_State => OTS.Sleeping,
                 State      => OT.Sleeping,
                 Wake_Up_At => Ada.Real_Time.Time_Last);
   begin
      OTS.Yield_Processor_To_Kernel (Resulting_Task_State => State);
   end Complete_Task;

end ARPART.Tasks;
