with Oak.Processor_Support_Package.Task_Support;

package body Oak.Oak_Task.Context is
   package TS renames Oak.Processor_Support_Package.Task_Support;

   --------------------------
   -- Task_Cycle_Completed --
   --------------------------

   procedure Task_Cycle_Completed is
      State : constant TS.Task_Requested_State :=
        (Type_State => TS.Other,
         State      => Cycle_Completed,
         Parameter  => (0, 0));
   begin
      TS.Yield_Processor_To_Kernel (Resulting_Task_State => State);
   end Task_Cycle_Completed;

   ----------------
   -- Sleep_Task --
   ----------------

   procedure Sleep_Task (Wake_At : Time) is
      State : constant TS.Task_Requested_State :=
        (Type_State => TS.Sleeping,
         State      => Sleeping,
         Wake_Up_At => Wake_At);
   begin
      TS.Yield_Processor_To_Kernel (Resulting_Task_State => State);
   end Sleep_Task;

end Oak.Oak_Task.Context;
