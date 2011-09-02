with Oak.Oak_Task;
with Oak.Processor_Support_Package.Task_Support;

package body Ada.Real_Time.Delays is
   package OT renames Oak.Oak_Task;
   package OTS renames Oak.Processor_Support_Package.Task_Support;
   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      State : constant OTS.Task_Requested_State :=
        (State      => OT.Sleeping,
         Type_State => OTS.Sleeping,
         Wake_Up_At => T);
   begin
      OTS.Yield_Processor_To_Kernel (Resulting_Task_State => State);
   end Delay_Until;

end Ada.Real_Time.Delays;
