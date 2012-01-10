with Oak.Oak_Task;
with Oak.Processor_Support_Package.Task_Support;
use Oak.Oak_Task;

package body Ada.Real_Time.Delays is
   package OTS renames Oak.Processor_Support_Package.Task_Support;
   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      Message : constant Oak_Task_Message :=
        (Message_Type => Sleeping,
         Wake_Up_At   => T);
   begin
      OTS.Yield_Processor_To_Kernel (Task_Message => Message);
   end Delay_Until;

end Ada.Real_Time.Delays;
