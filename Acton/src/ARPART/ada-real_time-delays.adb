with Oak.Oak_Task;
with ARPART.Tasks;
use Oak.Oak_Task;

package body Ada.Real_Time.Delays is
   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      Message : constant Oak_Task_Message :=
        (Message_Type => Sleeping,
         Wake_Up_At   => T);
   begin
      ARPART.Tasks.Yield_Processor_To_Kernel (Task_Message => Message);
   end Delay_Until;

end Ada.Real_Time.Delays;
