with Oak.Oak_Task.Context;

package body Ada.Real_Time.Delays is

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
   begin
      Oak.Oak_Task.Context.Sleep_Task (Wake_At => T);
   end Delay_Until;

end Ada.Real_Time.Delays;
