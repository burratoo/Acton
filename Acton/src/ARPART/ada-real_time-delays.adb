with ARPART.Tasks;
with Oak.Real_Time.Conversion; use Oak.Real_Time.Conversion;

with Oak.Agent.Tasks; use Oak.Agent.Tasks;

package body Ada.Real_Time.Delays is
   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      Message : constant Oak_Task_Message :=
        (Message_Type => Sleeping,
         Wake_Up_At   => To_Oak_Time (T));
   begin
      ARPART.Tasks.Yield_Processor_To_Kernel (Task_Message => Message);
   end Delay_Until;

end Ada.Real_Time.Delays;
