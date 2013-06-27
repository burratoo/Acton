with Oakland.Tasks;
with Oak.Oak_Time.Conversion; use Oak.Oak_Time.Conversion;

with Oak.Message; use Oak.Message;
with Oak.States;  use Oak.States;

package body Ada.Real_Time.Delays is
   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      Message : constant Oak_Message :=
        (Message_Type => Sleeping,
         Wake_Up_At   => To_Oak_Time (T));
   begin
      Oakland.Tasks.Yield_Processor_To_Kernel (Task_Message => Message);
   end Delay_Until;

end Ada.Real_Time.Delays;
