------------------------------------------------------------------------------------------
--                                                                                      --
--                                  OAKLAND COMPONENTS                                  --
--                                                                                      --
--                                 ADA.REAL_TIME.DELAYS                                 --
--                                                                                      --
--                       Copyright (C) 2011-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Oakland.Tasks;
with Oak.Oak_Time.Conversion; use Oak.Oak_Time.Conversion;

with Oak.Message; use Oak.Message;
with Oak.States;  use Oak.States;

package body Ada.Real_Time.Delays is
   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until (T : Time) is
      Message : Oak_Message :=
        (Message_Type            => Sleeping,
         Wake_Up_At              => To_Oak_Time (T),
         Remove_From_Charge_List => True);
   begin
      Oakland.Tasks.Yield_Processor_To_Kernel (With_Message => Message);
   end Delay_Until;

end Ada.Real_Time.Delays;
