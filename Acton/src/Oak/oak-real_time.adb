package body Oak.Real_Time is

   -----------
   -- Clock --
   -----------

   function Clock return Time is
   begin
      return Time (Oak_CSP_Time.Get_Clock);
   end Clock;

end Oak.Real_Time;
