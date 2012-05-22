package body Oak.Oak_Time is

   -----------
   -- Clock --
   -----------

   function Clock return Time is
   begin
      return Time (Oak_CSP_Time.Get_Clock);
   end Clock;

end Oak.Oak_Time;
