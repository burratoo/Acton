package body Oakland.Memory is
   procedure Malloc is
   begin
      raise Storage_Error;
   end Malloc;
end Oakland.Memory;
