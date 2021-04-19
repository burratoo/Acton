------------------------------------------------------------------------------------------
--                                                                                      --
--                                  OAKLAND COMPONENTS                                  --
--                                                                                      --
--                                    OAKLAND.MEMORY                                    --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package body Oakland.Memory is
   procedure Malloc is
   begin
      raise Storage_Error;
   end Malloc;
end Oakland.Memory;
