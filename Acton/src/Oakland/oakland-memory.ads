------------------------------------------------------------------------------------------
--                                                                                      --
--                                  OAKLAND COMPONENTS                                  --
--                                                                                      --
--                                    OAKLAND.MEMORY                                    --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package Oakland.Memory with Pure is
   procedure Malloc
     with Export, Convention => C, External_Name => "__gnat_malloc";
end Oakland.Memory;
