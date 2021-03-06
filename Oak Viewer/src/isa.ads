------------------------------------------------------------------------------------------
--                                                                                      --
--                                      OAK VIEWER                                      --
--                                                                                      --
--                                         ISA                                          --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package ISA with Pure is

   type Enable_Type is (Disable, Enable);
   for Enable_Type use (Disable => 0, Enable => 1);

   type Enabled_Type is (Disabled, Enabled);
   for Enabled_Type use (Disabled => 0, Enabled => 1);

   type Decision_Type is (No, Yes);
   for Decision_Type use (No => 0, Yes => 1);

end ISA;
