------------------------------------------------------------------------------------------
--                                                                                      --
--                                    OAK COMPONENTS                                    --
--                                                                                      --
--                                      OAK.BROKERS                                     --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

--  This package defines the types that define Oak's brokers. They are housed
--  together in this package together to prevent circular dependencies
--  among the Broker children.

--    Protected_Broker   Broker that represents protected objects.

with Oak.Project_Support_Package; use Oak.Project_Support_Package;

package Oak.Brokers with Pure is

   Protected_Id_Low_Bound  : constant := 1;
   Protected_Id_High_Bound : constant := Max_Protected_Agents;

   type Protected_Id_With_No is mod Protected_Id_High_Bound + 1;
   subtype Protected_Id is Protected_Id_With_No range
     Protected_Id_Low_Bound .. Protected_Id_High_Bound;
   No_Protected_Object : constant Protected_Id_With_No :=
                           Protected_Id_With_No'First;

end Oak.Brokers;
