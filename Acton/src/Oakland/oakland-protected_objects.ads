------------------------------------------------------------------------------
--                                                                          --
--                            OAKLAND COMPONENTS                            --
--                                                                          --
--                         OAKLAND.PROTECTED_OBJECTS                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2011-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Brokers; use Oak.Brokers;
with Oak.Indices; use Oak.Indices;
with Oak.Message; use Oak.Message;

with System; use System;

package Oakland.Protected_Objects with Preelaborate is

   type Entry_Barrier_Function_Handler is
     access function (PO : System.Address;
                      E  : Protected_Entry_Index) return Boolean;

   procedure Enter_Protected_Object
     (PO              : in Protected_Id;
      Subprogram_Kind : in Protected_Subprogram_Type;
      Entry_Id        : in Entry_Index := No_Entry);

   procedure Exit_Protected_Object (PO : Protected_Id);

   function Entry_Count
     (PO       : in Protected_Id;
      Entry_Id : in Entry_Index) return Natural;

end Oakland.Protected_Objects;
