--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                           OAK.PROTECTED_OBJECTS                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2011-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with Oak.Agent;   use Oak.Agent;
with Oak.Brokers; use Oak.Brokers;
with Oak.Indices; use Oak.Indices;
with Oak.Message; use Oak.Message;

package Oak.Protected_Objects with Preelaborate is

   procedure Acquire_Protected_Object_For_Interrupt (PO : in Protected_Id);
   --  Acquires a protected object without the overhead that a normal enter
   --  request would have since the interrupt should already be at the ceiling
   --  priority of the protected object.

   procedure Process_Enter_Request
     (Entering_Agent  : in Task_Id;
      PO              : in Protected_Id;
      Subprogram_Kind : in Protected_Subprogram_Type;
      Entry_Id        : in Entry_Index);
   --  Processess a task's entry request to the desired protected object.

   procedure Process_Exit_Request
     (Exiting_Agent : in Task_Id;
      PO            : in Protected_Id);
   --  Processess a task's exit request from the desired protected object.

   procedure Process_Interrupt_Exit (PO : in Protected_Id);
   --  Processess an interrupt's exit request from the desired protected
   --  object.

   procedure Release_Protected_Object_For_Interrupt (PO : in Protected_Id);
   --  Releases the protected object that was possesed by an interrupt.

end Oak.Protected_Objects;
