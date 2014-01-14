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
with Oak.Indices; use Oak.Indices;
with Oak.Message; use Oak.Message;

package Oak.Protected_Objects with Preelaborate is

   procedure Acquire_Protected_Object_For_Interrupt (PO : in Protected_Id);
   --  Acquires a protected object without the overhead that a normal enter
   --  request would have since the interrupt should already be at the ceiling
   --  priority of the protected object.

   procedure Process_Enter_Request
     (Entering_Agent    : in  Task_Id;
      PO                : in  Protected_Id;
      Subprogram_Kind   : in  Protected_Subprogram_Type;
      Entry_Id          : in  Entry_Index;
      Next_Agent_To_Run : out Oak_Agent_Id;
      Resubmitted       : in  Boolean := False);
   --  Processess a task's entry request to the desired protected object.
   --  If Resubmitted is false it means that this is the first enter request,
   --  and the entering task is removed from its scheduler. If true, it means
   --  the task has already made one request to the object and is already not
   --  part of its scheduler.

   procedure Process_Exit_Request
     (Exiting_Agent     : in Task_Id;
      PO                : in  Protected_Id;
      Next_Agent_To_Run : out Oak_Agent_Id);
   --  Processess a task's exit request from the desired protected object.

   procedure Release_Protected_Object_For_Interrupt (PO : in Protected_Id);
   --  Releases the protected object that was possesed by an interrupt.

end Oak.Protected_Objects;
