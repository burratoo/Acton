with Oak.Atomic_Actions; use Oak.Atomic_Actions;
with Oak.Indices; use Oak.Indices;

package ARPART.Atomic_Actions is
   procedure Enter_Action
     (Atomic_Action : not null access Atomic_Object;
      Action_Id     : Action_Index);

   procedure Action_End_Barrier
     (Atomic_Action : not null access Atomic_Object;
      Action_Id     : Action_Index;
      Exception_Raised : Boolean);

   procedure Exit_Action
     (Atomic_Action : not null access Atomic_Object;
      Action_Id     : Action_Index);

end ARPART.Atomic_Actions;
