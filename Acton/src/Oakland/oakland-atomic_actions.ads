with Oak.Atomic_Actions; use Oak.Atomic_Actions;
with Oak.Indices; use Oak.Indices;

package Oakland.Atomic_Actions is
   procedure Enter_Action
     (AO        : not null access Atomic_Object;
      Action_Id : in Action_Index);

   procedure Action_End_Barrier
     (AO               : not null access Atomic_Object;
      Action_Id        : in Action_Index;
      Exception_Raised : in out Boolean);

   procedure Exit_Action
     (AO               : not null access Atomic_Object;
      Action_Id        : in Action_Index;
      Exception_Raised : in Boolean);

end Oakland.Atomic_Actions;
