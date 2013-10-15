with Oak.Indices; use Oak.Indices;
with Oak.Message;

limited with Oak.Agent.Tasks.Protected_Objects;
limited with Oak.Protected_Objects;

package Oakland.Protected_Objects with Preelaborate is

   procedure Enter_Protected_Object
     (PO              : not null access
        Oak.Agent.Tasks.Protected_Objects.Protected_Agent'Class;
      Subprogram_Kind : in Oak.Message.Protected_Subprogram_Type;
      Entry_Id        : in Entry_Index := No_Entry);

   procedure Exit_Protected_Object
     (PO : not null access
        Oak.Agent.Tasks.Protected_Objects.Protected_Agent'Class);

   function Entry_Count
     (PO       : not null access
        Oak.Agent.Tasks.Protected_Objects.Protected_Agent'Class;
      Entry_Id : in Entry_Index) return Natural;

end Oakland.Protected_Objects;
