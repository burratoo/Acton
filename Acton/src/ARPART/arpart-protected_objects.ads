with Oak.Protected_Objects;

with Oak.Agent.Tasks.Protected_Objects; use Oak.Agent.Tasks.Protected_Objects;
with Oak.Entries; use Oak.Entries;

package ARPART.Protected_Objects with Preelaborate is

   procedure Enter_Protected_Object
     (PO              : not null access Protected_Agent'Class;
      Subprogram_Kind : in Oak.Protected_Objects.Protected_Subprogram_Type;
      Entry_Id        : in Entry_Index := No_Entry);

   procedure Exit_Protected_Object
     (PO : not null access Protected_Agent'Class);

   function Entry_Count
     (PO       : not null access Protected_Agent'Class;
      Entry_Id : in Entry_Index) return Natural;

end ARPART.Protected_Objects;
