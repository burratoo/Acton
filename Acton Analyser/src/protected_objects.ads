with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Wide_Unbounded.Wide_Hash;
with Ada.Real_Time;

with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;

with Asis.Text;

with Acton_Properties; use Acton_Properties;
with Analyser_Support; use Analyser_Support;

package Protected_Objects is

   type Protected_Unit_Info is private;
   type Protected_Object_Info is private;

   procedure Add_Protected_Body_Information
     (Name   : in Wide_String;
      Source : in Wide_String;
      Line   : in Asis.Text.Line_Number);

   procedure Add_Protected_Unit_Declaration
     (Name         : in Wide_String;
      Is_Singleton : in Boolean;
      Source       : in Wide_String;
      Line         : in Asis.Text.Line_Number);

   procedure Add_Protected_Object_Declaration
     (Name              : in Wide_String;
      Unit_Name         : in Wide_String;
      Number_Of_Objects : in Integer;
      Source            : in Wide_String;
      Line              : in Asis.Text.Line_Number);

   procedure Add_Priority
     (Name     : in Wide_String;
      Priority : in Integer);

   procedure Print_Protected_Units;
   procedure Print_Protected_Objects;

   function Total_Number_Of_Protected_Objects return Integer;

private

   type Protected_Unit_Info is record
      Declaration_Location : Element_Location := No_Location;
      Body_Location        : Element_Location := No_Location;
      Is_Singleton         : Boolean          := False;
      Priority             : Integer          := Uninitialised_Priority;
   end record;

   package Protected_Unit_Store is new
     Ada.Containers.Hashed_Maps (Key_Type        => Unbounded_Wide_String,
                                 Element_Type    => Protected_Unit_Info,
                                 Hash            => Ada.Strings.Wide_Unbounded.Wide_Hash,
                                 Equivalent_Keys => "=",
                                 "="             => "=");

   Protected_Units : Protected_Unit_Store.Map;
   use Protected_Unit_Store;

   type Protected_Object_Info is record
      Object_Location   : Element_Location            := No_Location;
      Unit_Declaration  : Protected_Unit_Store.Cursor := No_Element;
      Number_Of_Objects : Integer                     := 0;
   end record;

   package Protected_Object_Store is new
     Ada.Containers.Hashed_Maps (Key_Type        => Unbounded_Wide_String,
                                 Element_Type    => Protected_Object_Info,
                                 Hash            => Ada.Strings.Wide_Unbounded.Wide_Hash,
                                 Equivalent_Keys => "=",
                                 "="             => "=");

   Protected_Objects : Protected_Object_Store.Map;
   use Protected_Object_Store;

end Protected_Objects;
