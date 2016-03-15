with Ada.Containers;
with Ada.Containers.Ordered_Sets;

with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;

with Acton_Properties; use Acton_Properties;
with Analyser_Support; use Analyser_Support;

with Asis.Text;

package Schedulers is

   type Scheduler_Info is private;

   procedure Add_Scheduler
     (Name          : in Wide_String;
      Low_Priority  : in Integer;
      High_Priority : in Integer;
      Storage_Size  : in Integer;
      Source        : in Wide_String;
      Line          : in Asis.Text.Line_Number);

   procedure Print_Schedulers;

private

   type Scheduler_Info is record
      Scheduler_Name  : Unbounded_Wide_String;
      Low_Priority    : Integer;
      High_Priority   : Integer;
      Pragma_Location : Element_Location := No_Location;
      Storage_Size    : Integer := Uninitialised_Storage_Size;
   end record;

   function "<" (Left, Right : Scheduler_Info) return Boolean is
     (Left.High_Priority < Right.Low_Priority);

   function "=" (Left, Right : Scheduler_Info) return Boolean is
     (Left.Low_Priority = Right.Low_Priority and
        Left.High_Priority = Right.High_Priority);

   package Scheduler_Store is new
     Ada.Containers.Ordered_Sets (Element_Type => Scheduler_Info,
                                  "<"          => "<",
                                  "="          => "=");

   Schedulers : Scheduler_Store.Set;
   use Scheduler_Store;

end Schedulers;
