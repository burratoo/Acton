------------------------------------------------------------------------------------------
--                                                                                      --
--                                    ACTON ANALYSER                                    --
--                                                                                      --
--                                        TASKS                                         --
--                                                                                      --
--                       Copyright (C) 2016-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Wide_Unbounded.Wide_Hash;
with Ada.Real_Time;

with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;

with Asis.Text;

with Acton_Properties; use Acton_Properties;
with Analyser_Support; use Analyser_Support;

package Tasks is

   type Cyclic_Kind is (Normal, Aperiodic, Sporadic, Periodic);

   type Event_Response is (No_Response, Handler, Abort_Cycle,
                           Abort_And_Raise_Exception);

   type Task_Unit_Info is private;
   type Task_Decls is private;

   procedure Add_Task_Body_Information
     (Name               : in Wide_String;
      Has_Cyclic_Section : in Boolean;
      Is_Main_Task       : in Boolean;
      Source             : in Wide_String;
      Line               : in Asis.Text.Line_Number);

   procedure Add_Task_Unit_Declaration
     (Name         : in Wide_String;
      Is_Main_Task : in Boolean;
      Is_Singleton : in Boolean;
      Source       : in Wide_String;
      Line         : in Asis.Text.Line_Number);

   procedure Add_Task_Object_Declaration
     (Name             : in Wide_String;
      Unit_Name        : in Wide_String;
      Is_Main_Task     : in Boolean;
      Number_Of_Tasks  : in Integer;
      Source           : in Wide_String;
      Line             : in Asis.Text.Line_Number);

   procedure Add_Cycle_Behaviour
     (Name      : in Wide_String;
      Behaviour : in Cyclic_Kind);

   procedure Add_Priority
     (Name     : in Wide_String;
      Priority : in Integer);

   procedure Add_Storage_Size
     (Name : in Wide_String;
      Size : in Integer);

   procedure Add_Cycle_Period_Span
     (Name   : in Wide_String;
      Period : in Ada.Real_Time.Time_Span);

   procedure Add_Cycle_Phase_Span
     (Name  : in Wide_String;
      Phase : in Ada.Real_Time.Time_Span);

   procedure Add_Relative_Deadline
     (Name     : in Wide_String;
      Deadline : in Ada.Real_Time.Time_Span);

   procedure Add_Exceution_Deadline
     (Name     : in Wide_String;
      Deadline : in Ada.Real_Time.Time_Span);

   procedure Print_Task_Units;
   procedure Print_Task_Objects;

   function Total_Number_Of_Task_Objects return Integer;
   function Stack_Space_Allocated_To_Task_Objects return Integer;

private

   type Task_Unit_Info is record
      Declaration_Location : Element_Location        := No_Location;
      Body_Location        : Element_Location        := No_Location;
      Is_Singleton         : Boolean                 := False;
      Is_Main_Task         : Boolean                 := False;
      Has_Cyclic_Section   : Boolean                 := False;
      Cyclic_Behaviour     : Cyclic_Kind             := Normal;
      Priority             : Integer                 := Uninitialised_Priority;
      Storage_Size         : Integer                 := Uninitialised_Storage_Size;
      Cycle_Period         : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_First;
      Cycle_Phase          : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
      Execution_Budget     : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_First;
      Relative_Deadline    : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_First;
   end record;
   --  Ada.Real_Time.Time_Span is used here since it provides nanosecond
   --  resolution on the host. Should really use a lookup table for the
   --  strings.

   package Task_Unit_Store is new
     Ada.Containers.Hashed_Maps (Key_Type        => Unbounded_Wide_String,
                                 Element_Type    => Task_Unit_Info,
                                 Hash            => Ada.Strings.Wide_Unbounded.Wide_Hash,
                                 Equivalent_Keys => "=",
                                 "="             => "=");

   Task_Units : Task_Unit_Store.Map;
   use Task_Unit_Store;

   type Task_Decls is record
      Object_Location  : Element_Location       := No_Location;
      Unit_Declaration : Task_Unit_Store.Cursor := No_Element;
      Number_Of_Tasks  : Integer                := 0;
      Is_Main_Task     : Boolean                := False;
   end record;

   package Task_Object_Store is new
     Ada.Containers.Hashed_Maps (Key_Type        => Unbounded_Wide_String,
                                 Element_Type    => Task_Decls,
                                 Hash            => Ada.Strings.Wide_Unbounded.Wide_Hash,
                                 Equivalent_Keys => "=",
                                 "="             => "=");

   Task_Objects : Task_Object_Store.Map;
   use Task_Object_Store;

end Tasks;
