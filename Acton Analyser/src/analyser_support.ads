------------------------------------------------------------------------------------------
--                                                                                      --
--                                    ACTON ANALYSER                                    --
--                                                                                      --
--                                   ANALYSER_SUPPORT                                   --
--                                                                                      --
--                       Copyright (C) 2016-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Asis;
with Asis.Text;

with Ada.Real_Time;
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with GNAT;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package Analyser_Support is
   type Element_Location is record
      Source_Name : Unbounded_Wide_String;
      Line_Number : Asis.Text.Line_Number;
   end record;

   No_Location : constant Element_Location :=
                   (Source_Name => Null_Unbounded_Wide_String,
                    Line_Number => 0);

   type Supported_Aspects is
     (Priority,
      Cycle_Behaviour,
      Cycle_Period,
      Cycle_Phase,
      Relative_Deadline,
      Execution_Budget);

   function Get_Static_Integer (E : Asis.Expression) return Integer;
   --  Returns the integer value of the expression E. Note that it assumes that
   --  the return type is an integer.

   type Bound_End is (Lower, Upper);
   function Get_Bound (D : Asis.Declaration; Bound : Bound_End) return Integer;

   function Calculate_Static_Time_Span
     (For_Expression : Asis.Expression)
      return Ada.Real_Time.Time_Span;

   function Expanded_Name (Defining_Name : Asis.Defining_Name) return Wide_String;

   function Format_Property_String (S : Wide_String; Length : Integer)
                                    return Wide_String;

   function Element_Location_Image (EL : Element_Location) return Wide_String;

   function Image (J : Integer) return Wide_String;


   No_Value_Image  : constant Wide_String := "👻";
   Tree            : constant Wide_String := "🌳 ";
   Line_Decoration : constant Unbounded_Wide_String := 50 * Tree;

   Supported_GCC : constant String_Access :=
                     GNAT.OS_Lib.Locate_Exec_On_Path ("arm-burratoo-acton-gcc");

end Analyser_Support;
