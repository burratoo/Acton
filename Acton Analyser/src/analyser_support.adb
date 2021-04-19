------------------------------------------------------------------------------------------
--                                                                                      --
--                                    ACTON ANALYSER                                    --
--                                                                                      --
--                                   ANALYSER_SUPPORT                                   --
--                                                                                      --
--                       Copyright (C) 2016-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;

with Asis.Declarations;
with Asis.Definitions;
with Asis.Elements;
with Asis.Expressions;
with Asis.Extensions;
with Asis.Set_Get;

with Ada.Wide_Text_IO;       use Ada.Wide_Text_IO;
with Ada.Wide_Text_IO.Wide_Unbounded_IO; use Ada.Wide_Text_IO.Wide_Unbounded_IO;
with Ada.Strings.Wide_Fixed; use Ada.Strings.Wide_Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Uintp; use Uintp;

with A4G.DDA_Aux; use A4G.DDA_Aux;

package body Analyser_Support is

   function Get_Static_Integer (E : Asis.Expression) return Integer is
   begin
      case Asis.Elements.Expression_Kind (E)  is
         when Asis.An_Integer_Literal =>
            return Integer (UI_To_Int (Eval_Scalar_Node (Asis.Set_Get.Node (E))));
         when Asis.A_Function_Call =>
            declare
               Operator          : constant Asis.Operator_Kinds := Asis.Elements.Operator_Kind (Asis.Expressions.Prefix (E));
               Func_Params       : constant Asis.Expression_List := Asis.Expressions.Function_Call_Parameters (E);
               First_Func_Param  : constant Asis.Expression := Asis.Expressions.Actual_Parameter (Func_Params (1));
               Second_Func_Param : constant Asis.Expression := Asis.Expressions.Actual_Parameter (Func_Params (2));
            begin
               case Operator is
                  when Asis.A_Multiply_Operator =>
                     return Get_Static_Integer (First_Func_Param) * Get_Static_Integer (Second_Func_Param);
                  when Asis.A_Plus_Operator =>
                     return Get_Static_Integer (First_Func_Param) + Get_Static_Integer (Second_Func_Param);
                  when Asis.A_Minus_Operator =>
                     return Get_Static_Integer (First_Func_Param) - Get_Static_Integer (Second_Func_Param);
                  when Asis.A_Divide_Operator =>
                     return Get_Static_Integer (First_Func_Param) / Get_Static_Integer (Second_Func_Param);
                  when others =>
                     raise Program_Error with "Unsupported Operator";
               end case;
            end;

         when Asis.A_Parenthesized_Expression =>
            return Get_Static_Integer (Asis.Expressions.Expression_Parenthesized (E));

         when Asis.An_Attribute_Reference =>
            declare
               Prefix_Declaration : constant Asis.Element := Asis.Expressions.Corresponding_Name_Declaration (Asis.Expressions.Prefix (E));
            begin
               case Asis.Elements.Attribute_Kind (E) is
                  when Asis.A_First_Attribute =>
                     return Get_Bound (Prefix_Declaration, Bound => Lower);

                  when Asis.A_Last_Attribute =>
                     return Get_Bound (Prefix_Declaration, Bound => Upper);

                  when Asis.An_Implementation_Defined_Attribute =>
                     if Asis.Expressions.Name_Image
                       (Asis.Expressions.Attribute_Designator_Identifier
                          (E)) = "Range_Length"
                     then
                        return Get_Bound (Prefix_Declaration, Bound => Upper)
                          - Get_Bound (Prefix_Declaration, Bound => Lower) + 1;
                     else
                        raise Program_Error with "Unsupported implementation defined attribute";
                     end if;
                  when others =>
                     raise Program_Error with "Unsupported Attribute";
               end case;
            end;
         when Asis.An_Identifier =>
            declare
               Ident_Decl : constant Asis.Declaration := Asis.Expressions.Corresponding_Name_Declaration (E);
            begin
               case Asis.Elements.Declaration_Kind (Ident_Decl) is
                  when Asis.A_Constant_Declaration =>
                     return Get_Static_Integer (Asis.Declarations.Initialization_Expression (Ident_Decl));
                  when Asis.An_Integer_Number_Declaration =>
                     return Get_Static_Integer (Asis.Declarations.Initialization_Expression (Ident_Decl));
                  when others =>
                     raise Program_Error with "Unsupported declaration kind";
               end case;
            end;

         when others =>
            Put_Line (Asis.Element_Kinds'Wide_Image (Asis.Elements.Element_Kind (E)));
            raise Program_Error
              with "Acton Analyser only supports Seconds, Milliseconds, etc functions that only have an integer literal as its sole parameter";
      end case;

   end Get_Static_Integer;

   function Get_Bound (D : Asis.Declaration; Bound : Bound_End) return Integer is
     Type_Def : constant Asis.Definition := Asis.Declarations.Type_Declaration_View (D);
   begin
      case Asis.Elements.Declaration_Kind (D) is
         when Asis.A_Subtype_Declaration =>
            declare
               Constraint_Range : constant Asis.Constraint := Asis.Definitions.Subtype_Constraint (Type_Def);
            begin
               case Bound is
                  when Lower =>
                     return Get_Static_Integer (Asis.Definitions.Lower_Bound (Constraint_Range));
                  when Upper =>
                     return Get_Static_Integer (Asis.Definitions.Upper_Bound (Constraint_Range));
               end case;
            end;
            --  Should cover the case where no subtype range is given.
            --  see ASIS RM 16.22

         when Asis.An_Ordinary_Type_Declaration =>
            case Asis.Elements.Definition_Kind (Type_Def) is
               when Asis.A_Type_Definition =>
                  case Asis.Elements.Type_Kind (Type_Def) is
                     when Asis.A_Modular_Type_Definition =>
                        case Bound is
                        when Lower =>
                           return 0;
                        when Upper =>
                           return Get_Static_Integer (Asis.Definitions.Mod_Static_Expression (Type_Def)) - 1;
                        end case;

                     when others =>
                        raise Program_Error with "Unsupported type defintion";
                  end case;

               when others =>
                  raise Program_Error with "Unsupported definition kind for Get_Bound";
            end case;
         when others =>
            raise Program_Error with "Unsupported type declaration for the Get_Bound procedure";
      end case;
   end Get_Bound;

   function Calculate_Static_Time_Span
     (For_Expression : Asis.Expression)
      return Ada.Real_Time.Time_Span
   is
      E : Asis.Expression;

   begin
      case Asis.Elements.Expression_Kind (For_Expression) is
         when Asis.A_Function_Call =>
            E := Asis.Extensions.Corresponding_Called_Function_Unwound (For_Expression);

            if Asis.Elements.Declaration_Kind (E) in Asis.A_Function_Declaration then
               declare
                  Function_Name    : constant Wide_String := Asis.Declarations.Defining_Name_Image (Asis.Extensions.First_Name (E));
                  Func_Params      : constant Asis.Expression_List := Asis.Expressions.Function_Call_Parameters (For_Expression);
                  First_Func_Param : constant Asis.Expression := Asis.Expressions.Actual_Parameter (Func_Params (1));
               begin
                  if Function_Name = "Nanoseconds" then
                     return Ada.Real_Time.Nanoseconds (Get_Static_Integer (First_Func_Param));

                  elsif Function_Name = "Microseconds" then
                     return Ada.Real_Time.Microseconds (Get_Static_Integer (First_Func_Param));

                  elsif Function_Name = "Milliseconds" then
                     return Ada.Real_Time.Milliseconds (Get_Static_Integer (First_Func_Param));

                  elsif Function_Name = "Seconds" then
                     return Ada.Real_Time.Seconds (Get_Static_Integer (First_Func_Param));

                  elsif Function_Name = "Minutes" then
                     return Ada.Real_Time.Minutes (Get_Static_Integer (First_Func_Param));

                  elsif Function_Name = """+""" then
                     return Calculate_Static_Time_Span (Asis.Expressions.Actual_Parameter (Func_Params (1))) +
                       Calculate_Static_Time_Span (Asis.Expressions.Actual_Parameter (Func_Params (2)));

                  else
                     raise Program_Error with "Unkown function name";
                  end if;
               end;
            else
               raise Program_Error;
            end if;

         when Asis.An_Identifier =>
            --  Assume that the identifier is of type Time_Span since any other
            --  type would have been rejected by the compiler before getting
            --  here.
            E := Asis.Declarations.Initialization_Expression
              (Asis.Expressions.Corresponding_Name_Declaration (For_Expression));

            if E in Asis.Nil_Element then
               raise Program_Error with "Aspect value must be static: Non-static identifier found!";
            end if;

            return Calculate_Static_Time_Span (E);

         when others =>
            --  Shoud not get here at all.
            Put_Line (Asis.Expression_Kinds'Wide_Image (Asis.Elements.Expression_Kind (For_Expression)));
            raise Program_Error with "Aspect expression not supported with Acton Analyser";
      end case;
   end Calculate_Static_Time_Span;

   function Format_Property_String
     (S : Wide_String; Length : Integer)
      return Wide_String is
      Result : Wide_String (1 .. Length);
   begin
      Move (S, Result);
      return Result & " : ";
   end Format_Property_String;

   function Element_Location_Image (EL : Element_Location) return Wide_String is
   begin
      if EL.Source_Name = Null_Unbounded_Wide_String then
         return No_Value_Image;
      else
         return To_Wide_String (EL.Source_Name) & (":") & Asis.Text.Line_Number'Wide_Image (EL.Line_Number);
      end if;
   end Element_Location_Image;

   function Image (J : Integer) return Wide_String is
      S : constant Wide_String := Integer'Wide_Image (J);
   begin
      return S (2 .. S'Last);
   end Image;

   function Expanded_Name (Defining_Name : Asis.Defining_Name) return Wide_String is
   begin
      return To_Wide_String (Asis.Set_Get.Unit_Name (Asis.Elements.Enclosing_Compilation_Unit (Defining_Name)))
        & "." & Asis.Declarations.Defining_Name_Image (Defining_Name);
   end Expanded_Name;

end Analyser_Support;
