------------------------------------------------------------------------------
--                                                                          --
--                       ASIS TUTORIAL COMPONENTS                           --
--                                                                          --
--           A C T U A L S _ F O R _ T R A V E R S I N G . P R E _ O P      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (c) 2000, Free Software Foundation, Inc.            --
--                                                                          --
-- ASIS  Application  Templates are  free software; you can redistribute it --
-- and/or  modify it under  terms  of the  GNU  General  Public  License as --
-- published by the Free Software Foundation; either version 2, or (at your --
-- option) any later version. ASIS Application Templates are distributed in --
-- the hope that they will be useful, but  WITHOUT  ANY  WARRANTY; without  --
-- even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR --
-- PURPOSE. See the GNU General Public License for more details. You should --
-- have  received a copy of the GNU General Public License distributed with --
-- distributed  with  GNAT;  see  file  COPYING. If not, write to the Free  --
-- Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, --
-- USA.                                                                     --
--                                                                          --
-- ASIS Tutorial was developed and are now maintained by Ada Core           --
-- Technologies Inc (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;
with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Ada.Exceptions;

with Asis.Compilation_Units;
with Asis.Declarations;
with Asis.Definitions;
with Asis.Exceptions;
with Asis.Expressions;
with Asis.Extensions;
with Asis.Errors;
with Asis.Implementation;
with Asis.Elements;
with Asis.Declarations;
with Asis.Text;
with Asis.Set_Get;

with Acton_Properties;  use Acton_Properties;
with Schedulers;        use Schedulers;
with Tasks;             use Tasks;
with Protected_Objects; use Protected_Objects;
with Analyser_Support;  use Analyser_Support;

with Atree;  use Atree;
with Nlists; use Nlists;
with Namet;  use Namet;
with Sinfo;  use Sinfo;
with Snames; use Snames;
with Uintp;  use Uintp;
with Types;  use Types;
with A4G.DDA_Aux; use A4G.DDA_Aux;
with A4G.GNAT_Int;

with GNAT.OS_Lib;   use GNAT.OS_Lib;

use Asis;

with Lib;

separate (Actuals_For_Traversing)
procedure Pre_Op
  (Element :        Asis.Element;
   Control : in out Asis.Traverse_Control;
   State   : in out Traversal_State)
is
   Argument_Compilation_Unit : Asis.Compilation_Unit;
   Argument_Declaration_Kind : Asis.Declaration_Kinds;
   Argument_Kind             : Asis.Element_Kinds;

   procedure Record_Protected_Unit
     (Is_Body      : Boolean;
      Is_Singleton : Boolean);

   procedure Record_Task_Unit
     (Is_Body            : Boolean;
      Is_Singleton       : Boolean;
      Has_Cyclic_Section : Boolean);

   function Storage_Size_For_Scheduler_Agent
     (Scheduler_Name : Wide_String) return Integer;

   procedure Record_Protected_Unit (Is_Body      : Boolean;
                                    Is_Singleton : Boolean)
   is
      Protected_Identifier : constant Wide_String :=
                          Expanded_Name (Asis.Extensions.First_Name (Element));
      Protected_Aspects    : constant Asis.Element_List :=
                          Asis.Declarations.Aspect_Specifications (Element);

   begin
      case Is_Body is
         when True =>
            Add_Protected_Body_Information
              (Name   => Protected_Identifier,
               Source => Asis.Compilation_Units.Text_Name (Argument_Compilation_Unit),
               Line   => Asis.Text.First_Line_Number (Element));
         when False =>
            Add_Protected_Unit_Declaration
              (Name         => Protected_Identifier,
               Is_Singleton => Is_Singleton,
               Source       => Asis.Compilation_Units.Text_Name (Argument_Compilation_Unit),
               Line         => Asis.Text.First_Line_Number (Element));

            if Is_Singleton then
               Add_Protected_Object_Declaration
                 (Name              => Protected_Identifier,
                  Unit_Name         => Protected_Identifier,
                  Number_Of_Objects => 1,
                  Source            => Asis.Compilation_Units.Text_Name (Argument_Compilation_Unit),
                  Line              => Asis.Text.First_Line_Number (Element));
            end if;
      end case;

      for Aspect of Protected_Aspects loop
         declare
            The_Aspect : Supported_Aspects;
            Aspect_Def : constant Asis.Element := (Asis.Definitions.Aspect_Definition (Aspect));
            Def_Node   : constant Node_Id := Asis.Set_Get.Node (Aspect_Def);
         begin
            The_Aspect := Supported_Aspects'Wide_Value (Asis.Expressions.Name_Image (Asis.Definitions.Aspect_Mark (Aspect)));
            --  Assignment of A put here so it can be caught by the exception
            --  handler below.

            case The_Aspect is
               when Priority =>
                  Tasks.Add_Priority
                    (Name     => Protected_Identifier,
                     Priority => Integer (UI_To_Int (Eval_Scalar_Node (Def_Node))));

               when others =>
                  null;
            end case;
         exception
            when Constraint_Error =>
               null;
         end;
      end loop;
   end Record_Protected_Unit;

   procedure Record_Task_Unit (Is_Body            : Boolean;
                               Is_Singleton       : Boolean;
                               Has_Cyclic_Section : Boolean)
   is
      Task_Identifier : constant Wide_String :=
                          Expanded_Name (Asis.Extensions.First_Name (Element));
      Task_Aspects    : constant Asis.Element_List :=
                          Asis.Declarations.Aspect_Specifications (Element);

   begin
      case Is_Body is
         when True =>
            Add_Task_Body_Information
              (Name               => Task_Identifier,
               Has_Cyclic_Section => Has_Cyclic_Section,
               Is_Main_Task       => False,
               Source             => Asis.Compilation_Units.Text_Name (Argument_Compilation_Unit),
               Line               => Asis.Text.First_Line_Number (Element));
         when False =>
            Add_Task_Unit_Declaration
              (Name         => Task_Identifier,
               Is_Main_Task => False,
               Is_Singleton => Is_Singleton,
               Source       => Asis.Compilation_Units.Text_Name (Argument_Compilation_Unit),
               Line         => Asis.Text.First_Line_Number (Element));

            if Is_Singleton then
               Add_Task_Object_Declaration
                 (Name            => Task_Identifier,
                  Unit_Name       => Task_Identifier,
                  Is_Main_Task    => False,
                  Number_Of_Tasks => 1,
                  Source          => Asis.Compilation_Units.Text_Name (Argument_Compilation_Unit),
                  Line            => Asis.Text.First_Line_Number (Element));
            end if;
      end case;

      for Aspect of Task_Aspects loop
         declare
            The_Aspect : Supported_Aspects;
            Aspect_Def : constant Asis.Element := (Asis.Definitions.Aspect_Definition (Aspect));
            Def_Node   : constant Node_Id := Asis.Set_Get.Node (Aspect_Def);
         begin
            The_Aspect := Supported_Aspects'Wide_Value (Asis.Expressions.Name_Image (Asis.Definitions.Aspect_Mark (Aspect)));
            --  Assignment of A put here so it can be caught by the exception
            --  handler below.

            case The_Aspect is
               when Priority =>
                  Tasks.Add_Priority
                    (Name     => Task_Identifier,
                     Priority => Integer (UI_To_Int (Eval_Scalar_Node (Def_Node))));

               when Cycle_Behaviour =>
                  --  Cycle_Behaviour aspect defninition will be
                  --  An_Enumeration_Literal expression so can do a direct
                  --  conversion.

                  Add_Cycle_Behaviour
                    (Name      => Task_Identifier,
                     Behaviour =>
                        Cyclic_Kind'Wide_Value (Asis.Expressions.Name_Image (Aspect_Def)));

               when Cycle_Period =>
                  Add_Cycle_Period_Span
                    (Name   => Task_Identifier,
                     Period => Calculate_Static_Time_Span (Aspect_Def));

               when Cycle_Phase =>
                  Add_Cycle_Phase_Span
                    (Name  => Task_Identifier,
                     Phase => Calculate_Static_Time_Span (Aspect_Def));

               when Relative_Deadline =>
                  Add_Relative_Deadline
                    (Name     => Task_Identifier,
                     Deadline => Calculate_Static_Time_Span (Aspect_Def));

               when Execution_Budget =>
                  Add_Exceution_Deadline
                    (Name     => Task_Identifier,
                     Deadline => Calculate_Static_Time_Span (Aspect_Def));

            end case;
         exception
            when Constraint_Error =>
               null;
         end;
      end loop;

      --  While looping through the aspect list works for most aspects, it
      --  doesn't work for the Storage_Size aspect since it is converted into
      --  the Storage_Size pramga and there is no link from the aspect to the
      --  pragma. Since after this conversion the aspect is not analyzed, we
      --  cannot do the trick of pulling out the static value of the storage
      --  size aspect since it has never been analyzed and been marked as
      --  static. Thus we look for the analyzed pragma instead. Note since this
      --  is a generated pragma we need to play with GNAT's AST to find it.

      if not Is_Body then
         declare
            N      : constant Node_Id := Task_Definition (Asis.Set_Get.R_Node (Element));
            N_Decl : constant List_Id := Visible_Declarations (N);
            A_Decl : Node_Id := First (N_Decl);
            --  Get the N_Task_Definition node out of the A_Task_Declaration or
            --  A_Single_Task_Definition node
         begin
            while Present (A_Decl) loop
               if Nkind (A_Decl) = N_Pragma
                 and then Pragma_Name (A_Decl) = Name_Storage_Size
               then
                  Add_Storage_Size
                    (Name => Task_Identifier,
                     Size =>
                       Integer
                         (UI_To_Int
                              (Eval_Scalar_Node
                                 (Sinfo.Expression
                                    (First (Pragma_Argument_Associations (A_Decl)))))));
               end if;

               Next (A_Decl);

            end loop;
         end;
      end if;
   end Record_Task_Unit;

   function Storage_Size_For_Scheduler_Agent
     (Scheduler_Name : Wide_String) return Integer
   is
   --  Find the run-time directory by pulling the path for "Oak.ads"
      My_Context           : constant Asis.Context := Asis.Compilation_Units.Enclosing_Context (Asis.Elements.Enclosing_Compilation_Unit (Element));
      System_Unit          : constant Asis.Compilation_Unit :=
                               Asis.Compilation_Units.Library_Unit_Declaration
                                 (Name        => "Oak",
                                  The_Context => My_Context);
      Oak_Path             : constant Wide_String := Asis.Compilation_Units.Text_Name (System_Unit);
      Run_Time_Path        : constant Wide_String := Oak_Path (1 .. Oak_Path'Last - 7);
      Scheduler_Agent_Path : String_Access := new String'(To_String (Run_Time_Path & "acton-scheduler_agents-" & Scheduler_Name & ".ads"));

      Have_Tree            : Boolean;
   begin
      Asis.Extensions.Compile
        (Source_File => Scheduler_Agent_Path,
         Args        => A4G.GNAT_Int.Nul_Argument_List,
         Success     => Have_Tree,
         GCC         => Supported_GCC); -- Alternatively use the default options and specify the compiler using the --GCC flag on the command line

      Free (Scheduler_Agent_Path);

      if not Have_Tree then
         return 0;
      end if;

      declare
         Scheduler_Unit : constant Asis.Compilation_Unit :=
                            Asis.Compilation_Units.Library_Unit_Declaration
                              (Name        => "Acton.Scheduler_Agents." & Scheduler_Name,
                               The_Context => My_Context);
         Declarations   : constant Asis.Declarative_Item_List :=
                            Asis.Declarations.Private_Part_Declarative_Items
                              (Asis.Elements.Unit_Declaration (Scheduler_Unit));
      begin
         for Decl of Declarations loop
            if Asis.Elements.Declaration_Kind (Decl) = Asis.An_Integer_Number_Declaration then
               declare
                  Decl_Name : constant Wide_String := Asis.Declarations.Defining_Name_Image (Asis.Extensions.First_Name (Decl));
                  Decl_Expr : constant Asis.Expression := Asis.Declarations.Initialization_Expression (Decl);
               begin
                  if Decl_Name = "Stack_Size" then
                     return Get_Static_Integer (Decl_Expr);
                  end if;
               end;
            end if;
         end loop;
         return 0;
      end ;
   end Storage_Size_For_Scheduler_Agent;

begin
   Argument_Kind := Asis.Elements.Element_Kind (Element);
   Argument_Compilation_Unit := Asis.Elements.Enclosing_Compilation_Unit (Element);

   case Argument_Kind is

      when Asis.A_Declaration =>

         Argument_Declaration_Kind := Asis.Elements.Declaration_Kind (Element);

         case Argument_Declaration_Kind is

            --  Task unit and body declarations
            when Asis.A_Single_Task_Declaration =>
               Record_Task_Unit (Is_Body            => False,
                                 Is_Singleton       => True,
                                 Has_Cyclic_Section => False);

            when Asis.A_Task_Type_Declaration =>
               Record_Task_Unit (Is_Body            => False,
                                 Is_Singleton       => False,
                                 Has_Cyclic_Section => False);
            when Asis.A_Task_Body_Declaration =>
               if Asis.Declarations.Cyclic_Statements (Element) = Asis.Nil_Element_List then
                  Record_Task_Unit (Is_Body            => True,
                                    Is_Singleton       => False,
                                    Has_Cyclic_Section => False);
               else
                  Record_Task_Unit (Is_Body            => True,
                                    Is_Singleton       => False,
                                    Has_Cyclic_Section => True);
               end if;

               --  Protected unit and body declarations

            when Asis.A_Single_Protected_Declaration =>
               Record_Protected_Unit (Is_Body      => False,
                                      Is_Singleton => True);

            when Asis.A_Protected_Type_Declaration =>
               Record_Protected_Unit (Is_Body      => False,
                                      Is_Singleton => False);
            when Asis.A_Protected_Body_Declaration =>
               Record_Protected_Unit (Is_Body      => True,
                                      Is_Singleton => False);

               --  Object declarations
            when A_Variable_Declaration =>
               declare
                  Variable_Names  : constant Asis.Defining_Name_List := Asis.Declarations.Names (Element);
                  Object_Def      : constant Asis.Definition := Asis.Declarations.Object_Declaration_View (Element);
                  Type_Decl       : Asis.Declaration := Nil_Element;
                  Number_Of_Objects : Integer := 1;

                  function Process_Subtype_Identifier (Subtype_Identifer : Asis.Expression) return Asis.Declaration;

                  function Process_Subtype_Identifier (Subtype_Identifer : Asis.Expression) return Asis.Declaration is
                  begin
                     case Asis.Elements.Expression_Kind (Subtype_Identifer) is
                        when An_Identifier =>
                           return Asis.Expressions.Corresponding_Name_Declaration (Subtype_Identifer);
                        when others =>
                           return Nil_Element;
                     end case;
                  end Process_Subtype_Identifier;

               begin
                  case Asis.Elements.Definition_Kind (Object_Def) is
                     when Asis.A_Subtype_Indication =>
                        Type_Decl := Process_Subtype_Identifier (Asis.Definitions.Subtype_Mark (Object_Def));

                     when Asis.A_Type_Definition =>
                        case Asis.Elements.Type_Kind (Object_Def) is
                           when Asis.A_Constrained_Array_Definition =>
                              Type_Decl := Process_Subtype_Identifier
                                (Asis.Definitions.Subtype_Mark
                                   (Asis.Definitions.Component_Definition_View
                                        (Asis.Definitions.Array_Component_Definition (Object_Def))));
                              declare
                                 Array_Range : Asis.Definition := Asis.Definitions.Discrete_Subtype_Definitions (Object_Def) (1);
                              begin
                                 if Asis.Elements.Discrete_Range_Kind (Array_Range) = Asis.A_Discrete_Simple_Expression_Range then
                                    Number_Of_Objects := Get_Static_Integer (Asis.Definitions.Upper_Bound (Array_Range)) - Get_Static_Integer (Asis.Definitions.Lower_Bound (Array_Range)) + 1;
                                 end if;
                              end ;
                           when others =>
                              null;
                        end case;

                     when others =>
                        null;
                  end case;

                  --  Resolve private view

                  if Asis.Elements.Declaration_Kind (Type_Decl) = Asis.A_Private_Type_Declaration then
                     Type_Decl := Asis.Declarations.Corresponding_Type_Declaration (Type_Decl);
                  end if;

                  if Asis.Elements.Declaration_Kind (Type_Decl) = Asis.A_Task_Type_Declaration then
                     for Object_Name of Variable_Names loop
                        Add_Task_Object_Declaration
                          (Name            => Expanded_Name (Object_Name),
                           Unit_Name       => Expanded_Name (Asis.Extensions.First_Name (Type_Decl)),
                           Is_Main_Task    => False,
                           Number_Of_Tasks => Number_Of_Objects,
                           Source          => Asis.Compilation_Units.Text_Name (Argument_Compilation_Unit),
                           Line            => Asis.Text.First_Line_Number (Element));
                     end loop;

                  elsif Asis.Elements.Declaration_Kind (Type_Decl) = Asis.A_Protected_Type_Declaration then
                     for Object_Name of Variable_Names loop
                        Add_Protected_Object_Declaration
                          (Name              => Expanded_Name (Object_Name),
                           Unit_Name         => Expanded_Name (Asis.Extensions.First_Name (Type_Decl)),
                           Number_Of_Objects => Number_Of_Objects,
                           Source            => Asis.Compilation_Units.Text_Name (Argument_Compilation_Unit),
                           Line              => Asis.Text.First_Line_Number (Element));
                     end loop;
                  end if;
               end;

            when others =>
               null;
         end case;

      when A_Pragma =>
         case Asis.Elements.Pragma_Kind (Element) is
            when A_Task_Dispatching_Policy_Pragma =>
               declare
                  Scheduler_Name : Wide_String :=
                                     Asis.Expressions.Name_Image
                                       (Asis.Expressions.Actual_Parameter
                                          (Asis.Elements.Pragma_Argument_Associations
                                             (Element) (1)));
               begin
                  Add_Scheduler (Name          => Scheduler_Name,
                                 Low_Priority  => Lower_System_Priority,
                                 High_Priority => Upper_System_Priority,
                                 Storage_Size  => Storage_Size_For_Scheduler_Agent (Scheduler_Name),
                                 Source        => Asis.Compilation_Units.Text_Name (Argument_Compilation_Unit),
                                 Line          => Asis.Text.First_Line_Number (Element));

               end;
            when others =>
               null;
         end case;

      when others =>
         null;
   end case;
exception

   when Ex : Asis.Exceptions.ASIS_Inappropriate_Context          |
        Asis.Exceptions.ASIS_Inappropriate_Container        |
        Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit |
        Asis.Exceptions.ASIS_Inappropriate_Element          |
        Asis.Exceptions.ASIS_Inappropriate_Line             |
        Asis.Exceptions.ASIS_Inappropriate_Line_Number      |
        Asis.Exceptions.ASIS_Failed                         =>

      Ada.Wide_Text_IO.Put ("Pre_Op : ASIS exception (");

      Ada.Wide_Text_IO.Put (Ada.Characters.Handling.To_Wide_String (
                            Ada.Exceptions.Exception_Name (Ex)));

      Ada.Wide_Text_IO.Put (") is raised");
      Ada.Wide_Text_IO.New_Line;

      Ada.Wide_Text_IO.Put ("ASIS Error Status is ");

      Ada.Wide_Text_IO.Put
        (Asis.Errors.Error_Kinds'Wide_Image (Asis.Implementation.Status));

      Ada.Wide_Text_IO.New_Line;

      Ada.Wide_Text_IO.Put ("ASIS Diagnosis is ");
      Ada.Wide_Text_IO.New_Line;
      Ada.Wide_Text_IO.Put (Asis.Implementation.Diagnosis);
      Ada.Wide_Text_IO.New_Line;

      Asis.Implementation.Set_Status;

   when Ex : others =>

      Ada.Wide_Text_IO.Put ("Pre_Op : ");

      Ada.Wide_Text_IO.Put (Ada.Characters.Handling.To_Wide_String (
                            Ada.Exceptions.Exception_Name (Ex)));

      Ada.Wide_Text_IO.Put (" is raised (");

      Ada.Wide_Text_IO.Put (Ada.Characters.Handling.To_Wide_String (
                            Ada.Exceptions.Exception_Information (Ex)));

      Ada.Wide_Text_IO.Put (")");
      Ada.Wide_Text_IO.New_Line;

end Pre_Op;
