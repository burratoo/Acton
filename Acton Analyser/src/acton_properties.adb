------------------------------------------------------------------------------------------
--                                                                                      --
--                                    ACTON ANALYSER                                    --
--                                                                                      --
--                                   ACTON_PROPERTIES                                   --
--                                                                                      --
--                       Copyright (C) 2016-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Asis.Compilation_Units;
with Asis.Declarations;
with Asis.Definitions;
with Asis.Elements;
with Asis.Extensions;
with Asis.Set_Get;

with A4G.DDA_Aux; use A4G.DDA_Aux;
with Uintp;       use Uintp;

with Ada.Wide_Text_IO;                        use Ada.Wide_Text_IO;
with Ada.Strings.Wide_Unbounded;              use Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Unbounded.Wide_Text_IO; use Ada.Strings.Wide_Unbounded.Wide_Text_IO;

with Analyser_Support; use Analyser_Support;

package body Acton_Properties is

   procedure Process_Acton_Properties (The_Context : Asis.Context) is
   begin
      --  Process properties from Oak.Project_Support_Package
      declare
         PSP_Unit     : constant Asis.Compilation_Unit :=
                          Asis.Compilation_Units.Library_Unit_Declaration
                            (Name        => "Oak.Project_Support_Package",
                             The_Context => The_Context);
         PSP_Decl     : constant Asis.Element := Asis.Elements.Unit_Declaration (PSP_Unit);
         Declarations : constant Asis.Declarative_Item_List := Asis.Declarations.Visible_Part_Declarative_Items (PSP_Decl);
      begin
         for Decl of Declarations loop
            if Asis.Elements.Declaration_Kind (Decl) in Asis.An_Integer_Number_Declaration then
               declare
                  Decl_Name : constant Wide_String := Asis.Declarations.Defining_Name_Image (Asis.Extensions.First_Name (Decl));
                  Decl_Expr : constant Asis.Expression := Asis.Declarations.Initialization_Expression (Decl);
               begin
                  if Decl_Name = "Max_Kernel_Agents" then
                     Number_Of_Kernel_Agents := Get_Static_Integer (Decl_Expr);
                  elsif Decl_Name = "Max_Scheduler_Agents" then
                     Max_Number_Of_Schedulers := Get_Static_Integer (Decl_Expr);
                  elsif Decl_Name = "Max_Task_Agents" then
                     Max_Number_Of_Tasks := Get_Static_Integer (Decl_Expr);
                  elsif Decl_Name = "Max_Protected_Agents" then
                    Max_Number_Of_Protected := Get_Static_Integer (Decl_Expr);
                  elsif Decl_Name = "Max_Sleep_Agents" then
                     Number_Of_Sleep_Agents := Get_Static_Integer (Decl_Expr);
                  elsif Decl_Name = "Call_Stack_Size" then
                     Default_Task_Storage_Size := Get_Static_Integer (Decl_Expr);
                  elsif Decl_Name = "Main_Task_Call_Stack_Size" then
                     Main_Task_Stack_Size := Get_Static_Integer (Decl_Expr);
                  elsif Decl_Name = "Interrupt_Stack_Size" then
                     Interrupt_Agent_Stack_Size := Get_Static_Integer (Decl_Expr);
                  end if;
               end;
            end if;
         end loop;
      end;

      --  Process properties from Oak.Core_Support_Package.Call_Stack
      declare
         CS_Unit  : constant Asis.Compilation_Unit :=
                      Asis.Compilation_Units.Library_Unit_Declaration
                        (Name        => "Oak.Core_Support_Package.Call_Stack",
                         The_Context => The_Context);
         CS_Decl  : constant Asis.Element := Asis.Elements.Unit_Declaration (CS_Unit);
         Declarations : constant Asis.Declarative_Item_List := Asis.Declarations.Visible_Part_Declarative_Items (CS_Decl);
      begin
         for Decl of Declarations loop
            if Asis.Elements.Declaration_Kind (Decl) in Asis.An_Integer_Number_Declaration then
               declare
                  Decl_Name : constant Wide_String := Asis.Declarations.Defining_Name_Image (Asis.Extensions.First_Name (Decl));
                  Decl_Expr : constant Asis.Expression := Asis.Declarations.Initialization_Expression (Decl);
               begin
                  if Decl_Name = "Oak_Call_Stack_Size" then
                     Oak_Stack_Size := Get_Static_Integer (Decl_Expr);
                  elsif Decl_Name = "Sleep_Stack_Size" then
                     Sleep_Agent_Stack_Size := Get_Static_Integer (Decl_Expr);
                  end if;
               end;
            end if;
         end loop;
      end;

      --  Process properties from System
      declare
         System_Unit  : constant Asis.Compilation_Unit :=
                          Asis.Compilation_Units.Library_Unit_Declaration
                            (Name        => "System",
                             The_Context => The_Context);
         System_Decl  : constant Asis.Element := Asis.Elements.Unit_Declaration (System_Unit);
         Declarations : constant Asis.Declarative_Item_List := Asis.Declarations.Visible_Part_Declarative_Items (System_Decl);
      begin
         --  Looking for Default Priority and Interrupt Priority range
         for Decl of Declarations loop
            case Asis.Elements.Declaration_Kind (Decl) is
               when Asis.A_Constant_Declaration =>
                  declare
                     Decl_Name : constant Wide_String := Asis.Declarations.Defining_Name_Image (Asis.Extensions.First_Name (Decl));
                     Decl_Expr : constant Asis.Expression := Asis.Declarations.Initialization_Expression (Decl);
                  begin
                     if Decl_Name = "Default_Priority" then
                        Default_Task_Priority := Get_Static_Integer (Decl_Expr);
                     end if;
                  end;

               when Asis.A_Subtype_Declaration =>
                  declare
                     Decl_Name   : constant Wide_String := Asis.Declarations.Defining_Name_Image (Asis.Extensions.First_Name (Decl));
                     Lower_Range : constant Integer := Get_Bound (Decl, Lower);
                     Upper_Range : constant Integer := Get_Bound (Decl, Upper);
                  begin
                     if Decl_Name = "Interrupt_Priority" then
                        Number_Of_Interrupt_Agents := Upper_Range - Lower_Range + 1;
                        Default_Protected_Priority := Upper_Range;
                     end if;

                     if Decl_Name = "Any_Priority" then
                        Low_Priority := Lower_Range;
                        High_Pirority := Upper_Range;
                     end if;
                  end;

               when others =>
                  null;
            end case;
         end loop;
      end;
   end Process_Acton_Properties;

   procedure Print_Acton_Properties is
      W : constant := 31;
   begin
      Put_Line (50 * "🌳 ");
      New_Line;
      Put_Line ("ACTON PROPERTIES");
      Put_Line (40 * "=");
      Put_Line (Format_Property_String ("Any_Priority Range", W) & Integer'Wide_Image (Low_Priority) &  " .. " & Integer'Wide_Image (High_Pirority));
      Put_Line (Format_Property_String ("Default Task Priority", W) & Integer'Wide_Image (Default_Task_Priority));

      New_Line;
      Put_Line (Format_Property_String ("Default Storage Size", W) & Integer'Wide_Image (Default_Task_Storage_Size));
      Put_Line (Format_Property_String ("Interrupt Agent Stack Size", W) & Integer'Wide_Image (Interrupt_Agent_Stack_Size));
      Put_Line (Format_Property_String ("Kernel Stack Size", W) & Integer'Wide_Image (Oak_Stack_Size));
      Put_Line (Format_Property_String ("Sleep Agent Stack Size", W) & Integer'Wide_Image (Sleep_Agent_Stack_Size));
      Put_Line (Format_Property_String ("Main Task Stack Size", W) & Integer'Wide_Image (Main_Task_Stack_Size));

      New_Line;
      Put_Line (Format_Property_String ("Number of Interrupt Agents", W) & Integer'Wide_Image (Number_Of_Interrupt_Agents));
      Put_Line (Format_Property_String ("Number of Sleep Agents", W) & Integer'Wide_Image (Number_Of_Sleep_Agents));
      Put_Line (Format_Property_String ("Number of Kernel Agents", W) & Integer'Wide_Image (Number_Of_Kernel_Agents));

      New_Line;
      Put_Line (Format_Property_String ("Max Number of Tasks", W) & Integer'Wide_Image (Max_Number_Of_Tasks));
      Put_Line (Format_Property_String ("Max Number of Protected Objects", W) & Integer'Wide_Image (Max_Number_Of_Protected));
      Put_Line (Format_Property_String ("Max Number of Schedulers", W) & Integer'Wide_Image (Max_Number_Of_Schedulers));
      Put_Line (40 * "=");
      New_Line;
   end Print_Acton_Properties;

end Acton_Properties;
