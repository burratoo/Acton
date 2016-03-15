------------------------------------------------------------------------------
--                                                                          --
--                           ASIS TUTORIAL COMPONENTS                       --
--                                                                          --
--                        U N I T _ P R O C E S S I N G                     --
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

--  This is the modification of the file with the same name from the ASIS
--  Application Templates to be used in the solution of the metrics tool
--  (for Task 1 and Task 2 the same version of this file is used)

with Asis.Compilation_Units;
with Asis.Elements;

with Element_Processing;
with Tasks;

package body Unit_Processing is

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (The_Unit : Asis.Compilation_Unit) is
      Cont_Clause_Elements : constant Asis.Element_List :=
         Asis.Elements.Context_Clause_Elements (Compilation_Unit => The_Unit,
                                                Include_Pragmas  => True);
      --  This is the list of the context clauses, including pragmas, if any.
      --  If you do not want to process pragmas, set Include_Pragmas OFF when
      --  calling Asis.Elements.Context_Clause_Elements

      Unit_Decl : Asis.Element := Asis.Elements.Unit_Declaration (The_Unit);
      --  The top-level structural element of the library item or subunit
      --  contained in The_Unit.

   begin

      for J in Cont_Clause_Elements'Range loop
         Element_Processing.Process_Construct (Cont_Clause_Elements (J));
      end loop;

      --  Many applications are not interested in processing the context
      --  clause of the compilation units. If this is the case for your
      --  application, simply remove this loop statement.

      if Asis.Compilation_Units.Can_Be_Main_Program (The_Unit) then
         case Asis.Compilation_Units.Unit_Kind (The_Unit) is
            when Asis.A_Procedure_Body =>
               Tasks.Add_Task_Body_Information
                 (Name               => Asis.Compilation_Units.Unit_Full_Name (The_Unit),
                  Has_Cyclic_Section => False,
                  Is_Main_Task       => True,
                  Source             => Asis.Compilation_Units.Text_Name (The_Unit),
                  Line               => 0);
               Tasks.Add_Task_Object_Declaration
                 (Name            => Asis.Compilation_Units.Unit_Full_Name (The_Unit),
                  Unit_Name       => Asis.Compilation_Units.Unit_Full_Name (The_Unit),
                  Is_Main_Task    => True,
                  Number_Of_Tasks => 1,
                  Source          => Asis.Compilation_Units.Text_Name (The_Unit),
                  Line            => 0);
            when Asis.A_Procedure =>
               Tasks.Add_Task_Unit_Declaration
                 (Name         => Asis.Compilation_Units.Unit_Full_Name (The_Unit),
                  Is_Main_Task => True,
                  Is_Singleton => True,
                  Source       => Asis.Compilation_Units.Text_Name (The_Unit),
                  Line         => 0);
               Tasks.Add_Task_Object_Declaration
                 (Name            => Asis.Compilation_Units.Unit_Full_Name (The_Unit),
                  Unit_Name       => Asis.Compilation_Units.Unit_Full_Name (The_Unit),
                  Is_Main_Task    => True,
                  Number_Of_Tasks => 1,
                  Source          => Asis.Compilation_Units.Text_Name (The_Unit),
                  Line            => 0);
            when others =>
               null;
         end case;
      end if;

      Element_Processing.Process_Construct (Unit_Decl);

      --  This procedure does not contain any exception handler because it
      --  supposes that Element_Processing.Process_Construct should handle
      --  all the exceptions which can be raised when processing the element
      --  hierarchy

   end Process_Unit;

end Unit_Processing;
