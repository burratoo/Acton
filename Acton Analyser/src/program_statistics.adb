------------------------------------------------------------------------------------------
--                                                                                      --
--                                    ACTON ANALYSER                                    --
--                                                                                      --
--                                  PROGRAM_STATISTICS                                  --
--                                                                                      --
--                       Copyright (C) 2016-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Acton_Properties;  use Acton_Properties;
with Analyser_Support;  use Analyser_Support;
with Protected_Objects; use Protected_Objects;
with Tasks;             use Tasks;

with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;
with Ada.Strings.Wide_Unbounded;              use Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Unbounded.Wide_Text_IO; use Ada.Strings.Wide_Unbounded.Wide_Text_IO;

package body Program_Statistics is

   procedure Calculate_Statistics is
   begin
      Number_Of_Task_Objects := Total_Number_Of_Task_Objects;
      Number_Of_Protected_Objects := Total_Number_Of_Protected_Objects;

      Program_Stack_Size := Stack_Space_Allocated_To_Task_Objects
        +  Stack_Space_Allocated_For_System_Agents;


   end Calculate_Statistics;

   procedure Print_Statistics is
      W : constant := 25;
   begin
      Put_Line (Line_Decoration);
      New_Line;
      Put_Line ("Program Statistics");
      Put_Line (50 * "=");

      Put_Line (Format_Property_String ("Task Objects (max)", W) &
                  Image (Number_Of_Task_Objects) & " (" & Image (Task_Limit) & ")");
      Put_Line (Format_Property_String ("Protected Objects (max)", W) &
                  Image (Number_Of_Protected_Objects) & " (" & Image (Protected_Limit) & ")");
      Put_Line (Format_Property_String ("Stack Size Allocated", W) &
                  Image (Program_Stack_Size) & " bytes");

      Put_Line (50 * "=");
      New_Line;

      if Number_Of_Task_Objects > Task_Limit then
         Put_Line ("WARNING: More task units defined than supported by the current Acton project!");
         Put_Line ("RESOLUTION: Modify Oak.Project_Support_Package.Max_Task_Agents and recompile Acton.");
      end if;
      New_Line;
      Put_Line (Line_Decoration);
   end Print_Statistics;
end Program_Statistics;
