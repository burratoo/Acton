--  with Ada.Real_Time; use Ada.Real_Time;

package Oak.Processor_Support_Package is
   pragma Pure;

   Number_Of_Processors : constant := 2;
   type Oak_Instance_Id is range 1 .. Number_Of_Processors;

   Max_Tasks            : constant := 2 ** 16;
   Max_Task_Name_Length : constant := 80;

end Oak.Processor_Support_Package;
