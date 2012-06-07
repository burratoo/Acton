package Oak.Core_Support_Package with Pure is

   Number_Of_Processors : constant := 1;
   type Oak_Instance_Id is range 1 .. Number_Of_Processors;

   Max_Tasks            : constant := 2 ** 16;
   Max_Task_Name_Length : constant := 80;

   Max_Protected_Entries : constant := 10;
   Max_Task_Entries      : constant := 0;
   Max_Actions           : constant := 10;
end Oak.Core_Support_Package;
