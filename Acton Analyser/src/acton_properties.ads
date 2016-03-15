with Asis;

package Acton_Properties is

   Uninitialised_Priority     : constant Integer := -1;
   Uninitialised_Storage_Size : constant Integer := -1;

   function Default_Priority return Integer;
   function Default_Priority_Ceiling return Integer;
   function Default_Storage_Size return Integer;

   function Lower_System_Priority return Integer;
   function Upper_System_Priority return Integer;

   function Stack_Size_Main_Task return Integer;

   function Task_Limit return Integer;
   function Protected_Limit return Integer;

   function Stack_Space_Allocated_For_System_Agents return Integer;

   procedure Process_Acton_Properties (The_Context : Asis.Context);
   procedure Print_Acton_Properties;
private
   Low_Priority               : Integer;
   High_Pirority              : Integer;
   Default_Protected_Priority : Integer := Uninitialised_Priority;
   Default_Task_Priority      : Integer := Uninitialised_Priority;
   Default_Task_Storage_Size  : Integer := Uninitialised_Storage_Size;

   Interrupt_Agent_Stack_Size : Integer;
   Oak_Stack_Size             : Integer;
   Sleep_Agent_Stack_Size     : Integer;
   Main_Task_Stack_Size       : Integer;

   Number_Of_Interrupt_Agents : Integer;
   Number_Of_Sleep_Agents     : Integer;
   Number_Of_Kernel_Agents    : Integer;

   Max_Number_Of_Tasks        : Integer;
   Max_Number_Of_Protected    : Integer;
   Max_Number_Of_Schedulers   : Integer;

   function Default_Priority return Integer is (Default_Task_Priority);
   function Default_Priority_Ceiling return Integer is (Default_Protected_Priority);
   function Default_Storage_Size return Integer is (Default_Task_Storage_Size);

   function Lower_System_Priority return Integer is (Low_Priority);
   function Upper_System_Priority return Integer is (High_Pirority);

   function Stack_Size_Main_Task return Integer is (Main_Task_Stack_Size);
   function Protected_Limit return Integer is (Max_Number_Of_Protected);
   function Task_Limit return Integer is (Max_Number_Of_Tasks);

   function Stack_Space_Allocated_For_System_Agents return Integer is
     (Number_Of_Kernel_Agents * Oak_Stack_Size
      + Number_Of_Interrupt_Agents * Interrupt_Agent_Stack_Size
      + Number_Of_Sleep_Agents * Sleep_Agent_Stack_Size);
end Acton_Properties;
