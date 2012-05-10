with Oak.Core_Support_Package;

with Oak.Memory.Call_Stack;         use Oak.Memory.Call_Stack;

package Oak.Agent with Pure is

   type Task_Id is range 0 .. Oak.Core_Support_Package.Max_Tasks;
   subtype Task_Name is String
     (1 .. Core_Support_Package.Max_Task_Name_Length);

--     type Memory_Region;
--     type Memory_Region_Link is access all Memory_Region;
--     type Memory_Permission is (Read_Only, Read_Write);
--
--     type Memory_Region is record
--        Location : Memory_Slice;
--        Next     : Memory_Region_Link;
--        Previous : Memory_Region_Link;
--     end record;

   type Oak_Agent is abstract tagged private;

   function Id   (Agent : Oak_Agent'Class) return Task_Id;
   function Name (Agent : Oak_Agent'Class) return Task_Name;

private

   type Oak_Agent is abstract tagged record
      Id          : Task_Id := Task_Id'Last;
      Name        : Task_Name;
      Name_Length : Natural := 0;

      ----
      --  This gives us a pointer to the starting location of the Stack (is
      --  this useful?) and the size of the stack.
      --  We also need to store the stack pointer. Its is probably useful to
      --  store the stack pointer in its own variable in the OTCR. But why?
      --  usually it is stored in a register anyway.
      -----
      Call_Stack : Call_Stack_Handler;

      --  Memory_List : Memory_Region_Link := null;
   end record;

   procedure Initialise_Agent
     (Agent      : in out Oak_Agent'Class;
      Name       : in String;
      Call_Stack : in Call_Stack_Handler);

   function Id (Agent : Oak_Agent'Class) return Task_Id is
     (Agent.Id);

   function Name (Agent : Oak_Agent'Class) return Task_Name is
     (Agent.Name);

end Oak.Agent;
