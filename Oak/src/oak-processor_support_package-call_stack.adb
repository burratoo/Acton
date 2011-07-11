package body Oak.Processor_Support_Package.Call_Stack is

   -----------------------------
   -- Default_Call_Stack_Size --
   -----------------------------

   function Default_Call_Stack_Size
      return System.Storage_Elements.Storage_Count
   is
   begin
      return Call_Stack_Size;
   end Default_Call_Stack_Size;

end Oak.Processor_Support_Package.Call_Stack;
