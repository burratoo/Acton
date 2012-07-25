package body Ada.Interrupts is

   function Is_Reserved (Interrupt : Interrupt_Id)
                         return Boolean is
      pragma Unreferenced (Interrupt);
   begin
      return False;
   end Is_Reserved;

   function Is_Attached (Interrupt : Interrupt_Id)
                         return Boolean is
      pragma Unreferenced (Interrupt);
   begin
      return False;
   end Is_Attached;

   function Current_Handler (Interrupt : Interrupt_Id)
                             return Parameterless_Handler is
      pragma Unreferenced (Interrupt);
   begin
      return null;
   end Current_Handler;

   procedure Attach_Handler
      (New_Handler : in Parameterless_Handler;
       Interrupt   : in Interrupt_Id) is
   begin
      raise Program_Error;
   end Attach_Handler;

   procedure Exchange_Handler
      (Old_Handler : out Parameterless_Handler;
       New_Handler : in Parameterless_Handler;
       Interrupt   : in Interrupt_Id) is
   begin
      raise Program_Error;
   end Exchange_Handler;

   procedure Detach_Handler
     (Interrupt : in Interrupt_Id) is
   begin
      raise Program_Error;
   end Detach_Handler;

   function Reference (Interrupt : Interrupt_Id)
                       return System.Address is
   begin
      raise Program_Error;
      return System.Null_Address;
   end Reference;

   --  function Get_CPU (Interrupt : Interrupt_Id)
   --     return System.Multiprocessors.CPU_Range;

end Ada.Interrupts;
