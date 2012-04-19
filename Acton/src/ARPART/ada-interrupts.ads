with System;
with Oak.Processor_Support_Package.Interrupts;
--  with System.Multiprocessors;

package Ada.Interrupts is

   pragma Preelaborate;

   type Interrupt_Id is new
     Oak.Processor_Support_Package.Interrupts.Oak_Interrupt_Id;
   type Parameterless_Handler is
     access protected procedure;

   function Is_Reserved (Interrupt : Interrupt_Id)
      return Boolean;

   function Is_Attached (Interrupt : Interrupt_Id)
      return Boolean;

   function Current_Handler (Interrupt : Interrupt_Id)
      return Parameterless_Handler;

   procedure Attach_Handler
      (New_Handler : in Parameterless_Handler;
       Interrupt   : in Interrupt_Id);

   procedure Exchange_Handler
      (Old_Handler : out Parameterless_Handler;
       New_Handler : in Parameterless_Handler;
       Interrupt   : in Interrupt_Id);

   procedure Detach_Handler
      (Interrupt : in Interrupt_Id);

   function Reference (Interrupt : Interrupt_Id)
      return System.Address;

   --  function Get_CPU (Interrupt : Interrupt_Id)
   --     return System.Multiprocessors.CPU_Range;

private
   --  not specified by the language
end Ada.Interrupts;
