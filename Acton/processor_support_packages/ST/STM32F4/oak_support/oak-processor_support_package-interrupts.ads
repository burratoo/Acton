with System; use System;

with Oak.Brokers; use Oak.Brokers;

with Oak.Core_Support_Package.Call_Stack;
with Oak.Core_Support_Package.Clock.Interrupt;
with Oak.Core_Support_Package.Interrupts;
use Oak.Core_Support_Package.Interrupts;

with ISA.ARM.Cortex_M4.Exceptions; use ISA.ARM.Cortex_M4.Exceptions;

with ST.STM32F4.NVIC; use ST.STM32F4.NVIC;

package Oak.Processor_Support_Package.Interrupts with Preelaborate is

   -----------------------
   -- INTERFACE FOR OAK --
   -----------------------

   subtype External_Interrupt_Id is ST.STM32F4.NVIC.ST32F4_Interrupt_Id;
   Default_Interrupt_Priority : constant Interrupt_Priority :=
                                  Interrupt_Priority'Last;

   type Parameterless_Handler is access protected procedure;

   procedure External_Interrupt_Handler (Interrupt_Id : External_Interrupt_Id);
   procedure Initialise_Interrupts;
   procedure Complete_Interrupt_Initialisation;

   procedure Attach_Handler (Interrupt : External_Interrupt_Id;
                             Handler   : Parameterless_Handler;
                             Priority  : Interrupt_Priority);

   function Handler_Protected_Object
     (Interrupt : External_Interrupt_Id) return Protected_Id_With_No;

   function Current_Interrupt_Priority return Any_Priority;
   function Get_External_Interrupt_Id return External_Interrupt_Id;

   procedure Set_Hardware_Priority (P : Any_Priority);
   procedure Clear_Hardware_Priority;

   function Has_Outstanding_Interrupts (Above_Priority : Any_Priority)
                                        return Boolean;

   ---------------------------------
   -- INTERNAL PACKAGE COMPONENTS --
   ---------------------------------

   Oak_Mask_Priority : constant := 16;
   --  The mask priority of Oak. Corresponds to an exception priority level of
   --  1 (since the unused bits of the priority byte lie on the least
   --  significant end)

   function To_Cortex_Priority (Priority : Interrupt_Priority)
                                return Exception_Priority
     with Pure_Function;

   function To_Ada_Priority (Priority : Exception_Priority)
                             return Interrupt_Priority;
   --  The priority conversion functions are here since the conversion depends
   --  on the number of priority bits implemented by the processor.

   procedure Start_Procedure
     with Import, Convention => Assembler, External_Name => "_start";

   procedure Trap_Handler;

   Exception_Vector_Table : array
     (External_Interrupt_Id'First .. External_Interrupt_Id'Last + 15) of
     Address :=
       (Stack_Address =>
            Oak.Core_Support_Package.Call_Stack.Stack_Pointer_Init'Address,
        Reset         => Start_Procedure'Address,
        NMI           => Trap_Handler'Address,
        Hard_Fault    => Trap_Handler'Address,
        Mem_Manage    => Trap_Handler'Address,
        Bus_Fault     => Trap_Handler'Address,
        Usage_Fault   => Trap_Handler'Address,
        Reserved2     => Trap_Handler'Address,
        Reserved3     => Trap_Handler'Address,
        Reserved4     => Trap_Handler'Address,
        Reserved5     => Trap_Handler'Address,
        SVCall        => SVCall_Handler'Address,
        Reserved6     => Trap_Handler'Address,
        Reserved7     => Trap_Handler'Address,
        PendSV        => Trap_Handler'Address,
        SysTick       =>
          Oak.Core_Support_Package.Clock.Interrupt.Update_Clock'Address,
        IRQ0          => IRQ_Interrupt_Handler'Address,
        IRQ1          => IRQ_Interrupt_Handler'Address,
        IRQ2          => IRQ_Interrupt_Handler'Address,
        IRQ3          => IRQ_Interrupt_Handler'Address,
        IRQ4          => IRQ_Interrupt_Handler'Address,
        IRQ5          => IRQ_Interrupt_Handler'Address,
        IRQ6          => IRQ_Interrupt_Handler'Address,
        IRQ7          => IRQ_Interrupt_Handler'Address,
        IRQ8          => IRQ_Interrupt_Handler'Address,
        IRQ9          => IRQ_Interrupt_Handler'Address,
        IRQ10         => IRQ_Interrupt_Handler'Address,
        IRQ11         => IRQ_Interrupt_Handler'Address,
        IRQ12         => IRQ_Interrupt_Handler'Address,
        IRQ13         => IRQ_Interrupt_Handler'Address,
        IRQ14         => IRQ_Interrupt_Handler'Address,
        IRQ15         => IRQ_Interrupt_Handler'Address,
        IRQ16         => IRQ_Interrupt_Handler'Address,
        IRQ17         => IRQ_Interrupt_Handler'Address,
        IRQ18         => IRQ_Interrupt_Handler'Address,
        IRQ19         => IRQ_Interrupt_Handler'Address,
        IRQ20         => IRQ_Interrupt_Handler'Address,
        IRQ21         => IRQ_Interrupt_Handler'Address,
        IRQ22         => IRQ_Interrupt_Handler'Address,
        IRQ23         => IRQ_Interrupt_Handler'Address,
        IRQ24         => IRQ_Interrupt_Handler'Address,
        IRQ25         => IRQ_Interrupt_Handler'Address,
        IRQ26         => IRQ_Interrupt_Handler'Address,
        IRQ27         => IRQ_Interrupt_Handler'Address,
        IRQ28         => IRQ_Interrupt_Handler'Address,
        IRQ29         => IRQ_Interrupt_Handler'Address,
        IRQ30         => IRQ_Interrupt_Handler'Address,
        IRQ31         => IRQ_Interrupt_Handler'Address,
        IRQ32         => IRQ_Interrupt_Handler'Address,
        IRQ33         => IRQ_Interrupt_Handler'Address,
        IRQ34         => IRQ_Interrupt_Handler'Address,
        IRQ35         => IRQ_Interrupt_Handler'Address,
        IRQ36         => IRQ_Interrupt_Handler'Address,
        IRQ37         => IRQ_Interrupt_Handler'Address,
        IRQ38         => IRQ_Interrupt_Handler'Address,
        IRQ39         => IRQ_Interrupt_Handler'Address,
        IRQ40         => IRQ_Interrupt_Handler'Address,
        IRQ41         => IRQ_Interrupt_Handler'Address,
        IRQ42         => IRQ_Interrupt_Handler'Address,
        IRQ43         => IRQ_Interrupt_Handler'Address,
        IRQ44         => IRQ_Interrupt_Handler'Address,
        IRQ45         => IRQ_Interrupt_Handler'Address,
        IRQ46         => IRQ_Interrupt_Handler'Address,
        IRQ47         => IRQ_Interrupt_Handler'Address,
        IRQ48         => IRQ_Interrupt_Handler'Address,
        IRQ49         => IRQ_Interrupt_Handler'Address,
        IRQ50         => IRQ_Interrupt_Handler'Address,
        IRQ51         => IRQ_Interrupt_Handler'Address,
        IRQ52         => IRQ_Interrupt_Handler'Address,
        IRQ53         => IRQ_Interrupt_Handler'Address,
        IRQ54         => IRQ_Interrupt_Handler'Address,
        IRQ55         => IRQ_Interrupt_Handler'Address,
        IRQ56         => IRQ_Interrupt_Handler'Address,
        IRQ57         => IRQ_Interrupt_Handler'Address,
        IRQ58         => IRQ_Interrupt_Handler'Address,
        IRQ59         => IRQ_Interrupt_Handler'Address,
        IRQ60         => IRQ_Interrupt_Handler'Address,
        IRQ61         => IRQ_Interrupt_Handler'Address,
        IRQ62         => IRQ_Interrupt_Handler'Address,
        IRQ63         => IRQ_Interrupt_Handler'Address,
        IRQ64         => IRQ_Interrupt_Handler'Address,
        IRQ65         => IRQ_Interrupt_Handler'Address,
        IRQ66         => IRQ_Interrupt_Handler'Address,
        IRQ67         => IRQ_Interrupt_Handler'Address,
        IRQ68         => IRQ_Interrupt_Handler'Address,
        IRQ69         => IRQ_Interrupt_Handler'Address,
        IRQ70         => IRQ_Interrupt_Handler'Address,
        IRQ71         => IRQ_Interrupt_Handler'Address,
        IRQ72         => IRQ_Interrupt_Handler'Address,
        IRQ73         => IRQ_Interrupt_Handler'Address,
        IRQ74         => IRQ_Interrupt_Handler'Address,
        IRQ75         => IRQ_Interrupt_Handler'Address,
        IRQ76         => IRQ_Interrupt_Handler'Address,
        IRQ77         => IRQ_Interrupt_Handler'Address,
        IRQ78         => IRQ_Interrupt_Handler'Address,
        IRQ79         => IRQ_Interrupt_Handler'Address,
        IRQ80         => IRQ_Interrupt_Handler'Address,
        IRQ81         => IRQ_Interrupt_Handler'Address)
     with Linker_Section => ".arm_vector_table",
          Export, Convention => Assembly,
          External_Name => "exception_vector_table";

private
   Interrupt_Vector_Table : array (External_Interrupt_Id) of
     Parameterless_Handler := (others => null);

   function To_Cortex_Priority (Priority : Interrupt_Priority)
                                   return Exception_Priority is
     (Exception_Priority (Interrupt_Priority'Last - Priority + 1) * 16);

   function To_Ada_Priority (Priority : Exception_Priority)
                             return Interrupt_Priority is
     (Interrupt_Priority'Last - Any_Priority'Base (Priority / 16) + 1);

end Oak.Processor_Support_Package.Interrupts;
