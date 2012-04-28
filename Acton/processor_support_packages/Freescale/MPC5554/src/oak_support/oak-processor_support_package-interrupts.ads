with Interfaces; use Interfaces;
with System; use System;
with MPC5554; use MPC5554;
with MPC5554.INTC; use MPC5554.INTC;
with MPC5554.H7F_Driver;
with MPC5554.Flash;

limited with Oak.Oak_Task;

package Oak.Processor_Support_Package.Interrupts is

   pragma Preelaborate;

   subtype Oak_Interrupt_Id is MPC5554.INTC.INTC_ID_Type;
   Default_Interrupt_Priority : constant Interrupt_Priority :=
                                  Interrupt_Priority'Last;

   type Parameterless_Handler is access protected procedure;

   procedure External_Interrupt_Handler;
   procedure Initialise_Interrupts;
   procedure Complete_Interrupt_Initialisation;

   procedure Attach_Handler (Interrupt : Oak_Interrupt_Id;
                             Handler   : Parameterless_Handler;
                             Priority  : Interrupt_Priority);
   procedure Get_Resource (PO : Oak.Oak_Task.Oak_Task_Handler);
   procedure Release_Resource;

   pragma Export (Asm,
                  External_Interrupt_Handler,
                  "__OI_External_Interrupt");

private
   type FIFO_Array is array
     (Oak_Interrupt_Id'First + 1 .. Oak_Interrupt_Id'Last)
       of MPC5554_Interrupt_Priority;

   type Interrupt_FIFO is record
      Stack : FIFO_Array;
      Top  : Oak_Interrupt_Id := 0;
   end record;

   Interrupt_Priority_FIFO : Interrupt_FIFO;

   Flash_Driver_Config     : aliased H7F_Driver.SSD_Config :=
     (Control_Register_Base => System'To_Address (Flash.Flash_Base_Address),
      Main_Array_Base       => System'To_Address (Flash.Array_Bases_Address),
      Main_Array_Size       => 0,
      Shadow_Row_Base       => System'To_Address (Flash.Shadow_Base_Address),
      Shadow_Row_Size       => 1024,
      Low_Block_Number      => 0,
      Mid_Block_Number      => 0,
      High_Block_Number     => 0,
      Page_Size             => H7F_Driver.H7FA_Page_Size,
      Debug_Mode_Selection  => 0);

   Saved_FBIUCR            : Flash.Flash_Bus_Interface_Unit_Control_Type;
   Handler_Staging_Slot    : Parameterless_Handler;

   Interrupt_Address_Block : constant := 16#FFFB#;
   Lock_All_Blocks         : constant := 16#FFFF#;
   Interrupt_Entry_Size    : constant := 8;

   ------------------------
   --  INTC Vector Table --
   ------------------------

   --  The INTC vector table is an array of Parameterless_Handler, which is an
   --  access-to-protected-subprogram pointer. These pointers consist of the
   --  address of the protected object and the address of the protected
   --  procedure and occupy two words in memory.
   --
   --  The INTC vector table is set up in two stages. The enties of the initial
   --  table programmed into Flash when the program is downloaded are set to
   --  the value (16#FFFF_FFFF#, 16#001F_FFFC#). This pair allows the table
   --  to be easily written to when handlers are later attached in the second
   --  stage.

   --  The choice of the value 16#1F_FFFC# for the default handler address is
   --  because we can only easily programm bits in Flash from 1 to 0. From this
   --  value we can easily program any valid word align address that falls
   --  within the Flash's memory space. It also points to the last word in
   --  Flash, enabling a default handler to run if an unregistered interrupt
   --  occurs.

   type Vector_Pair is record
      Protected_Object : Unsigned_32;
      Handler_Address  : Unsigned_32;
   end record;

   Default_Handler : constant Vector_Pair := (16#FFFF_FFFF#, 16#001F_FFFC#);

   Programmed_Vector_Table   : array (Oak_Interrupt_Id) of Vector_Pair :=
    ((16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#),
     (16#FFFF_FFFF#, 16#001F_FFFC#), (16#FFFF_FFFF#, 16#001F_FFFC#));

   pragma Linker_Section (Programmed_Vector_Table, ".intc_vector_table");
   pragma Export (Assembly, Programmed_Vector_Table, "__OI_Vector_Table");

   INTC_Vector_Table : array (Oak_Interrupt_Id) of Parameterless_Handler;
   pragma Import (Assembly, INTC_Vector_Table, "__OI_Vector_Table");

   --  Ensure we link to the defualt handler
   pragma Linker_Options ("-Wl,--undefined=__OI_Default_Handler");
end Oak.Processor_Support_Package.Interrupts;
