with Oak.Agent.Tasks;
with Oak.Core_Support_Package.Task_Support;
use Oak.Core_Support_Package.Task_Support;

with System.Machine_Code;     use System.Machine_Code;

package body Oak.Processor_Support_Package.Interrupts is
   --  Stack frame design for interrupt handling on the MPC5554 with support
   --  for SPE unit:
   --              ****************
   --     0xC      *   R24        * ---------------
   --     0xB      *   R18        *       ^
   --     0xA      *   R19        *       |
   --     0x9      *   R20        *       |
   --     0x8      *   R21        *       |
   --     0x7      *   R22        *       |
   --     0x6      *   R23        *     8 bits
   --     0x5      *   R25        *       |
   --     0x4      *   R26        *       |
   --     0x3      *   R27        *       |
   --     0x2      *   R30        *       |
   --     0x1      *   R31        *       v
   --     0x0      *   SREG       * ---------------
   --              ****************
   --
   --  Depth: 0xC or 13 bytes

   --  R24 is stored by the inital handler.

   procedure Interrupt_Handler (Id : Oak_Interrupt_Id) is
   begin
      --  Create Stack frame, save working registers and save srr0 and SRR1
      Asm (
           "push r18"  & ASCII.LF & ASCII.HT &
           "push r19"  & ASCII.LF & ASCII.HT &
           "push r20"  & ASCII.LF & ASCII.HT &
           "push r21"  & ASCII.LF & ASCII.HT &
           "push r22"  & ASCII.LF & ASCII.HT &
           "push r23"  & ASCII.LF & ASCII.HT &
           "push r25"  & ASCII.LF & ASCII.HT &
           "push r26"  & ASCII.LF & ASCII.HT &
           "push r27"  & ASCII.LF & ASCII.HT &
           "push r30"  & ASCII.LF & ASCII.HT &
           "push r31"  & ASCII.LF & ASCII.HT &
           "in   r18, __SREG__"  & ASCII.LF & ASCII.HT &
           "push r18",
           Volatile => True);

      Interrupt_Vector_Table (Id).all;

      Asm (
           "pop  r18"          & ASCII.LF & ASCII.HT &
           "out __SREG__, r0"  & ASCII.LF & ASCII.HT &
           "pop  r31"          & ASCII.LF & ASCII.HT &
           "pop  r30"          & ASCII.LF & ASCII.HT &
           "pop  r27"          & ASCII.LF & ASCII.HT &
           "pop  r26"          & ASCII.LF & ASCII.HT &
           "pop  r25"          & ASCII.LF & ASCII.HT &
           "pop  r23"          & ASCII.LF & ASCII.HT &
           "pop  r22"          & ASCII.LF & ASCII.HT &
           "pop  r21"          & ASCII.LF & ASCII.HT &
           "pop  r20"          & ASCII.LF & ASCII.HT &
           "pop  r19"          & ASCII.LF & ASCII.HT &
           "pop  r18"          & ASCII.LF & ASCII.HT &
           --  Return
           "reti",
        Volatile => True);

   end Interrupt_Handler;

   procedure Initialise_Interrupts is

   begin
      null;
   end Initialise_Interrupts;

   procedure Complete_Interrupt_Initialisation is
   begin
      null;
   end Complete_Interrupt_Initialisation;

   procedure Attach_Handler (Interrupt : Oak_Interrupt_Id;
                             Handler   : Parameterless_Handler;
                             Priority  : Interrupt_Priority) is
      pragma Unreferenced (Priority);
   begin
      Interrupt_Vector_Table (Interrupt) := Handler;
   end Attach_Handler;

   procedure Get_Resource
     (PO : access Agent.Tasks.Protected_Objects.Protected_Agent'Class)
   is
      pragma Unreferenced (PO);
   begin
      Task_Not_Interruptible;
   end Get_Resource;

   procedure Release_Resource
     (PO : access Agent.Tasks.Protected_Objects.Protected_Agent'Class)
   is
      pragma Unreferenced (PO);
   begin
      Task_Interruptible;
   end Release_Resource;

   procedure Vector_1 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (1);
   end Vector_1;

   procedure Vector_2 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (2);
   end Vector_2;

   procedure Vector_3 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (3);
   end Vector_3;

   procedure Vector_4 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (4);
   end Vector_4;

   procedure Vector_5 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (5);
   end Vector_5;

   procedure Vector_6 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (6);
   end Vector_6;

   procedure Vector_7 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (7);
   end Vector_7;

   procedure Vector_8 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (8);
   end Vector_8;

   procedure Vector_9 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (9);
   end Vector_9;

   procedure Vector_10 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (10);
   end Vector_10;

   procedure Vector_11 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (11);
   end Vector_11;

   procedure Vector_12 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (12);
   end Vector_12;

   procedure Vector_13 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (13);
   end Vector_13;

   procedure Vector_14 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (14);
   end Vector_14;

   procedure Vector_15 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (15);
   end Vector_15;

   procedure Vector_16 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (16);
   end Vector_16;

   procedure Vector_17 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (17);
   end Vector_17;

   procedure Vector_18 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (18);
   end Vector_18;

   procedure Vector_19 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (19);
   end Vector_19;

   procedure Vector_20 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (20);
   end Vector_20;

   procedure Vector_21 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (21);
   end Vector_21;

   procedure Vector_22 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (22);
   end Vector_22;

   procedure Vector_23 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (23);
   end Vector_23;

   procedure Vector_24 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (24);
   end Vector_24;

   procedure Vector_25 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (25);
   end Vector_25;

   procedure Vector_26 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (26);
   end Vector_26;

   procedure Vector_27 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (27);
   end Vector_27;

   procedure Vector_28 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (28);
   end Vector_28;

   procedure Vector_29 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (29);
   end Vector_29;

   procedure Vector_30 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (31);
   end Vector_30;

   procedure Vector_31 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (31);
   end Vector_31;

   procedure Vector_32 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (32);
   end Vector_32;

   procedure Vector_33 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (33);
   end Vector_33;

   procedure Vector_34 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (34);
   end Vector_34;

   procedure Vector_35 is
   begin
      Asm ("push r24", Volatile => True);
      Interrupt_Handler (35);
   end Vector_35;

end Oak.Processor_Support_Package.Interrupts;
