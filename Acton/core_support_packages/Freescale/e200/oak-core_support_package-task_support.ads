--
--  Processor Support Package for the e200z6 PowerPC Core.
--
with System;
with Oak.Oak_Time;
with System.Storage_Elements;

package Oak.Core_Support_Package.Task_Support with Pure is

   --  This procedure saves the Kernel's registers to its respective store,
   --  loads in the task's registers, enables Oak's wakeup call and then
   --  switches the Instruction Register. On Power Architectures, we'll just
   --  place the instruction pointer and its stack pointer into a SPR. All this
   --  has to be in one function as we do not want to touch the stack as we
   --  carry out these steps. They all have to be done using assembly
   --  instructions as well. Store and load the registers from onto their
   --  respective task's or kernel's stack. If we are using a processor without
   --  SPRs we have to look up the kernel's SP either through a fixed memory
   --  address or reserve a register, the latter being unlikely.

   --  In the future we may want to speed things up by creating specialised
   --  context switch functions for switching between the kernel and the
   --  scheduler agents. The scheduler agents do not need to store all their
   --  registers on the call stack when they are switched out of context, as
   --  they only loose context once they have finished their one of their
   --  functions. Since this involves calling a Parameterless function the only
   --  registers to store is the Stack Pointer. Even then, this could just be
   --  set up by the kernel before the context switch... mmmm........

   procedure Context_Switch_To_Task with Inline_Always;
   procedure Context_Switch_To_Kernel with Inline_Always;
   procedure Context_Switch_To_Scheduler_Agent with Inline_Always;

   procedure Yield_Processor_To_Kernel with Inline_Always;

   procedure Set_Oak_Wake_Up_Timer (Wake_Up_At : Oak.Oak_Time.Time);
   procedure Disable_Oak_Wake_Up_Interrupt with Inline_Always;
   procedure Enable_Oak_Wake_Up_Interrupt with Inline_Always;

   procedure Sleep_Kernel;
   procedure Sleep_Task;

   DI, SI, CSTT, CSTS : constant System.Storage_Elements.Storage_Element;
   pragma Import (Assembler, DI, "__OTS_DI");
   pragma Import (Assembler, SI, "__OTS_SI");
   pragma Import (Assembler, CSTT, "__OTS_CSTT");
   pragma Import (Assembler, CSTS, "__OTS_CSTS");

   IVOR10_Decrementer_Intr : constant System.Address := DI'Address;
   IVOR10_Sleep_Intr       : constant System.Address := SI'Address;
   IVOR8_CS_To_Task        : constant System.Address := CSTT'Address;
   IVOR8_CS_To_Sleep       : constant System.Address := CSTS'Address;

end Oak.Core_Support_Package.Task_Support;
