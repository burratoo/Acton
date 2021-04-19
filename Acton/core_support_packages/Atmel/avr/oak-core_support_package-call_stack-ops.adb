------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                      ATMEL AVR                                       --
--                                                                                      --
--                        OAK.CORE_SUPPORT_PACKAGE.CALL_STACK.OPS                       --
--                                                                                      --
--                       Copyright (C) 2012-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Oak.Agent.Tasks;
with System;

with System.Machine_Code; use System.Machine_Code;

package body Oak.Core_Support_Package.Call_Stack.Ops is
   use System.Storage_Elements;

   package Task_Agents renames Oak.Agent.Tasks;

   pragma Warnings (Off);
   function To_Message_Loc is
     new Ada.Unchecked_Conversion
       (Source => System.Address,
        Target => Task_Agents.Oak_Task_Message_Location);
   pragma Warnings (On);

   ----------------------------------
   -- Set_Task_Instruction_Pointer --
   ----------------------------------

   procedure Set_Task_Instruction_Pointer
     (Stack               : in Oak.Memory.Call_Stack.Call_Stack_Handler;
      Instruction_Address : in System.Address)
   is
      use System;

   begin
      Asm
        ("std  %a0+32,  r1" & ASCII.LF & ASCII.HT &  -- r1
         "std  %a0+35, %A1" & ASCII.LF & ASCII.HT &
         "std  %a0+34, %B1" & ASCII.LF & ASCII.HT &
         "std  %a0+1,   r1", -- Disables interrupts for the task
         Inputs   => (Address'Asm_Input ("b", Stack.Pointer),
                      Address'Asm_Input ("r", Instruction_Address)),
         Volatile => True);
   end Set_Task_Instruction_Pointer;

   -------------------------------
   --  Set_Task_Body_Procedure  --
   -------------------------------

   --  On the AVR we place the task's procedure address before the registers.
   --  The Task_Value_Record goes in registers r24 and r25. The zero register,
   --  r1, is also zeroed. Note that on the AVR the stack preincrements, so
   --  that data we place on the stack is one position behind were we think it
   --  should be. We also zero the status register.

   procedure Set_Task_Body_Procedure
     (Stack             : in Oak.Memory.Call_Stack.Call_Stack_Handler;
      Procedure_Address : in System.Address;
      Task_Value_Record : in System.Address)
   is
      use System;

   begin
      --  Set r24 and r25 to the memory address of the task value record,
      --  which corresponds to the first parameter of the task body procedure.

      Asm
        ("std %a0+32,  r1" & ASCII.LF & ASCII.HT &
         "std %a0+35, %A1" & ASCII.LF & ASCII.HT &
         "std %a0+34, %B1" & ASCII.LF & ASCII.HT &
         "std %a0+9,  %A2" & ASCII.LF & ASCII.HT &
         "std %a0+8,  %B2" & ASCII.LF & ASCII.HT &
         "ldi r16,   0x80" & ASCII.LF & ASCII.HT &
         "std %a0+1,  r16", -- Enables interrupts for the task
         Inputs   => (Address'Asm_Input ("b", Stack.Pointer),
                      Address'Asm_Input ("r", Procedure_Address),
                      Address'Asm_Input ("r", Task_Value_Record)),
         Volatile => True,
         Clobber  => "r16");
   end Set_Task_Body_Procedure;

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address)
   is
   begin
      Stack.Pointer := Stack.Pointer -
        Task_Agents.Oak_Task_Message_Store'Size / System.Storage_Unit;
      Stack.Pointer := Stack.Pointer - Task_Registers_Save_Size;
      Set_Task_Instruction_Pointer
        (Stack             => Stack,
         Instruction_Address => Start_Instruction);
   end Initialise_Call_Stack;

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address;
      Task_Value_Record : in System.Address;
      Message_Location  : out Task_Agents.Oak_Task_Message_Location)
   is
   begin
      Stack.Pointer := Stack.Pointer -
        Task_Agents.Oak_Task_Message_Store'Size / System.Storage_Unit;
      Message_Location := To_Message_Loc (Stack.Pointer);
      Message_Location.Yield_Status := Task_Agents.Voluntary;
      Stack.Pointer := Stack.Pointer - Task_Registers_Save_Size;
      Set_Task_Body_Procedure
        (Stack               => Stack,
         Procedure_Address => Start_Instruction,
         Task_Value_Record    => Task_Value_Record);
   end Initialise_Call_Stack;

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address;
      Task_Value_Record : in System.Address;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count;
      Message_Location  : out Task_Agents.Oak_Task_Message_Location)
   is
   begin
      Stack.Top     := Stack_Address +  Stack_Size;
      Stack.Pointer := Stack.Top;
      Stack.Bottom  := Stack_Address;

      Initialise_Call_Stack
        (Stack             => Stack,
         Start_Instruction => Start_Instruction,
         Task_Value_Record => Task_Value_Record,
         Message_Location  => Message_Location);
   end Initialise_Call_Stack;

end Oak.Core_Support_Package.Call_Stack.Ops;
