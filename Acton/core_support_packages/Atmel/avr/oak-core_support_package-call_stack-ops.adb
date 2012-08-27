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
        ("st   %a0+, %A1" & ASCII.LF & ASCII.HT &
         "st   %a0,  %B1",
         Inputs   => (Address'Asm_Input ("b", Stack.Pointer),
                      Address'Asm_Input ("r", Instruction_Address)),
         Volatile => True);
   end Set_Task_Instruction_Pointer;

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
        ("st  %a0,    %A1" & ASCII.LF & ASCII.HT &
         "std %a0+1,  %B1" & ASCII.LF & ASCII.HT &
         "std %a0+9,  %A2" & ASCII.LF & ASCII.HT &
         "std %a0+10, %B2",
         Inputs   => (Address'Asm_Input ("b", Stack.Pointer),
                      Address'Asm_Input ("r", Procedure_Address),
                      Address'Asm_Input ("r", Task_Value_Record)),
         Volatile => True);
   end Set_Task_Body_Procedure;

   procedure Initialise_Call_Stack
     (Stack             : in out Oak.Memory.Call_Stack.Call_Stack_Handler;
      Start_Instruction : in System.Address)
   is
   begin
      Stack.Pointer := Stack.Pointer -
        Task_Agents.Oak_Task_Message_Store'Size / System.Storage_Unit;
      Stack.Pointer := Stack.Pointer - Task_Registers_Save_Size;
      Set_Task_Body_Procedure
        (Stack             => Stack,
         Procedure_Address => Start_Instruction,
         Task_Value_Record => System.Null_Address);
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
