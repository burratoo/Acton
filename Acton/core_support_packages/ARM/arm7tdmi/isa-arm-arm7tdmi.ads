------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                     ARM ARM7TDMI                                     --
--                                                                                      --
--                                   ISA.ARM.ARM7TDMI                                   --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------


package ISA.ARM.ARM7TDMI with Pure is

   type Instruction_Mode is (ARM, Thumb);

   type Mode_Bits is (User, FIQ, IRQ, Supervisor,
                      Abort_Mode, Undefined, System_Mode);

   type Status_Type is record
      Mode               : Mode_Bits;
      State              : Instruction_Mode;
      FIQ_Disable        : Boolean;
      IRQ_Disable        : Boolean;
      Overflow           : Boolean;
      Carry              : Boolean;
      Zero               : Boolean;
      Negative_Less_Than : Boolean;
   end record;

   for Mode_Bits use (User        => 2#10000#,
                      FIQ         => 2#10001#,
                      IRQ         => 2#10010#,
                      Supervisor  => 2#10011#,
                      Abort_Mode  => 2#10111#,
                      Undefined   => 2#11011#,
                      System_Mode => 2#11111#);

   for Status_Type use record
      Mode               at 0 range 0 .. 4;
      State              at 0 range 5 .. 5;
      FIQ_Disable        at 0 range 6 .. 6;
      IRQ_Disable        at 0 range 7 .. 7;
      Overflow           at 0 range 28 .. 28;
      Carry              at 0 range 29 .. 29;
      Zero               at 0 range 30 .. 30;
      Negative_Less_Than at 0 range 31 .. 31;
   end record;

   --  Ensure these are inlined
   procedure Set_Mode (New_Mode : Mode_Bits) with Inline_Always;

   function Current_Program_Status_Register return Status_Type
     with Inline_Always;
   function Saved_Program_Status_Register return Status_Type
     with Inline_Always;
   procedure Set_Current_Program_Status_Register (New_Value : in Status_Type)
     with Inline_Always;

   procedure Switch_To_Supervisor_Mode with Inline_Always;
   --  Switches to Supervisor Mode with interrupts disabled

   procedure Switch_To_System_Mode with Inline_Always;
   --  Switches to System Mode with interrupts disabled

   procedure Switch_To_IRQ_Mode with Inline_Always;
   --  Switches to IRQ Mode with interrupts disabled

   procedure Switch_To_FIQ_Mode with Inline_Always;
   --  Switches to FIQ Mode with interrupts disabled

   --  Ensure these are not inlined

   procedure Disable_All_Interrupts;
   procedure Enable_All_Interrupts;
end ISA.ARM.ARM7TDMI;
