------------------------------------------------------------------------------
--                                                                          --
--                         OAK CORE SUPPORT PACKAGE                         --
--                              ATMEL ARM7TDMI                              --
--                                                                          --
--                             ISA.ARM.ARM7TDMI                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2014-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

with System.Machine_Code; use System.Machine_Code;

package body ISA.ARM.ARM7TDMI is

   -------------------------------------
   -- Current_Program_Status_Register --
   -------------------------------------

   function Current_Program_Status_Register return Status_Type is
      CPSR : Status_Type;
   begin
      Asm ("mrs %0, cpsr",
           Outputs => Status_Type'Asm_Output ("=r", CPSR),
           Volatile => True);
      return CPSR;
   end Current_Program_Status_Register;

   -----------------------------------------
   -- Set_Current_Program_Status_Register --
   -----------------------------------------

   procedure Set_Current_Program_Status_Register
     (New_Value : in Status_Type) is
   begin
      Asm ("msr cpsr, %0",
           Inputs  => Status_Type'Asm_Input ("r", New_Value),
           Volatile => True);
   end Set_Current_Program_Status_Register;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode (New_Mode : Mode_Bits) is
      CPSR : Status_Type := Current_Program_Status_Register;
   begin
      CPSR.Mode := New_Mode;
      Set_Current_Program_Status_Register (CPSR);
   end Set_Mode;

   ---------------------------
   -- Switch_To_System_Mode --
   ---------------------------

   procedure Switch_To_System_Mode is
   begin
      Asm ("msr cpsr_c, #0xDF", Volatile => True);
      --  Interrupts disabled, System Mode
   end Switch_To_System_Mode;

   -------------------------------
   -- Switch_To_Supervisor_Mode --
   -------------------------------

   procedure Switch_To_Supervisor_Mode is
   begin
      Asm ("msr cpsr_c, #0xD3", Volatile => True);
      --  Interrupts disabled, System Mode
   end Switch_To_Supervisor_Mode;

   ------------------------
   -- Switch_To_IRQ_Mode --
   ------------------------

   procedure Switch_To_IRQ_Mode is
   begin
      Asm ("msr cpsr_c, #0xD2", Volatile => True);
      --  Interrupts disabled, IRQ Mode
   end Switch_To_IRQ_Mode;

   ------------------------
   -- Switch_To_FIQ_Mode --
   ------------------------

   procedure Switch_To_FIQ_Mode is
   begin
      Asm ("msr cpsr_c, #0xD1", Volatile => True);
      --  Interrupts disabled, FIQ Mode
   end Switch_To_FIQ_Mode;

   ----------------------------
   -- Disable_All_Interrupts --
   ----------------------------

   procedure Disable_All_Interrupts is
      CPSR : Status_Type := Current_Program_Status_Register;
   begin
      CPSR := (IRQ_Disable => True, FIQ_Disable => True, others => <>);
      Set_Current_Program_Status_Register (CPSR);
   end Disable_All_Interrupts;

   ---------------------------
   -- Enable_All_Interrupts --
   ---------------------------

   procedure Enable_All_Interrupts is
      CPSR : Status_Type := Current_Program_Status_Register;
   begin
      CPSR := (IRQ_Disable => False, FIQ_Disable => False, others => <>);
      Set_Current_Program_Status_Register (CPSR);
   end Enable_All_Interrupts;

end ISA.ARM.ARM7TDMI;
