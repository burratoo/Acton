with System; use System;
with System.Machine_Code;     use System.Machine_Code;

package body MPC5554.Flash is

   procedure Write_Flash_Bus_Interface_Unit_Control_Register
     (Contents : Flash_Bus_Interface_Unit_Control_Type)
   is
   begin
      --  Save Link Register
      Asm ("mflr  r4"           & ASCII.LF & ASCII.HT &
           --  Load address of the register writing function to r5
           "mr    r5, %0"       & ASCII.LF & ASCII.HT &
           "mtlr  r5"           & ASCII.LF & ASCII.HT &
           --  Load address of the Flash Bus Interface Unit Control Register
           "lis   r6, %1@ha"    & ASCII.LF & ASCII.HT &
           "addi  r6, r6, %1@l" & ASCII.LF & ASCII.HT &
           --  Move register contents to r7
           "mr    r7, %2"       & ASCII.LF & ASCII.HT &
           --  Branch to register writing function
           "blrl"               & ASCII.LF & ASCII.HT &
           --  Restore Link Register
           "mtlr  r4",
        Inputs => (Address'Asm_Input ("r", SRAM_LOAD'Address),
                   Address'Asm_Input ("i",
                     System'To_Address (Flash_Base_Address +
                         BIUCR_Offset_Address)),
                   Flash_Bus_Interface_Unit_Control_Type'Asm_Input ("r",
                     Contents)),
        Clobber => "r4, r5, r6, r7",
        Volatile => True);
   end Write_Flash_Bus_Interface_Unit_Control_Register;
end MPC5554.Flash;
