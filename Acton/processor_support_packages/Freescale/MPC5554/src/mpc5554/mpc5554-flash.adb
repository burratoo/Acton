with System.Machine_Code;     use System.Machine_Code;
with Ada.Unchecked_Conversion;

package body MPC5554.Flash is

--  Instead of throwing an exception, should probably return a code like
--  the Standard Software Driver does.
   procedure Initialise_For_Flash_Programming is
      MCR : Module_Configuration_Type renames Module_Configuration_Register;
      FBIUCR : Flash_Bus_Interface_Unit_Control_Type;
   begin
      if MCR.ECC_Error_Event = Occurred then
         raise Flash_Exception;
      elsif MCR.Read_While_Write_Error_Event = Occurred then
         raise Flash_Exception;
      elsif MCR.Stop_Mode = Enable then
         raise Flash_Exception;
      end if;

      --  Disable Prefetching and pipelining prior to programming.

      Saved_FBIUCR := Flash_Bus_Interface_Unit_Control_Register;
      FBIUCR := (Master_Prefetch => (others => Disable),
                 Address_Pipeline_Control => No_Pipelining,
                 Write_Wait_State_Control =>
                   Saved_FBIUCR.Write_Wait_State_Control,
                 Read_Wait_State_Control  =>
                   Saved_FBIUCR.Read_Wait_State_Control,
                 Data_Prefetch            => No_Prefetching,
                 Instruction_Prefetch     => No_Prefetching,
                 Prefetch_Limit           => Saved_FBIUCR.Prefetch_Limit,
                 FBIU_Line_Read_Buffers   => Disable);
      Write_Flash_Bus_Interface_Unit_Control_Register (FBIUCR);
   end Initialise_For_Flash_Programming;

   procedure Completed_Flash_Programming is
   begin
      Write_Flash_Bus_Interface_Unit_Control_Register (Saved_FBIUCR);
   end Completed_Flash_Programming;

   procedure Program_Protected_Access
     (P           : access protected procedure;
      Destination : in Address)
   is
      MCR : Module_Configuration_Type := Module_Configuration_Register;

      P_Destination : access protected procedure;

      --  It would be preferable to use Address_To_Access_Conversions instead
      --  of the Address clause but that would require a new type. Import
      --  declaration is needed since we are overlapping memory.
      for P_Destination'Address use Destination;
      pragma Import (Ada, P_Destination);
   begin
      Do_Not_Clear_Error_States (MCR);
      MCR.Program := Executing;
      Module_Configuration_Register := MCR;

      P_Destination := P;

      MCR.High_Voltage := Enable;
      Module_Configuration_Register := MCR;

      while Module_Configuration_Register.Done = No loop
         null;
      end loop;

      MCR.High_Voltage := Disable;
      Module_Configuration_Register := MCR;

      if Module_Configuration_Register.Program_Erase_Good = No then
         MCR.Program := Not_Executing;
         Module_Configuration_Register := MCR;
         raise Flash_Exception;
      end if;

      MCR.Program := Not_Executing;
      Module_Configuration_Register := MCR;

   end Program_Protected_Access;

   procedure Unlock_Space_Block_Locking_Register
     (Space   : in Address_Space)
   is
      function To_LMLR is new Ada.Unchecked_Conversion
        (Unsigned_32, Low_Mid_Address_Space_Block_Locking_Type);
      function To_HLR is new Ada.Unchecked_Conversion
        (Unsigned_32, High_Address_Space_Block_Locking_Type);
   begin
      case Space is
         when Shadow_Primary | Low_Primary | Mid_Primary =>
            if Low_Mid_Address_Space_Block_Locking_Register.Locks =
              Not_Editable then
               Low_Mid_Address_Space_Block_Locking_Register :=
                 To_LMLR (LMLR_Password);
            end if;

         when Shadow_Secondary | Low_Secondary | Mid_Secondary =>
            if Secondary_Low_Mid_Address_Space_Block_Locking_Register.Locks =
              Not_Editable then
               Secondary_Low_Mid_Address_Space_Block_Locking_Register :=
                 To_LMLR (SLMLR_Password);
            end if;

         when High =>
            if High_Address_Space_Block_Locking_Register.Locks =
              Not_Editable then
               High_Address_Space_Block_Locking_Register :=
                 To_HLR (HLR_Password);
            end if;
      end case;
   end Unlock_Space_Block_Locking_Register;

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

   procedure Do_Not_Clear_Error_States (MCR : in out Module_Configuration_Type)
   is
   begin
      MCR.ECC_Error_Event := Not_Occurred;
      MCR.Read_While_Write_Error_Event := Not_Occurred;
   end Do_Not_Clear_Error_States;

end MPC5554.Flash;
