package ISA.Power.e200.Processor_Control_Registers is
   pragma Pure;

   ---------------
   --  Machine State Register
   ---------------

   type Computation_Mode_Type is (Mode_32, Mode_64);
   type Set_Type is (Clear, Set);
   type Processor_Mode is (Supervisor, User);
   type Available_Type is (Not_Available, Available);
   type Address_Space is range 0 .. 1;

   type Machine_State_Register_Type is record
      Computation_Mode           : Computation_Mode_Type;
      Interrupt_Computation_Mode : Set_Type;
      User_Mode_Cache_Lock       : Enable_Type;
      Signal_Processing          : Enable_Type;
      Wait_State                 : Enable_Type;
      Critical_Interrupt         : Enable_Type;
      External_Interrupts        : Enable_Type;
      User_Mode                  : Processor_Mode;
      Floating_Point             : Available_Type;
      Machine_Check              : Enable_Type;
      FP_Exception_Mode_0        : Boolean;
      Debug_Interrupt            : Enable_Type;
      FP_Exception_Mode_1        : Boolean;
      Instruction_Address_Space  : Address_Space;
      Data_Address_Space         : Address_Space;
      Performance_Monitor        : Enable_Type;
   end record;

   for Computation_Mode_Type use (Mode_32 => 0, Mode_64 => 1);
   for Set_Type use (Clear => 0, Set => 1);
   for Processor_Mode use (Supervisor => 0, User => 1);
   for Available_Type use (Not_Available => 0, Available => 1);

   for Machine_State_Register_Type use record
      Computation_Mode           at 0 range 0 .. 0;
      Interrupt_Computation_Mode at 0 range 1 .. 1;
      User_Mode_Cache_Lock       at 0 range 5 .. 5;
      Signal_Processing          at 0 range 6 .. 6;
      Wait_State                 at 0 range 13 .. 13;
      Critical_Interrupt         at 0 range 14 .. 14;
      External_Interrupts        at 0 range 16 .. 16;
      User_Mode                  at 0 range 17 .. 17;
      Floating_Point             at 0 range 18 .. 18;
      Machine_Check              at 0 range 19 .. 19;
      FP_Exception_Mode_0        at 0 range 20 .. 20;
      Debug_Interrupt            at 0 range 22 .. 22;
      FP_Exception_Mode_1        at 0 range 23 .. 23;
      Instruction_Address_Space  at 0 range 26 .. 26;
      Data_Address_Space         at 0 range 27 .. 27;
      Performance_Monitor        at 0 range 29 .. 29;
   end record;

   for Machine_State_Register_Type'Size use Register_Size_32;

end ISA.Power.e200.Processor_Control_Registers;
