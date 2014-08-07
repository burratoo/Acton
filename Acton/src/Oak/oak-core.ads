------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                                 OAK.CORE                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--  This package provides the core kernel services provided by Oak. It includes
--  the kernel setup routines, the kernel run loop and the general agent
--  switching services.

with Oak.Agent;    use Oak.Agent;
with Oak.Message;  use Oak.Message;
with Oak.Oak_Time; use Oak.Oak_Time;
with Oak.States;   use Oak.States;
with Oak.Timers;   use Oak.Timers;

package Oak.Core with Preelaborate is

   -----------
   -- Types --
   -----------

   Global_Start_Time : Time;
   --  The global start time used by the system.

   type Run_Reason is (Agent_Request, Timer, External_Interrupt);
   --  The reason why Oak was run.

   -----------------
   -- Subprograms --
   -----------------

   --  Ininitialisation
   --  ???? A bit about initialising Oak.

   procedure Initialise
     with Export, Convention => Ada, External_Name => "__oak_initialise";
   --  The first stage of Oak's intialisation routine. Called once by the
   --  by the program/system's startup code.

   procedure Complete_Initialisation
     with Export, Convention => Ada,
     External_Name =>  "__oak_complete_initialisation";
   --  Called once by the system's startup code after other parts of the system
   --  have completed initialisation. This includes after setting up the top
   --  level scheduler agents and the main task.

   procedure Request_Oak_Service
     (Reason_For_Run : in     Run_Reason;
      Message        : in out Oak_Message) with Inline => False;
   --  Called by agents to request something from Oak. The signature of this
   --  procedure must be the same as the Run_Oak.

   procedure Run_Loop with No_Return;
   --  The Oak kernel's run loop that performs the kernel's operations.

   procedure Start
     with Export, Convention => Ada, External_Name => "__oak_start";
   --  Called once by the system startup code to begin executing Oak the
   --  kernel.

   procedure Start_Oak_Kernel;
   --  Start a particular instance of the Oak Kernel.
   --  ??? Should be private?

   procedure Update_Entry_Stats (Oak_Kernel : in Kernel_Id);
   --  Update run-time statistics on entry to the kernel.

   procedure Update_Exit_Stats (Oak_Kernel : in Kernel_Id);
   --  Update run-time statistics on exit of the kernel.

private
   Global_Start_Time_Offset : Time_Span
     with Import, Convention => Ada,
     External_Name => "_global_start_phase";

   No_Message_Here : aliased Oak_Message := (Message_Type => No_Message);

   type Invoke_Count is mod 2 ** 32 with Size => 32;
   type Run_Reason_Count is array (Run_Reason) of Invoke_Count;
   type Message_Reason_Count is array (Agent_State) of Invoke_Count;
   type Timer_Kind_Count is array (Oak_Timer_Kind) of Invoke_Count;

   type Invoke_Reason is record
      Reason_For_Run : Run_Reason_Count;
      Message_Reason : Message_Reason_Count;
      Timer_Kind     : Timer_Kind_Count;
      Early_Fire     : Invoke_Count;
   end record;

   Invoke_Reason_Table : Invoke_Reason;
end Oak.Core;
