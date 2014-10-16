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

end Oak.Core;
