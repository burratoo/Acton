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

package Oak.Core with Preelaborate is

   -----------
   -- Types --
   -----------

   Global_Start_Time : Time;
   --  The global start time used by the system.

   type Run_Reason is (First_Run, Task_Yield, Timer, External_Interrupt);
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

   procedure Perform_Quick_Switch (Message : in out Oak_Message)
     with Inline => False;
   --  Performs a quick switch to the scheduler agent. Does not save most
   --  registers. Ensure that this is not inlined.

   procedure Run_Loop with No_Return;
   --  The Oak kernel's run loop that performs the kernel's operations.

   procedure Run_Oak
     (Reason_For_Run : in     Run_Reason;
      Message        : in out Oak_Message);
   --  Run Oak once to handle the reason for why Oak needs to run.

   function This_Oak_Kernel return Kernel_Id with Inline_Always;
   --  Return the id of the current Oak_Kernel.

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

   function This_Oak_Kernel return Kernel_Id is
     (Kernel_Id'First);
   --  In theory on a multiprocessor machine we would query the processor to
   --  find out what its id is.
end Oak.Core;
