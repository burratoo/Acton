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

with Oak.Agent;                     use Oak.Agent;
with Oak.Oak_Time;                  use Oak.Oak_Time;

package Oak.Core with Preelaborate is

   -----------
   -- Types --
   -----------

   Global_Start_Time : Time;
   --  The global start time used by the system.

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

   procedure Context_Switch_To_Agent (Agent : in Oak_Agent_Id);
   --  Switches context to the provided agent.

   function Oak_Kernel return Kernel_Id with Inline_Always;
   --  Return the id of the current Oak_Kernel.

   procedure Run_Loop (Oak_Kernel : in Kernel_Id);
   --  Run-loop that runs once. Kernel schedules the procedure at a latter date
   --  to run the run-loop again. Should document the design descision behind
   --  this. Actaully the Run_Loop can run all it likes really. If there is
   --  nothing to do it can always just delay until. Though we are only really
   --  implementing delay until for tasks running on top the the kernel.
   --  Hmmm...

   procedure Start
     with Export, Convention => Ada, External_Name => "__oak_start";
   --  Called once by the system startup code to begin executing Oak the
   --  kernel.

   procedure Start_Oak_Kernel (Oak_Kernel : in Kernel_Id);
   --  Start a particular instance of the Oak Kernel.
   --  ??? Should be private?

private
   Global_Start_Time_Offset : Time_Span
     with Import, Convention => Ada,
          External_Name => "_global_start_phase";

   function Oak_Instance return Kernel_Id is
     (Kernel_Id'First);
   --  In theory on a multiprocessor machine we would query the processor to
   --  find out what its id is.
end Oak.Core;
