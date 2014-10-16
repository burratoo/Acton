------------------------------------------------------------------------------
--                                                                          --
--                         OAK CORE SUPPORT PACKAGE                         --
--                              FREESCALE e200                              --
--                                                                          --
--                  OAK.CORE_SUPPORT_PACKAGE.TASK_SUPPORT                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                 Copyright (C) 2010-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

--
--  Core Support Package for the e200z6 PowerPC Core.
--

with Oak.Oak_Time;

with Oak.Agent;   use Oak.Agent;
with Oak.Core;    use Oak.Core;
with Oak.Message; use Oak.Message;
with Oak.States;  use Oak.States;
with System;      use System;

package Oak.Core_Support_Package.Task_Support with Preelaborate is

   --  On ARM the kernel runs in the Supervisor Mode and is switched directly
   --  to by a procedure call (unlike Power which requires a software
   --  interrupt). All other tasks operate in User Mode.

   procedure Initialise_Task_Enviroment;

   procedure Context_Switch with Inline_Always;
   procedure Context_Switch_From_Oak
     (Reason_For_Oak_To_Run : out Run_Reason;
      Message_Address       : out Address);
   procedure Context_Switch_To_Oak
     (Reason_For_Run : in     Run_Reason;
      Message        : in out Oak_Message) with Inline => False;
   procedure Context_Switch_Save_Callee_Registers with Inline_Always;
   --  Procedures that initiate the context switch.

   procedure Context_Switch_Will_Be_To_Interrupted_Task with Inline_Always;
   procedure Context_Switch_Will_Be_To_Agent with Inline_Always;
   procedure Context_Switch_Will_Switch_In_Place with Inline_Always;
   --  Procedures that set up the appropriate interrupt handlers.

   procedure Enter_Barrier_Function with Inline_Always;
   procedure Exit_Barrier_Function with Inline_Always;

   procedure Set_Oak_Wake_Up_Timer (Wake_Up_At : in Oak.Oak_Time.Time);

   procedure Sleep_Agent_Run_Loop;

   procedure Entered_Kernel_Trace (Reason  : Run_Reason;
                                   Request : Agent_State) is null;
   procedure Exited_Kernel_Trace (To_Agent : Oak_Agent_Id) is null;

private
   SWI_Vector   : Address
     with Import, Convention => Asm, External_Name => "swi_vector";

   SWI_Return_Vector : Address
     with Export, Convention => Ada, External_Name => "swi_return_vector";
end Oak.Core_Support_Package.Task_Support;
