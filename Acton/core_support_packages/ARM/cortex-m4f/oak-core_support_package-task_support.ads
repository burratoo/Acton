------------------------------------------------------------------------------------------
--                                                                                      --
--                               OAK CORE SUPPORT PACKAGE                               --
--                                    ARM CORTEX M4F                                    --
--                                                                                      --
--                        OAK.CORE_SUPPORT_PACKAGE.TASK_SUPPORT                         --
--                                                                                      --
--                       Copyright (C) 2014-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Oak.Oak_Time;

with Oak.Agent;   use Oak.Agent;
with Oak.Core;    use Oak.Core;
with Oak.Message; use Oak.Message;
with Oak.States;  use Oak.States;

with System; use System;

package Oak.Core_Support_Package.Task_Support with Preelaborate is

   -----------------------
   -- INTERFACE FOR OAK --
   -----------------------

   procedure Initialise_Task_Enviroment;

   procedure Context_Switch with Inline_Always;
   procedure Context_Switch_From_Oak
     (Reason_For_Oak_To_Run : out Run_Reason;
      Message_Address       : out Address) with Inline_Always;
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
                                   Request : Agent_State);
   procedure Exited_Kernel_Trace (To_Agent : Oak_Agent_Id);

private

   SVC_Vector : Address
     with Export, Convention => Ada, External_Name => "svc_vector";

   SVC_Return_Vector : Address
     with Export, Convention => Ada, External_Name => "svc_return_vector";

end Oak.Core_Support_Package.Task_Support;
