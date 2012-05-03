with Oak.Scheduler;                 use Oak.Scheduler;
with Oak.Core_Support_Package;      use Oak.Core_Support_Package;
with Oak.Oak_Task;                  use Oak.Oak_Task;
with System;                        use System;
with Oak.Memory.Call_Stack;         use Oak.Memory.Call_Stack;

package Oak.Core with Preelaborate is

   type Activation_Reason is (
      First_Run,
      Task_Yield,
      Scheduler_Agent,
      Missed_Deadline);

   type Oak_Data is limited private;

   procedure Initialise;
   pragma Export (Ada, Initialise, "__oak_initialise");

   procedure Complete_Initialisation;
   pragma Export (Ada, Complete_Initialisation,
                  "__oak_complete_initialisation");

   procedure Start; --  System initialisation routine.
   pragma Export (Ada, Start, "__oak_start");

   procedure Start_Oak_Instance (Oak_Instance : in out Oak_Data);

   --  Run-loop that runs once. Kernel schedules the procedure at a latter date
   --  to run the run-loop again. Should document the design descision behind
   --  this. Actaully the Run_Loop can run all it likes really. If there is
   --  nothing to do it can always just delay until. Though we are only really
   --  implementing delay until for tasks running on top the the kernel.
   --  Hmmm...
   procedure Run_Loop (Oak_Instance : in out Oak_Data);

   function Get_Current_Task_Stack_Pointer return Address with Inline_Always;
   procedure Set_Current_Task_Stack_Pointer (SP : Address) with Inline_Always;
   procedure Set_Current_Task (T : Oak_Task_Handler) with Inline_Always;
   function Get_Current_Task return Oak_Task_Handler with Inline_Always;

   function Get_Oak_Instance return access Oak_Data with Inline_Always;
   function Get_Scheduler_Info
     (Oak_Instance : access Oak_Data)
      return         access Oak_Scheduler_Info with Inline_Always;
   function Get_Main_Task return Oak_Task_Handler;

private
   Main_Task_OTCR : aliased Oak.Oak_Task.Oak_Task;

   type Oak_Data is record
      Id           : Oak_Instance_Id   := 1;
      Scheduler    : aliased Oak_Scheduler_Info;
      Woken_By     : Activation_Reason := First_Run;
      Current_Task : Oak_Task_Handler  := null;
      --  Probably need to fix this up so that it gets set somewhere. (In case
      --  it doesn't already when the task context switches.
      Call_Stack : Call_Stack_Handler;
   end record;

   type Oak_List is array (Oak_Instance_Id) of aliased Oak_Data;
   --   for Oak_List'Address use

   Processor_Kernels : Oak_List;

end Oak.Core;
