with Oak.Core_Support_Package.Processor;

with Oak.Agent; use Oak.Agent;
with Oak.Agent.Tasks; use Oak.Agent.Tasks;
with Oak.Core_Support_Package;      use Oak.Core_Support_Package;
with Oak.Memory.Call_Stack;         use Oak.Memory.Call_Stack;
with Oak.Scheduler;                 use Oak.Scheduler;
with System;                        use System;

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

   procedure Run_Loop (Oak_Instance : in out Oak_Data);
   --  Run-loop that runs once. Kernel schedules the procedure at a latter date
   --  to run the run-loop again. Should document the design descision behind
   --  this. Actaully the Run_Loop can run all it likes really. If there is
   --  nothing to do it can always just delay until. Though we are only really
   --  implementing delay until for tasks running on top the the kernel.
   --  Hmmm...

   function Current_Agent return access Oak_Agent'Class with Inline_Always;
   function Current_Agent_Stack_Pointer return Address with Inline_Always;
   function Current_Task return access Task_Agent'Class with Inline_Always;
   function Main_Task return access Task_Agent;
   function Oak_Instance return access Oak_Data with Inline_Always;
   function Scheduler_Info
     (Oak_Instance : access Oak_Data)
      return access Oak_Scheduler_Info with Inline_Always;

   procedure Set_Current_Agent_Stack_Pointer
     (SP : Address)
      with Inline_Always;
   procedure Set_Current_Agent
     (Agent : access Oak_Agent'Class)
      with Inline_Always;

private
   package Processor renames Oak.Core_Support_Package.Processor;

   Main_Task_OTCR : aliased Task_Agent;

   type Oak_Data is record
      Id            : Oak_Instance_Id   := 1;
      Scheduler     : aliased Oak_Scheduler_Info;
      Woken_By      : Activation_Reason := First_Run;
      Current_Agent : access Oak_Agent'Class := null;
      --  Probably need to fix this up so that it gets set somewhere. (In case
      --  it doesn't already when the task context switches.
      Call_Stack    : Call_Stack_Handler;
   end record;

   type Oak_List is array (Oak_Instance_Id) of aliased Oak_Data;

   Processor_Kernels : Oak_List;

   function Current_Agent return access Oak_Agent'Class is
     (Processor_Kernels (Processor.Proccessor_Id).Current_Agent);

   function Current_Agent_Stack_Pointer return Address is
     (Stack_Pointer (
        Processor_Kernels (Processor.Proccessor_Id).Current_Agent.all));

   function Current_Task return access Task_Agent'Class is
      (Task_Handler (Current_Agent));

   function Main_Task return access Task_Agent is (Main_Task_OTCR'Access);

   function Oak_Instance return access Oak_Data is
     (Processor_Kernels (Processor_Kernels'First)'Access);

   function Scheduler_Info
     (Oak_Instance : access Oak_Data)
      return access Oak_Scheduler_Info is (Oak_Instance.Scheduler'Access);

end Oak.Core;
