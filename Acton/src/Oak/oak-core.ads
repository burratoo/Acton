with Oak.Core_Support_Package.Processor;

with Oak.Agent; use Oak.Agent;
with Oak.Agent.Tasks; use Oak.Agent.Tasks;
with Oak.Agent.Tasks.Interrupts; use Oak.Agent.Tasks.Interrupts;
with Oak.Core_Support_Package;      use Oak.Core_Support_Package;
with Oak.Oak_Time;                  use Oak.Oak_Time;
with Oak.Timers;
with Oak.Scheduler;                 use Oak.Scheduler;
with System;                        use System;

package Oak.Core with Preelaborate is

   Global_Start_Time : Time;

   type Activation_Reason is (
      First_Run,
      Task_Yield,
      Timer,
      External_Interrupt);

   type Active_State is (Inactive, Active);

   type Oak_Data is new Oak_Agent with private;

   procedure Initialise
     with Export, Convention => Ada, External_Name => "__oak_initialise";

   procedure Complete_Initialisation
     with Export, Convention => Ada,
          External_Name =>  "__oak_complete_initialisation";

   procedure Start
     with Export, Convention => Ada, External_Name => "__oak_start";
   --  System initialisation routine.

   procedure Start_Oak_Instance (Oak_Instance : in out Oak_Data);

   procedure Run_Loop (Oak_Instance : in out Oak_Data);
   --  Run-loop that runs once. Kernel schedules the procedure at a latter date
   --  to run the run-loop again. Should document the design descision behind
   --  this. Actaully the Run_Loop can run all it likes really. If there is
   --  nothing to do it can always just delay until. Though we are only really
   --  implementing delay until for tasks running on top the the kernel.
   --  Hmmm...

   function Current_Agent    return access Oak_Agent'Class with Inline_Always;
   function Current_Agent_Stack_Pointer return Address with Inline_Always;
   function Current_Task     return access Task_Agent'Class with Inline_Always;
   function Main_Task        return access Task_Agent;
   function Oak_Instance     return access Oak_Data'Class with Inline_Always;
   function Oak_Stack_Pointer return Address with Inline_Always;
   function Oak_Timer_Store   return access Oak.Timers.Oak_Timer_Info
     with Inline_Always;
   function Scheduler_Info
     (Oak_Instance : access Oak_Data'Class)
      return access Oak_Scheduler_Info with Inline_Always;

   procedure Context_Switch_To_Agent (Agent : access Oak_Agent'Class);

   procedure Set_Current_Agent_Stack_Pointer
     (SP : Address)
      with Inline_Always;
   procedure Set_Current_Agent
     (Agent : access Oak_Agent'Class)
      with Inline_Always;

   procedure Set_Oak_Stack_Pointer
     (SP : Address)
      with Inline_Always;

private
   package Processor renames Oak.Core_Support_Package.Processor;

   type IA_Store is array (Interrupt_Priority) of aliased Interrupt_Agent;
   type Interrupt_Active_Set is array (Interrupt_Priority) of Active_State
     with Pack;

   Main_Task_OTCR : aliased Task_Agent;

   type Oak_Data is new Oak_Agent with record
      Scheduler          : aliased Oak_Scheduler_Info;
      Woken_By           : Activation_Reason := First_Run;
      Current_Priority   : System.Any_Priority := System.Any_Priority'First;
      Current_Agent      : access Oak_Agent'Class := null;
      Entry_Exit_Stamp   : Time;
      --  Probably need to fix this up so that it gets set somewhere. (In case
      --  it doesn't already when the task context switches.
      Sleep_Agent        : aliased Task_Agent;
      Interrupt_Agents   : IA_Store;
      Interrupt_States   : Interrupt_Active_Set;
      Oak_Timers         : aliased Oak.Timers.Oak_Timer_Info;
   end record;

   type Oak_List is array (Oak_Instance_Id) of aliased Oak_Data;

   Processor_Kernels : Oak_List;

   Global_Start_Time_Offset : Time_Span
     with Import, Convention => Ada,
          External_Name => "_global_start_offset";

   function Current_Agent return access Oak_Agent'Class is
     (Processor_Kernels (Processor.Proccessor_Id).Current_Agent);

   function Current_Agent_Stack_Pointer return Address is
     (Stack_Pointer (
        Processor_Kernels (Processor.Proccessor_Id).Current_Agent.all));

   function Current_Task return access Task_Agent'Class is
     (Task_Handler (Current_Agent));

   function Main_Task return access Task_Agent is (Main_Task_OTCR'Access);

   function Oak_Instance return access Oak_Data'Class is
     (Processor_Kernels (Processor_Kernels'First)'Access);

   function Oak_Stack_Pointer return Address is
     (Processor_Kernels (Processor.Proccessor_Id).Stack_Pointer);

   function Oak_Timer_Store return access Oak.Timers.Oak_Timer_Info  is
     (Processor_Kernels
        (Processor.Proccessor_Id).Oak_Timers'Unchecked_Access);

   function Scheduler_Info
     (Oak_Instance : access Oak_Data'Class)
      return access Oak_Scheduler_Info is (Oak_Instance.Scheduler'Access);

end Oak.Core;
