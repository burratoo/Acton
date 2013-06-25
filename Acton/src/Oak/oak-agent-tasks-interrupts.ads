with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;
with Oak.Timers; use Oak.Timers;

package Oak.Agent.Tasks.Interrupts with Preelaborate is
   type Interrupt_Agent is new Task_Agent with private
     with Preelaborable_Initialization;

   type Interrupt_Type is (External, Timer_Action);
   procedure Interrupt_Run_Loop;

   function Interrupt_Kind (Agent : in out Interrupt_Agent'Class)
                            return Interrupt_Type;

   procedure Set_External_Id
     (Agent : in out Interrupt_Agent'Class;
      Id    : in Oak_Interrupt_Id);

   procedure Set_Interrupt_Kind
     (Agent : in out Interrupt_Agent'Class;
      Kind  : in Interrupt_Type);

   procedure Set_Timer_To_Handle
     (Agent : in out Interrupt_Agent'Class;
      Timer : access Action_Timer);

   function Timer_To_Handle (Agent : in out Interrupt_Agent'Class)
                             return access Action_Timer'Class;

--     procedure Set_Interrupt_Task
--       (Agent : in out Interrupt_Agent'Class;
--        T     : access Task_Agent'Class);

private
   type Interrupt_Agent is new Task_Agent with record
      Interrupt_Kind  : Interrupt_Type;
      External_Id     : Oak_Interrupt_Id;
      Timer_To_Handle : access Action_Timer'Class;
      Interrupt_Task  : access Task_Agent'Class;
   end record;

   function Interrupt_Kind (Agent : in out Interrupt_Agent'Class)
     return Interrupt_Type is (Agent.Interrupt_Kind);

   function Timer_To_Handle (Agent : in out Interrupt_Agent'Class)
     return access Action_Timer'Class is (Agent.Timer_To_Handle);

end Oak.Agent.Tasks.Interrupts;
