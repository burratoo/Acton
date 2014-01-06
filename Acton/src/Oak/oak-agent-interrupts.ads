with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;
with Oak.Timers; use Oak.Timers;

package Oak.Agent.Interrupts with Preelaborate is
   type Interrupt_Agent is new Oak_Agent with private
     with Preelaborable_Initialization;

   type Interrupt_Type is (External, Timer_Action);

   procedure Initialise_Interrupt_Agent
     (Agent    : not null access Interrupt_Agent'Class;
      Priority : in Oak_Priority);

   procedure Interrupt_Run_Loop (Self : Interrupt_Agent'Class);

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
      Timer : access Event_Timer);

   function Timer_To_Handle (Agent : in out Interrupt_Agent'Class)
                             return access Event_Timer'Class;

--     procedure Set_Interrupt_Task
--       (Agent : in out Interrupt_Agent'Class;
--        T     : access Task_Agent'Class);

private
   type Interrupt_Agent is new Oak_Agent with record
      Interrupt_Kind  : Interrupt_Type;
      External_Id     : Oak_Interrupt_Id;
      Timer_To_Handle : access Event_Timer'Class;
   end record;

   function Interrupt_Kind (Agent : in out Interrupt_Agent'Class)
     return Interrupt_Type is (Agent.Interrupt_Kind);

   function Timer_To_Handle (Agent : in out Interrupt_Agent'Class)
     return access Event_Timer'Class is (Agent.Timer_To_Handle);

end Oak.Agent.Interrupts;
