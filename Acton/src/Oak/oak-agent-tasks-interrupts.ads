with Oak.Processor_Support_Package.Interrupts;
use Oak.Processor_Support_Package.Interrupts;

package Oak.Agent.Tasks.Interrupts with Preelaborate is
   type Interrupt_Agent is new Task_Agent with private;

   type Interrupt_Type is (External, Missed_Deadline, Budget_Exhausted);
   procedure Interrupt_Run_Loop;

   procedure Set_External_Id
     (Agent : in out Interrupt_Agent'Class;
      Id    : in Oak_Interrupt_Id);

   procedure Set_Interrupt_Kind
     (Agent : in out Interrupt_Agent'Class;
      Kind  : in Interrupt_Type);

   procedure Set_Interrupt_Task
     (Agent : in out Interrupt_Agent'Class;
      T     : access Task_Agent'Class);

private
   type Interrupt_Agent is new Task_Agent with record
      Interrupt_Kind : Interrupt_Type;
      External_Id    : Oak_Interrupt_Id;
      Interrupt_Task : access Task_Agent'Class;
   end record;

end Oak.Agent.Tasks.Interrupts;
