with Ada.Unchecked_Conversion;
with Ada.Task_Identification;
with Oak.Core;
with Oak.Core_Support_Package.Task_Support;
with Oak.Core_Support_Package.Call_Stack;
use Oak.Core_Support_Package.Call_Stack;

package body Oak.Agent.Interrupts is

   function To_Task_Id is
     new Ada.Unchecked_Conversion
       (Agent_Handler, Ada.Task_Identification.Task_Id);

   procedure Initialise_Interrupt_Agent
     (Agent    : not null access Interrupt_Agent'Class;
      Priority : in Oak_Priority) is
   begin
      Initialise_Agent
        (Agent              => Agent,
         Name               => "Interrupt_Agent",
         Call_Stack_Address => Null_Address,
         Call_Stack_Size    => Interrupt_Stack_Size,
         Run_Loop           => Interrupt_Run_Loop'Address,
         Run_Loop_Parameter => Agent.all'Address,
         Normal_Priority    => Priority,
         Initial_State      => Interrupt_Done,
         Wake_Time          => Oak_Time.Time_Last);
   end Initialise_Interrupt_Agent;

   procedure Interrupt_Run_Loop (Self : Interrupt_Agent'Class) is
      Exit_Message : constant Oak_Message :=
                  (Message_Type => Interrupt_Done);
   begin
      loop
         case Self.Interrupt_Kind is
            when External =>
               External_Interrupt_Handler (Self.External_Id);
            when Timer_Action =>
               Self.Timer_To_Handle.Handler.all
                 (To_Task_Id (Self.Timer_To_Handle.Agent_To_Handle));
         end case;

         Core.Current_Agent.Set_Agent_Message (Exit_Message);
         Core_Support_Package.Task_Support.Yield_Processor_To_Kernel;
      end loop;
   end Interrupt_Run_Loop;

   procedure Set_External_Id
     (Agent : in out Interrupt_Agent'Class;
      Id    : in Oak_Interrupt_Id) is
   begin
      Agent.External_Id := Id;
   end Set_External_Id;

   procedure Set_Interrupt_Kind
     (Agent : in out Interrupt_Agent'Class;
      Kind  : in Interrupt_Type) is
   begin
      Agent.Interrupt_Kind := Kind;
   end Set_Interrupt_Kind;

   procedure Set_Timer_To_Handle
     (Agent : in out Interrupt_Agent'Class;
      Timer : access Action_Timer) is
   begin
      Agent.Timer_To_Handle := Timer;
   end Set_Timer_To_Handle;

end Oak.Agent.Interrupts;
