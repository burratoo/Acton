with Ada.Unchecked_Conversion;
with Ada.Task_Identification;
with Oak.Core;
with Oak.Core_Support_Package.Task_Support;

package body Oak.Agent.Tasks.Interrupts is

   function To_Task_Id is
     new Ada.Unchecked_Conversion
       (Task_Handler, Ada.Task_Identification.Task_Id);

   procedure Interrupt_Run_Loop is
      Exit_Message : constant Oak_Message :=
                  (Message_Type => Interrupt_Done);
      T            : constant access Interrupt_Agent :=
                       Interrupt_Agent (Oak.Core.Current_Task.all)'Access;
   begin
      loop
         case T.Interrupt_Kind is
            when External =>
               External_Interrupt_Handler (T.External_Id);
            when Timer_Action =>
               T.Timer_To_Handle.Handler.all
                 (To_Task_Id (T.Timer_To_Handle.Agent_To_Handle));
         end case;

         Core.Current_Task.Set_Agent_Message (Exit_Message);
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

end Oak.Agent.Tasks.Interrupts;
