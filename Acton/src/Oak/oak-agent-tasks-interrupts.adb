with Oak.Core;
with Oak.Core_Support_Package.Task_Support;

package body Oak.Agent.Tasks.Interrupts is
   procedure Interrupt_Run_Loop is
      Exit_Message : constant Oak.Agent.Tasks.Oak_Task_Message :=
                  (Message_Type => Oak.Agent.Tasks.Interrupt_Done);
      T            : constant access Interrupt_Agent :=
                       Interrupt_Agent (Oak.Core.Current_Task.all)'Access;
   begin
      loop
         case T.Interrupt_Kind is
            when External =>
               External_Interrupt_Handler (T.External_Id);
            when Missed_Deadline =>
               null;
            when Budget_Exhausted =>
               null;
         end case;

         Core.Current_Task.Store_Oak_Task_Message (Exit_Message);
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

end Oak.Agent.Tasks.Interrupts;
