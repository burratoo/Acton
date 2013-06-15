with Oak.Oak_Time; use Oak.Oak_Time;
with Oak.Memory.Call_Stack.Ops;

package body Oak.Agent is

   Global_Task_Id : Task_Id := 1;

   function New_Task_Id return Task_Id;

   procedure Initialise_Agent
     (Agent      : access Oak_Agent'Class;
      Name       : in String;
      Call_Stack : in Call_Stack_Handler) is
   begin
      Agent.Name_Length                   :=
         Natural'Min (Task_Name'Length, Name'Length);
      Agent.Name (1 .. Agent.Name_Length) :=
        Name (Name'First .. Name'First + Agent.Name_Length - 1);

      Agent.Id         := New_Task_Id;
      Agent.Call_Stack := Call_Stack;
   end Initialise_Agent;

   procedure Initialise_Agent
     (Agent           : access Oak_Agent'Class;
      Name            : in String;
      Call_Stack_Size : in System.Storage_Elements.Storage_Count) is
   begin
      Agent.Name_Length                   :=
         Natural'Min (Task_Name'Length, Name'Length);
      Agent.Name (1 .. Agent.Name_Length) :=
        Name (Name'First .. Name'First + Agent.Name_Length - 1);

      Agent.Id         := New_Task_Id;
      Oak.Memory.Call_Stack.Ops.Allocate_Call_Stack
        (Stack            => Agent.Call_Stack,
         Size_In_Elements => Call_Stack_Size);
   end Initialise_Agent;

   procedure Charge_Execution_Time
     (To_Agent  : access Oak_Agent;
      Exec_Time : in Oak_Time.Time_Span) is
   begin
      To_Agent.Current_Execution_Time :=
        To_Agent.Current_Execution_Time + Exec_Time;
      To_Agent.Total_Execution_Time :=
        To_Agent.Total_Execution_Time + Exec_Time;
   end Charge_Execution_Time;

   procedure New_Execution_Cycle (Agent : in out Oak_Agent'Class) is
   begin
      Agent.Execution_Cycles := Agent.Execution_Cycles + 1;
      if Agent.Current_Execution_Time > Agent.Max_Execution_Time then
         Agent.Max_Execution_Time := Agent.Current_Execution_Time;
      end if;
      Agent.Current_Execution_Time := Oak_Time.Time_Span_Zero;
   end New_Execution_Cycle;

   function New_Task_Id return Task_Id is
      Chosen_Id : constant Task_Id := Global_Task_Id;
   begin
      Global_Task_Id := Global_Task_Id + 1;
      return Chosen_Id;
   end New_Task_Id;

   procedure Set_Stack_Pointer
     (Agent         : in out Oak_Agent'Class;
      Stack_Pointer : in System.Address) is
   begin
      Agent.Call_Stack.Pointer := Stack_Pointer;
   end Set_Stack_Pointer;

end Oak.Agent;
