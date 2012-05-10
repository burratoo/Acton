package body Oak.Agent is

   Global_Task_Id : Task_Id := 1;

   function New_Task_Id return Task_Id;

   procedure Initialise_Agent
     (Agent      : in out Oak_Agent'Class;
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

   function New_Task_Id return Task_Id is
      Chosen_Id : constant Task_Id := Global_Task_Id;
   begin
      Global_Task_Id := Global_Task_Id + 1;
      return Chosen_Id;
   end New_Task_Id;

end Oak.Agent;
