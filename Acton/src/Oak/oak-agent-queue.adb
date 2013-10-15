package body Oak.Agent.Queue is
   procedure Add_Agent_Before
     (Queue     : in out Agent_Handler;
      Agent     : not null access Oak_Agent'Class;
      Before    : not null access Oak_Agent'Class;
      Queue_End : in Queue_End_Point := Head) is
   begin
      if Queue = null then
         Queue := Agent_Handler (Agent);
         Agent.Previous_Agent := Agent;
         Agent.Next_Agent     := Agent;
      else
         if Before = Queue and Queue_End = Head then
            Queue := Agent_Handler (Agent);
         end if;
         declare
            Next  : constant not null access Oak_Agent'Class := Before;
            Prior : constant not null access Oak_Agent'Class :=
                      Before.Previous_Agent;
         begin
            Agent.Next_Agent     := Next;
            Agent.Previous_Agent := Prior;

            Next.Previous_Agent := Agent;
            Prior.Next_Agent    := Agent;
         end;
      end if;
   end Add_Agent_Before;

   procedure Add_Agent_After
     (Queue : in out Agent_Handler;
      Agent : not null access Oak_Agent'Class;
      After : not null access Oak_Agent'Class)
   is
   begin
      if Queue = null then
         Queue := Agent_Handler (Agent);
         Agent.Previous_Agent := Agent;
         Agent.Next_Agent     := Agent;
      else
         declare
            Next  : constant not null access Oak_Agent'Class :=
                      After.Next_Agent;
            Prior : constant not null access Oak_Agent'Class := After;
         begin
            Agent.Next_Agent     := Next;
            Agent.Previous_Agent := Prior;

            Next.Previous_Agent := Agent;
            Prior.Next_Agent    := Agent;
         end;
      end if;
   end Add_Agent_After;

   procedure Add_Agent_To_Head
     (Queue : in out Agent_Handler;
      Agent : not null access Oak_Agent'Class) is
   begin
      if Queue = null then
         Queue                := Agent_Handler (Agent);
         Agent.Previous_Agent := Agent;
         Agent.Next_Agent     := Agent;
      else
         Add_Agent_Before (Queue  => Queue,
                           Agent  => Agent,
                           Before => Queue);
      end if;
   end Add_Agent_To_Head;

   procedure Add_Agent_To_Tail
     (Queue : in out Agent_Handler;
      Agent : not null access Oak_Agent'Class) is
   begin
      if Queue = null then
         Queue                := Agent_Handler (Agent);
         Agent.Previous_Agent := Agent;
         Agent.Next_Agent     := Agent;

      else
         Add_Agent_After (Queue => Queue,
                          Agent => Agent,
                          After => Queue.Previous_Agent);
      end if;
   end Add_Agent_To_Tail;

   procedure Move_Head_To_Tail (Queue : in out Agent_Handler) is
   begin
      Queue := Queue.Next_Agent;
   end Move_Head_To_Tail;

   procedure Remove_Agent
     (Queue : in out Agent_Handler;
      Agent : not null access Oak_Agent'Class) is
   begin
      if Agent = Agent.Next_Agent then
         Queue := null;
      else
         if Queue = Agent then
            Queue := Agent.Next_Agent;
         end if;
         declare
            Next  : constant not null access Oak_Agent'Class :=
                      Agent.Next_Agent;
            Prior : constant not null access Oak_Agent'Class :=
                      Agent.Previous_Agent;
         begin
            Next.Previous_Agent := Prior;
            Prior.Next_Agent    := Next;
         end;
      end if;
      Set_Blank_Agent_Link (Agent);
   end Remove_Agent;

   procedure Remove_Agent_From_Head (Queue : in out Agent_Handler) is
   begin
      Remove_Agent (Queue => Queue, Agent => Queue);
   end Remove_Agent_From_Head;

   procedure Remove_Agent_From_Tail (Queue : in out Agent_Handler) is
   begin
      Remove_Agent (Queue => Queue, Agent => Queue.Previous_Agent);
   end Remove_Agent_From_Tail;

   procedure Set_Blank_Agent_Link
     (A : not null access Oak_Agent'Class) is
   begin
      A.Next_Agent     := null;
      A.Previous_Agent := null;
   end Set_Blank_Agent_Link;

end Oak.Agent.Queue;
