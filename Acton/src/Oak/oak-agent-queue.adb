package body Oak.Agent.Queue is
   procedure Add_Agent_Before
     (Queue     : in out Agent_Handler;
      Agent     : access Agent_Type'Class;
      Before    : access Agent_Type'Class;
      Queue_End : in Queue_End_Point := Head) is
   begin
      if Queue = null then
         Queue := Agent_Handler (Agent);
         Set_Queue_Link (Agent => Agent,
                         Prev  => Agent,
                         Next  => Agent);
      else
         if Before = Queue and Queue_End = Head then
            Queue := Agent_Handler (Agent);
         end if;
         declare
            Next  : constant access Agent_Type'Class := Before;
            Prior : constant access Agent_Type'Class := Prev_Agent (Before);
         begin
            Set_Queue_Link (Agent => Agent,
                            Next  => Next,
                            Prev  => Prior);
            Set_Prev_Agent (Agent => Next,
                            Prev  => Agent);
            Set_Next_Agent (Agent => Prior,
                            Next  => Agent);
         end;
      end if;
   end Add_Agent_Before;

   procedure Add_Agent_After
     (Queue : in out Agent_Handler;
      Agent : access Agent_Type'Class;
      After : access Agent_Type'Class)
   is
   begin
      if Queue = null then
         Queue := Agent_Handler (Agent);
         Set_Queue_Link (Agent => Agent,
                         Prev  => Agent,
                         Next  => Agent);
      else
         declare
            Next  : constant access Agent_Type'Class := Next_Agent (After);
            Prior : constant access Agent_Type'Class := After;
         begin
            Set_Queue_Link (Agent => Agent,
                            Next  => Next,
                            Prev  => Prior);
            Set_Prev_Agent (Agent => Next,
                            Prev  => Agent);
            Set_Next_Agent (Agent => Prior,
                            Next  => Agent);
         end;
      end if;
   end Add_Agent_After;

   procedure Add_Agent_To_Head
     (Queue : in out Agent_Handler;
      Agent : access Agent_Type'Class) is
   begin
      Add_Agent_Before (Queue  => Queue,
                       Agent  => Agent,
                       Before => Queue);
   end Add_Agent_To_Head;

   procedure Add_Agent_To_Tail
     (Queue : in out Agent_Handler;
      Agent : access Agent_Type'Class) is
   begin
      if Queue = null then
         Add_Agent_After (Queue => Queue,
                         Agent => Agent,
                         After => null);
      else
         Add_Agent_After (Queue => Queue,
                         Agent => Agent,
                         After => Prev_Agent (Queue));
      end if;
   end Add_Agent_To_Tail;

   procedure Remove_Agent
     (Queue : in out Agent_Handler;
      Agent : access Agent_Type'Class) is
   begin
      if Agent = Next_Agent (Agent) then
         Queue := null;
      else
         if Queue = Agent then
            Queue := Next_Agent (Agent);
         end if;
         declare
            Next  : constant Agent_Handler := Next_Agent (Agent);
            Prior : constant Agent_Handler := Prev_Agent (Agent);
         begin
            Set_Prev_Agent (Agent => Next, Prev => Prior);
            Set_Next_Agent (Agent => Prior, Next => Next);
         end;
      end if;
      Set_Blank_Link (Agent);
   end Remove_Agent;

   procedure Remove_Agent_From_Head (Queue : in out Agent_Handler) is
   begin
      Remove_Agent (Queue => Queue, Agent => Queue);
   end Remove_Agent_From_Head;

   procedure Remove_Agent_From_Tail (Queue : in out Agent_Handler) is
   begin
      Remove_Agent (Queue => Queue, Agent => Prev_Agent (Queue));
   end Remove_Agent_From_Tail;
end Oak.Agent.Queue;
