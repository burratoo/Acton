with Oak.Oak_Time; use Oak.Oak_Time;
with Oak.Agent.Schedulers;
with Oak.Agent.Tasks;
with Oak.Core;

package body Oak.Timers is

   procedure Timer_Updated (Timer : access Oak_Timer'Class);

   procedure Add_Timer
     (Timer      : not null access Oak_Timer'Class;
      Timer_Info : not null access Oak_Timer_Info)
   is
      T             : access Oak_Timer'Class;
      Start_Of_List : constant access Oak_Timer'Class
        := Timer_Info.Timers (Timer.Priority);
   begin
      if Start_Of_List = null then
         Timer_Info.Timers (Timer.Priority) := Timer;
         Timer.Next_Timer                   := Timer;
         Timer.Previous_Timer               := Timer;
      else
         T := Start_Of_List;

         while Timer.Fire_Time > T.Fire_Time and then T /= Start_Of_List loop
            T := T.Next_Timer;
         end loop;

         Timer.Previous_Timer             := T.Previous_Timer;
         Timer.Next_Timer                 := T;
         Timer.Previous_Timer.Next_Timer  := Timer;
         Timer.Next_Timer.Previous_Timer  := Timer;

         if Timer.Fire_Time < Start_Of_List.Fire_Time then
            Timer_Info.Timers (Timer.Priority) := Timer;
         end if;
      end if;
   end Add_Timer;

   procedure Add_Timer_To_Current_Processor
     (Timer : not null access Oak_Timer'Class) is
   begin
      Timer.Add_Timer (Core.Oak_Timer_Store);
   end Add_Timer_To_Current_Processor;

   function Earliest_Timer_To_Fire
     (Timer_Info     : Oak_Timer_Info;
      Above_Priority : Any_Priority := Interrupt_Priority'First)
      return access Oak_Timer'Class
   is
      P     : Oak_Priority := Oak_Interrupt_Priority'Last;
      T     : access Oak_Timer'Class := null;
   begin
      for Timer of reverse Timer_Info.Timers loop
         if Timer /= null then
            if T = null then
               T := Timer;
            elsif Timer.Fire_Time < T.Fire_Time then
               T := Timer;
            end  if;
         end if;

         P := P - 1;
         exit when P = Above_Priority;
      end loop;
      return T;
   end Earliest_Timer_To_Fire;

   procedure Remove_Timer (Timer : not null access Oak_Timer'Class) is
      Manager : constant access Oak_Timer_Info := Timer.Timer_Manager;
   begin
      if Timer.Timer_Manager = null then
         return;
      end if;

      Timer.Timer_Manager := null;
      if Manager.Timers (Timer.Priority) = Timer then
         if Timer = Timer.Next_Timer then
            Manager.Timers (Timer.Priority) := null;
         else
            Manager.Timers (Timer.Priority) := Timer.Next_Timer;
         end if;
      end if;

      Timer.Previous_Timer.Next_Timer := Timer.Next_Timer;
      Timer.Next_Timer.Previous_Timer := Timer.Previous_Timer;
   end Remove_Timer;

   procedure Set_Timer
     (Timer     : in out Oak_Timer;
      Fire_Time : in Oak_Time.Time;
      Priority  : in Oak_Interrupt_Priority) is
   begin
      Timer.Fire_Time := Fire_Time;

      if not Timer.Is_Armed then
         Timer.Priority := Priority;
      else
         if Timer.Priority = Priority then
            Timer.Timer_Updated;
         else
            declare
               Manager : constant access Oak_Timer_Info := Timer.Timer_Manager;
            begin
               Timer.Remove_Timer;
               Timer.Priority := Priority;
               Timer.Add_Timer (Manager);
            end;
         end if;
      end if;

   end Set_Timer;

   procedure Set_Timer
     (Timer           : in out Action_Timer;
      Fire_Time       : in Oak_Time.Time := Oak_Time.Time_Last;
      Priority        : in Oak_Interrupt_Priority;
      Timer_Action    : in Ada.Cyclic_Tasks.Event_Action;
      Handler         : in Ada.Cyclic_Tasks.Action_Handler;
      Agent_To_Handle : access Oak.Agent.Tasks.Task_Agent'Class) is
   begin
      Timer.Set_Timer (Fire_Time, Priority);
      Timer.Timer_Action      := Timer_Action;
      Timer.Handler           := Handler;
      Timer.Agent_To_Handle   := Agent_To_Handle;
   end Set_Timer;

   procedure Set_Timer
     (Timer     : in out Scheduler_Timer;
      Fire_Time : in Oak_Time.Time := Oak_Time.Time_Last;
      Priority  : in Oak_Interrupt_Priority;
      Scheduler : not null access Oak.Agent.Schedulers.Scheduler_Agent'Class)
   is
   begin
      Timer.Set_Timer (Fire_Time, Priority);
      Timer.Scheduler := Scheduler;
   end Set_Timer;

   procedure Timer_Updated (Timer : access Oak_Timer'Class) is
      T, Start_Of_List : access Oak_Timer'Class;
   begin
      if not Timer.Is_Armed then
         return;
      end if;

      Start_Of_List := Timer.Timer_Manager.Timers (Timer.Priority);

      if Timer.Fire_Time > Timer.Next_Timer.Fire_Time then
         T := Timer.Next_Timer;

         --  Remove timer from current position

         Timer.Previous_Timer.Next_Timer := Timer.Next_Timer;
         Timer.Next_Timer.Previous_Timer := Timer.Previous_Timer;

         if Timer = Start_Of_List then
            Timer.Timer_Manager.Timers (Timer.Priority) := Timer.Next_Timer;
         end if;

         --  Find new position for the timer

         while Timer.Fire_Time > T.Fire_Time and then
           T /= Start_Of_List
         loop
            T := T.Next_Timer;
         end loop;

         Timer.Previous_Timer             := T.Previous_Timer;
         Timer.Next_Timer                 := T;
         Timer.Previous_Timer.Next_Timer  := Timer;
         Timer.Next_Timer.Previous_Timer  := Timer;

      elsif Timer.Fire_Time < Timer.Previous_Timer.Fire_Time then
         T := Timer.Previous_Timer;

         --  Remove timer from current position

         Timer.Previous_Timer.Next_Timer := Timer.Next_Timer;
         Timer.Next_Timer.Previous_Timer := Timer.Previous_Timer;

         --  Find new position for the timer

         while Timer.Fire_Time < T.Fire_Time loop
            T := T.Previous_Timer;
            if T /= Start_Of_List.Previous_Timer then
               Timer.Timer_Manager.Timers (Timer.Priority) := Timer;
               exit;
            end if;
         end loop;

         Timer.Previous_Timer             := T;
         Timer.Next_Timer                 := T.Next_Timer;
         Timer.Previous_Timer.Next_Timer  := Timer;
         Timer.Next_Timer.Previous_Timer  := Timer;
      end if;
   end Timer_Updated;

   procedure Update_Timer
     (Timer    : in out Oak_Timer'Class;
      New_Time : in Oak_Time.Time) is
   begin
      Timer.Fire_Time := New_Time;
      Timer.Timer_Updated;
   end Update_Timer;

   procedure Delay_Timer
     (Timer    : in out Oak_Timer'Class;
      Delay_To : in Oak_Time.Time_Span) is
   begin
      Timer.Fire_Time := Timer.Fire_Time + Delay_To;
      Timer.Timer_Updated;
   end Delay_Timer;

   function Agent_To_Handle (Timer : in out Action_Timer'Class)
     return Oak.Agent.Tasks.Task_Handler is (Timer.Agent_To_Handle);

   function Scheduler_Agent (Timer : in out Scheduler_Timer'Class) return
     access Oak.Agent.Schedulers.Scheduler_Agent is (Timer.Scheduler);
end Oak.Timers;
