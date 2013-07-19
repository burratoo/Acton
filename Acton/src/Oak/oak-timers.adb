with Oak.Agent.Schedulers;
with Oak.Agent.Tasks;
with Oak.Core;

package body Oak.Timers is

   procedure Timer_Updated (Timer : not null access Oak_Timer'Class);

   procedure Add_Timer
     (Timer      : not null access Oak_Timer'Class;
      Timer_Info : not null access Oak_Timer_Info)
   is
      T             : access Oak_Timer'Class;
      Start_Of_List : constant access Oak_Timer'Class
        := Timer_Info.Timers (Timer.Priority);
   begin
      Timer.Timer_Manager := Timer_Info;

      if Start_Of_List = null then
         Timer_Info.Timers (Timer.Priority) := Timer;
         Timer.Next_Timer                   := Timer;
         Timer.Previous_Timer               := Timer;
      else
         T := Start_Of_List;

         while Timer.Fire_Time > T.Fire_Time loop
            T := T.Next_Timer;
            exit when T = Start_Of_List;
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
      Above_Priority : Any_Priority := Interrupt_Priority'First - 1)
      return access Oak_Timer'Class
   is
      P     : Oak_Priority := Oak_Priority'Last;
      T     : access Oak_Timer'Class := null;
   begin
      for Timer of reverse Timer_Info.Timers loop
         if Timer /= null then
            if T = null then
               T := Timer;
            elsif Timer.Fire_Time < T.Fire_Time then
               T := Timer;
            end if;
         end if;

         P := P - 1;
         exit when P = Above_Priority;
      end loop;
      return T;
   end Earliest_Timer_To_Fire;

   procedure Remove_Timer (Timer : not null access Oak_Timer'Class) is
   begin
      if Timer.Timer_Manager = null then
         return;
      end if;

      if Timer.Timer_Manager.Timers (Timer.Priority) = Timer then
         if Timer = Timer.Next_Timer then
            Timer.Timer_Manager.Timers (Timer.Priority) := null;
         else
            Timer.Timer_Manager.Timers (Timer.Priority) := Timer.Next_Timer;
         end if;
      end if;

      Timer.Timer_Manager := null;
      Timer.Previous_Timer.Next_Timer := Timer.Next_Timer;
      Timer.Next_Timer.Previous_Timer := Timer.Previous_Timer;
   end Remove_Timer;

   procedure Set_Timer
     (Timer     : in out Oak_Timer;
      Fire_Time : in Oak_Time.Time;
      Priority  : in Oak_Priority) is
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
      Priority        : in Oak_Priority;
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
      Priority  : in Oak_Priority;
      Scheduler : not null access Oak.Agent.Schedulers.Scheduler_Agent'Class)
   is
   begin
      Timer.Set_Timer (Fire_Time, Priority);
      Timer.Scheduler        := Scheduler;
      Timer.Deferrable_Timer := No;
   end Set_Timer;

   procedure Set_Timer_Deferrable_Behaviour
     (Timer      : not null access Scheduler_Timer'Class;
      Timer_Info : not null access Oak_Timer_Info;
      Defer_Kind : in Deferrable_Type) is
   begin
      if Defer_Kind = No then
         Set_Timer_To_Not_Be_Deferrable (Timer => Timer);
      else
         Set_Timer_To_Be_Deferrable
           (Timer      => Timer,
            Timer_Info => Timer_Info,
            Defer_Kind => Defer_Kind);
      end if;
   end Set_Timer_Deferrable_Behaviour;

   procedure Set_Timer_To_Be_Deferrable
     (Timer      : not null access Scheduler_Timer'Class;
      Timer_Info : not null access Oak_Timer_Info;
      Defer_Kind : in Deferrable_Type)
   is
      List : access Scheduler_Timer'Class renames
               Timer_Info.Timers_Delayed_By_Execution;
   begin
      Timer.Deferrable_Timer := Defer_Kind;

      if Timer.Next_Scheduler_Timer /= null then
         return;
      end if;

      if List = null then
         List                           := Timer;
         Timer.Next_Scheduler_Timer     := Timer;
         Timer.Previous_Scheduler_Timer := Timer;
      else
         List.Previous_Scheduler_Timer.Next_Scheduler_Timer := Timer;
         List.Previous_Scheduler_Timer := Timer;

         Timer.Previous_Scheduler_Timer := List.Previous_Scheduler_Timer;
         Timer.Next_Scheduler_Timer     := List;

         List := Timer;
      end if;
   end Set_Timer_To_Be_Deferrable;

   procedure Set_Timer_To_Not_Be_Deferrable
     (Timer : not null access Scheduler_Timer'Class)
   is
      List : access Scheduler_Timer'Class renames
               Timer.Timer_Manager.Timers_Delayed_By_Execution;
   begin
      Timer.Deferrable_Timer := No;

      if Timer.Next_Scheduler_Timer = null then
         return;
      end if;

      if List.Next_Scheduler_Timer = List then
         List.Next_Scheduler_Timer     := null;
         List.Previous_Scheduler_Timer := null;
         List                          := null;
      else
         Timer.Previous_Scheduler_Timer.Next_Scheduler_Timer :=
           Timer.Next_Scheduler_Timer;
         Timer.Next_Scheduler_Timer.Previous_Scheduler_Timer :=
           Timer.Previous_Scheduler_Timer;

         if List = Timer then
            List := Timer.Next_Scheduler_Timer;
         end if;

         Timer.Next_Scheduler_Timer     := null;
         Timer.Previous_Scheduler_Timer := null;
      end if;

   end Set_Timer_To_Not_Be_Deferrable;

   procedure Timer_Updated (Timer : not null access Oak_Timer'Class) is
   begin
      if not Timer.Is_Armed then
         return;
      end if;

      declare
         T             : not null access Oak_Timer'Class := Timer;
         Start_Of_List : not null access Oak_Timer'Class renames
                           Timer.Timer_Manager.Timers (Timer.Priority);
      begin
         if Timer /= Start_Of_List.Previous_Timer
           and then Timer.Fire_Time > Timer.Next_Timer.Fire_Time
         then
            --  Remove timer from current position

            Timer.Previous_Timer.Next_Timer := Timer.Next_Timer;
            Timer.Next_Timer.Previous_Timer := Timer.Previous_Timer;

            if Timer = Start_Of_List then
               Start_Of_List := Timer.Next_Timer;
            end if;

            T := Timer.Next_Timer;

            --  Find new position for the timer

            while Timer.Fire_Time > T.Fire_Time loop
               T := T.Next_Timer;
               exit when T = Start_Of_List;
            end loop;

            Timer.Previous_Timer             := T.Previous_Timer;
            Timer.Next_Timer                 := T;
            Timer.Previous_Timer.Next_Timer  := Timer;
            Timer.Next_Timer.Previous_Timer  := Timer;

         elsif Timer /= Start_Of_List
           and then Timer.Fire_Time < Timer.Previous_Timer.Fire_Time then
            --  Remove timer from current position

            Timer.Previous_Timer.Next_Timer := Timer.Next_Timer;
            Timer.Next_Timer.Previous_Timer := Timer.Previous_Timer;

            --  Find new position for the timer
            T := Timer.Previous_Timer;

            while Timer.Fire_Time < T.Fire_Time loop
               T := T.Previous_Timer;
               exit when T = Start_Of_List.Previous_Timer;
            end loop;

            Timer.Previous_Timer             := T;
            Timer.Next_Timer                 := T.Next_Timer;
            Timer.Previous_Timer.Next_Timer  := Timer;
            Timer.Next_Timer.Previous_Timer  := Timer;

            if Timer.Fire_Time < Start_Of_List.Fire_Time then
               Start_Of_List := Timer;
            end if;

         end if;
      end;
   end Timer_Updated;

   procedure Update_Timer
     (Timer    : in out Oak_Timer'Class;
      New_Time : in Oak_Time.Time) is
   begin
      Timer.Fire_Time := New_Time;
      Timer_Updated (Timer'Unchecked_Access);
   end Update_Timer;

   procedure Delay_Timer
     (Timer    : in out Oak_Timer'Class;
      Delay_By : in Oak_Time.Time_Span) is
   begin
      Timer.Fire_Time := Timer.Fire_Time + Delay_By;
      Timer_Updated (Timer'Unchecked_Access);
   end Delay_Timer;

   procedure Delay_Delayable_Timers
     (Timer_Info     : in Oak_Timer_Info;
      Delay_By       : in Oak_Time.Time_Span;
      Below_Priority : in Oak_Priority)
   is
      List  : access Scheduler_Timer'Class renames
                Timer_Info.Timers_Delayed_By_Execution;
      Timer : access Scheduler_Timer'Class;
   begin
      if List /= null then
         Timer := List;
         loop
            if Timer.Deferrable_Timer = Yes
              or else (Timer.Deferrable_Timer = Only_By_Higher_Priority_Tasks
              and then Timer.Priority < Below_Priority)
            then
               Timer.Fire_Time := Timer.Fire_Time + Delay_By;
            end if;

            Timer := Timer.Next_Scheduler_Timer;
            exit when Timer = List;
         end loop;
      end if;
   end Delay_Delayable_Timers;

   function Agent_To_Handle (Timer : in out Action_Timer'Class)
     return Oak.Agent.Agent_Handler is (Timer.Agent_To_Handle);

end Oak.Timers;
