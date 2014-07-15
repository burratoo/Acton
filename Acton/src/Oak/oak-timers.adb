------------------------------------------------------------------------------
--                                                                          --
--                              OAK COMPONENTS                              --
--                                                                          --
--                                OAK.TIMERS                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                 Copyright (C) 2011-2014, Patrick Bernardi                --
------------------------------------------------------------------------------

package body Oak.Timers is

   --------------------
   -- Activate_Timer --
   --------------------

   procedure Activate_Timer (Timer : in Oak_Timer_Id) is
   begin
      if not In_Queue (Timer_Queue, Item_Id => Timer) then
         Enqueue_Item (Timer_Queue, Timer);
      end if;
   end Activate_Timer;

   ----------------------
   -- Deactivate_Timer --
   ----------------------

   procedure Deactivate_Timer (Timer : in Oak_Timer_Id) is
   begin
      if In_Queue (Timer_Queue, Item_Id => Timer) then
         Remove_Item (Timer_Queue, Timer);
      end if;
   end Deactivate_Timer;

   ------------------
   -- Delete_Timer --
   ------------------

   procedure Delete_Timer (Timer : in Oak_Timer_Id) is
   begin
      Deactivate_Timer (Timer);
      Deallocate_Item (Timer);
   end Delete_Timer;

   ----------------------------
   -- Earliest_Timer_To_Fire --
   ----------------------------

   function Earliest_Timer_To_Fire
     (Above_Priority : in Any_Priority := Oak_Priority'First)
      return Oak_Timer_Id
   is
      Timer : constant Oak_Timer_Id :=
                Find_Earliest_Item (Timer_Queue, Above_Priority);
   begin
      if Firing_Time (Timer) < Time_Last then
         return Timer;
      else
         return No_Timer;
      end if;
   end Earliest_Timer_To_Fire;

   ---------------
   -- New_Timer --
   ---------------

   procedure New_Timer
     (Timer     : out Oak_Timer_Id;
      Priority  : in  Oak_Priority;
      Fire_Time : in  Oak_Time.Time := Time_Last;
      Enable    : in  Boolean := False) is
   begin
      Allocate_An_Item (Timer);

      Timer_Pool (Timer) :=
        (Fire_Time => Fire_Time,
         Priority  => Priority,
         Kind      => Empty_Timer);

      if Enable then
         Enqueue_Item (Timer_Queue, Timer);
      end if;
   end New_Timer;

   procedure New_Event_Timer
     (Timer        : out Oak_Timer_Id;
      Priority     : in  Oak_Priority;
      Timer_Action : in  Ada.Cyclic_Tasks.Event_Response;
      Agent        : in  Oak_Agent_Id;
      Handler      : in  Ada.Cyclic_Tasks.Response_Handler := null;
      Fire_Time    : in  Oak_Time.Time := Time_Last;
      Enable       : in  Boolean := False) is
   begin
      Allocate_An_Item (Timer);

      Timer_Pool (Timer) :=
        (Fire_Time        => Fire_Time,
         Priority         => Priority,
         Kind             => Event_Timer,
         Timer_Action     => Timer_Action,
         Agent_To_Handle  => Agent,
         Event_Handler    => Handler);

      if Enable then
         Enqueue_Item (Timer_Queue, Timer);
      end if;
   end New_Event_Timer;

   procedure New_Scheduler_Timer
     (Timer     : out Oak_Timer_Id;
      Priority  : in  Oak_Priority;
      Scheduler : in  Scheduler_Id;
      Fire_Time : in  Oak_Time.Time := Time_Last;
      Enable    : in  Boolean := False) is
   begin
      Allocate_An_Item (Timer);

      Timer_Pool (Timer) :=
        (Fire_Time        => Fire_Time,
         Priority         => Priority,
         Kind             => Scheduler_Timer,
         Scheduler        => Scheduler);

      if Enable then
         Enqueue_Item (Timer_Queue, Timer);
      end if;
   end New_Scheduler_Timer;

   ------------------
   -- Setup_Timers --
   ------------------

   procedure Setup_Timers is
   begin
      Setup_Storage;
      Timer_Pool (No_Timer) :=
        (Kind      => Empty_Timer,
         Fire_Time => Time_Last,
         Priority  => Oak_Priority'First);
   end Setup_Timers;

   ------------------
   -- Update_Timer --
   ------------------

   procedure Update_Timer
     (Timer    : in Oak_Timer_Id;
      New_Time : in Oak_Time.Time) is
   begin
      if In_Queue (Timer_Queue, Item_Id => Timer) then
         Remove_Item (Timer_Queue, Timer);
         Timer_Pool (Timer).Fire_Time := New_Time;
         Enqueue_Item (Timer_Queue, Timer);
      else
         Timer_Pool (Timer).Fire_Time := New_Time;
      end if;
   end Update_Timer;
end Oak.Timers;
