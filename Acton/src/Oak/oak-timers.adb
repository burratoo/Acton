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
      if not In_Tree (Pool => Pool, Item_Id => Timer) then
         Insert_Node (Pool, Timer);
      end if;
   end Activate_Timer;

   ----------------------
   -- Deactivate_Timer --
   ----------------------

   procedure Deactivate_Timer (Timer : in Oak_Timer_Id) is
   begin
      if In_Tree (Pool => Pool, Item_Id => Timer) then
         Remove_Node (Pool, Timer);
      end if;
   end Deactivate_Timer;

   ------------------
   -- Delete_Timer --
   ------------------

   procedure Delete_Timer (Timer : in Oak_Timer_Id) is
   begin
      Delete_Item (Pool, Timer);
   end Delete_Timer;

   ----------------------------
   -- Earliest_Timer_To_Fire --
   ----------------------------

   function Earliest_Timer_To_Fire
     (Above_Priority : in Any_Priority := Oak_Priority'First)
      return Oak_Timer_Id
   is
      Timer : constant Oak_Timer_Id :=
                Find_Earliest_Item (Pool, Above_Priority);
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
      Activate  : in  Boolean := False)
   is
      New_Timer : constant Oak_Timer :=
                    (Fire_Time => Fire_Time,
                     Priority  => Priority,
                     Kind      => Empty_Timer);
   begin
      New_Item
        (Pool        => Pool,
         Item        => New_Timer,
         Item_Id     => Timer,
         Add_To_Tree => Activate);
   end New_Timer;

   procedure New_Event_Timer
     (Timer       : out Oak_Timer_Id;
      Priority    : in  Oak_Priority;
      Action_Data : in  Event_Timer_Data;
      Fire_Time   : in  Oak_Time.Time := Time_Last;
      Activate    : in  Boolean := False)
   is
      New_Timer : constant Oak_Timer :=
                    (Fire_Time => Fire_Time,
                     Priority  => Priority,
                     Kind      => Event_Timer,
                     Data      => Action_Data);
   begin
      New_Item
        (Pool        => Pool,
         Item        => New_Timer,
         Item_Id     => Timer,
         Add_To_Tree => Activate);
   end New_Event_Timer;

   procedure New_Event_Timer
     (Timer        : out Oak_Timer_Id;
      Priority     : in  Oak_Priority;
      Timer_Action : in  Ada.Cyclic_Tasks.Event_Response;
      Agent        : in  Oak_Agent_Id;
      Handler      : in  Ada.Cyclic_Tasks.Response_Handler := null;
      Fire_Time    : in  Oak_Time.Time := Time_Last;
      Activate     : in  Boolean := False)
   is

      New_Timer : constant Oak_Timer :=
                    (Fire_Time => Fire_Time,
                     Priority  => Priority,
                     Kind      => Event_Timer,
                     Data      =>
                       (Timer_Action     => Timer_Action,
                        Handler_Priority => Priority,
                        Agent_To_Handle  => Agent,
                        Handler          => Handler));
   begin
      New_Item
        (Pool        => Pool,
         Item        => New_Timer,
         Item_Id     => Timer,
         Add_To_Tree => Activate);
   end New_Event_Timer;

   procedure New_Scheduler_Timer
     (Timer     : out Oak_Timer_Id;
      Priority  : in  Oak_Priority;
      Scheduler : in  Scheduler_Id;
      Fire_Time : in  Oak_Time.Time := Time_Last;
      Activate  : in  Boolean := False)
   is
      New_Timer : constant Oak_Timer :=
                    (Fire_Time        => Fire_Time,
                     Priority         => Priority,
                     Kind             => Scheduler_Timer,
                     Scheduler        => Scheduler,
                     Scheduler_Action => Service);
   begin
      New_Item
        (Pool        => Pool,
         Item        => New_Timer,
         Item_Id     => Timer,
         Add_To_Tree => Activate);
   end New_Scheduler_Timer;

   ---------------------
   -- Set_Event_Timer --
   ---------------------

   procedure Set_Event_Timer
     (Timer       : in Oak_Timer_Id;
      Action_Data : in  Event_Timer_Data;
      Fire_Time   : in  Oak_Time.Time) is
   begin
      Replace_Item
        (Pool     => Pool,
         Item_Id  => Timer,
         Contents => (Kind       => Event_Timer,
                      Fire_Time  => Fire_Time,
                      Priority   => Action_Data.Handler_Priority,
                      Data       => Action_Data));
   end Set_Event_Timer;

   --------------------------
   -- Set_Timer_Event_Data --
   --------------------------

   procedure Set_Timer_Event_Data
     (Timer : in Oak_Timer_Id;
      Data  : Event_Timer_Data)
   is
   begin
      Replace_Item
        (Pool     => Pool,
         Item_Id  => Timer,
         Contents => (Kind       => Event_Timer,
                      Fire_Time  => Firing_Time (Timer),
                      Priority   => Priority (Timer),
                      Data       => Data));
   end Set_Timer_Event_Data;

   --------------------------------
   -- Set_Timer_Scheduler_Action --
   --------------------------------

   procedure Set_Timer_Scheduler_Action
     (Timer            : in Oak_Timer_Id;
      Scheduler        : in Scheduler_Id;
      Scheduler_Action : in Scheduler_Timer_Action)
   is
   begin
      Replace_Item
        (Pool     => Pool,
         Item_Id  => Timer,
         Contents => (Kind             => Scheduler_Timer,
                      Fire_Time        => Firing_Time (Timer),
                      Priority         => Priority (Timer),
                      Scheduler        => Scheduler,
                      Scheduler_Action => Scheduler_Action));
   end Set_Timer_Scheduler_Action;

   ------------------
   -- Update_Timer --
   ------------------

   procedure Update_Timer
     (Timer    : in Oak_Timer_Id;
      New_Time : in Oak_Time.Time)
   is
      procedure Set_Timer (T : in out Oak_Timer; Time : in Oak_Time.Time)
        with Inline;

      procedure Update_Timer is new Generic_Update_Time (Set_Timer);

      ---------------
      -- Set_Timer --
      ---------------

      procedure Set_Timer (T : in out Oak_Timer; Time : in Oak_Time.Time) is
      begin
         T.Fire_Time := Time;
      end Set_Timer;

   begin
      Update_Timer (Pool, Timer, New_Time);
   end Update_Timer;
end Oak.Timers;
