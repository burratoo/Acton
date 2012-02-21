with Oak.Processor_Support_Package; use Oak.Processor_Support_Package;
with Oak.Memory;                    use Oak.Memory;
with Ada.Real_Time;                 use Ada.Real_Time;
with Oak.Memory.Call_Stack;         use Oak.Memory.Call_Stack;

with System; use System;

package Oak.Oak_Task is

   pragma Preelaborate;

   type Oak_Task_Kind is (Regular, Scheduler);

   --  Protected entry constants and types
   --  A lot of this could possibly move to Oak.Protected_Object.
   No_Entry     : constant := 0;
   Single_Entry : constant := 1;
   Max_Entry    : constant := Processor_Support_Package.Max_Entries;

   type Entry_Index is range No_Entry .. Max_Entry;

   type Oak_Task (Num_Entries : Entry_Index   := No_Entry;
                  Kind        : Oak_Task_Kind := Regular) is limited private;
   type Oak_Task_Handler is access all Oak_Task;

   type Task_Id is range 0 .. Max_Tasks;
   subtype Task_Name is String
     (1 .. Processor_Support_Package.Max_Task_Name_Length);

   type Task_State is (
                       Bad_State,                   -- 0
                       Activation_Pending,          -- 1
                       Activation_Failed,           -- 2
                       Activation_Successful,       -- 3
                       Activation_Complete,         -- 4
                       Running,                     -- 5
                       Runnable,                    -- 6
                       Sleeping,                    -- 7
                       Waiting,                     -- 8
                       Inactive,                    -- 9
                       Shared_State,                -- 10
                       Cycle_Completed,             -- 11
                       Change_Cycle_Period,         -- 12
                       Change_Relative_Deadline,    -- 13
                       Terminated,                  -- 14
                       Entering_PO,                 -- 15
                       Enter_PO_Refused,            -- 16
                       Exiting_PO,                  -- 17
                       Exit_PO_Error,               -- 18
                       Waiting_On_Protected_Object, -- 19
                       No_State);                   -- 20

   type Shared_Task_State is access all Task_State;
   No_Shared_State : constant Shared_Task_State
     := null;

   type Memory_Region;
   type Memory_Region_Link is access all Memory_Region;
   type Memory_Permission is (Read_Only, Read_Write);

   type Memory_Region is record
      Location : Memory_Slice;
      Next     : Memory_Region_Link;
      Previous : Memory_Region_Link;
   end record;

   type Task_Link_Element is record
      Next     : Oak_Task_Handler := null;
      Previous : Oak_Task_Handler := null;
   end record;

   Blank_Link : constant Task_Link_Element := (Next => null,
                                               Previous => null);

   type Reason_For_Run is (
                           Task_Yield,
                           Select_Next_Task,
                           Add_Task,
                           Remove_Task);

   type Boolean_Access is access all Boolean;

   type Activation_Chain is limited private;

   type Activation_Chain_Access is access all Activation_Chain;

   Unspecified_Priority : constant Integer := -1;

   type Entry_Queue_Array is array (Entry_Index range <>) of Oak_Task_Handler;
   type Entry_Barrier_State is (Closed, Open);
   type Entry_Barrier_Array is array (Entry_Index range <>) of
     Entry_Barrier_State;
   type Entry_Barrier_Wrapper (Array_Length : Entry_Index) is record
      State : Entry_Barrier_Array (1 .. Array_Length);
   end record;
   type Entry_Barrier_Handler is access all Entry_Barrier_Wrapper;

   type Protected_Subprogram_Type is
     (Protected_Function,
      Protected_Procedure,
      Protected_Entry);

   type Oak_Task_Message (Message_Type : Task_State := No_State) is record
      case Message_Type is
         when Sleeping =>
            Wake_Up_At : Time := Time_Last;
         when Change_Cycle_Period =>
            New_Cycle_Period : Time_Span := Time_Span_Zero;
         when Change_Relative_Deadline =>
            New_Deadline_Span : Time_Span := Time_Span_Zero;
         when Entering_PO =>
            PO_Enter         : Oak_Task_Handler := null;
            Subprogram_Kind  : Protected_Subprogram_Type := Protected_Function;
            Entry_Id_Enter   : Entry_Index := No_Entry;
         when Exiting_PO =>
            PO_Exit           : Oak_Task_Handler := null;
            Barrier_Exception : Boolean := False;
         when others =>
            null;
      end case;
   end record;

   type Oak_Task_Message_Store is record
      Task_Yielded : Boolean;
      Message      : Oak_Task_Message;
   end record;

   type Oak_Task_Message_Location is access all Oak_Task_Message_Store;

   --  Empty_Task_Request : constant Task_Requested_State
   --  := (State => No_State);

   -----------------
   --  Not sure if I will need this procedure or not. Mainly this is due to
   --  the the memory structure of the tasks could be defined during
   --  compliation and simply loaded into memory.
   --  This would be done by declaring a task like My_Great_Task : Oak_Task :=
   --  (Id=> .., Name=> .., etc)
   ------------
   --     procedure New_Task
   --       (My_Name         : Task_Name;
   --        My_Priority     : Priority;
   --        My_Deadline     : Time;
   --        My_Cycle_Period : Time;
   --        My_Start_Delay  : Time);

private

   Global_Task_Id : Task_Id := 1;

   type Oak_Task (Num_Entries : Entry_Index   := No_Entry;
                  Kind        : Oak_Task_Kind := Regular) is record
      Id          : Task_Id := Task_Id'Last;
      Name        : Task_Name;
      Name_Length : Natural := 0;

      ----
      --  This gives us a pointer to the starting location of the Stack (is
      --  this useful?) and the size of the stack.
      --  We also need to store the stack pointer. Its is probably useful to
      --  store the stack pointer in its own variable in the OTCR. But why?
      --  usually it is stored in a register anyway.
      -----
      Call_Stack : Call_Stack_Handler;
      Run_Loop   : Address := Null_Address;

      Activation_List : Oak_Task_Handler := null;
      Elaborated      : Boolean_Access   := null;

      Memory_List : Memory_Region_Link := null;
      --      Registers   : Register_Store;    --  Um, this can Just go onto
      --  the stack.
      --  Duh! Actually maybe we do...
      --  Actually we don't. Registers can
      --  simple be pushed onto the top of the
      --  stack when a task losses its context
      --  and poped off when it regains.
      --  need it...
      Controlling_Shared_State  : aliased Task_State :=  Waiting;
      Message_Location          : Oak_Task_Message_Location := null;

      case Kind is
         when Regular =>
            State           : Task_State   := Sleeping;
            Shared_State    : Shared_Task_State := No_Shared_State;
            Normal_Priority : Any_Priority := Default_Priority;
            Deadline        : Time_Span    := Time_Span_Zero;
            Cycle_Period    : Time_Span    := Time_Span_Zero;
            Phase           : Time_Span    := Time_Span_Zero;

            Next_Deadline  : Time := Time_Last;
            Next_Run_Cycle : Time := Time_Last;
            Wake_Time      : Time := Time_Last;

            Scheduler_Agent : Oak_Task_Handler := null;
            Queue_Link : Task_Link_Element;
            Deadline_List   : Task_Link_Element;

            Is_Protected_Object    : Boolean := False;
            Tasks_Within           : Oak_Task_Handler := null;
            Active_Subprogram_Kind : Protected_Subprogram_Type
              := Protected_Function;
            Entry_Queues           : Entry_Queue_Array (1 .. Num_Entries)
              := (others => null);
            Entry_Barriers         : Entry_Barrier_Handler := null;

         when Scheduler =>
            --  Scheduler Agents fields.
            Lowest_Prioirty, Highest_Prioirty : Any_Priority;
            Task_To_Run                       : Oak_Task_Handler := null;
            Manage_Task                       : Oak_Task_Handler := null;
            Desired_Agent_Run_Time            : Time             := Time_Last;
            Run_Reason                        : Reason_For_Run   :=
                                                  Select_Next_Task;

            Next_Agent : Oak_Task_Handler := null;
      end case;
   end record;

   type Activation_Chain is limited record
      Head : Oak_Task_Handler := null;
   end record;

end Oak.Oak_Task;
