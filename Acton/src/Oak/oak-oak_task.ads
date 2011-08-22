with Oak.Processor_Support_Package; use Oak.Processor_Support_Package;
with Oak.Memory;                    use Oak.Memory;
with Ada.Real_Time;                 use Ada.Real_Time;
with Oak.Memory.Call_Stack;         use Oak.Memory.Call_Stack;

with System; use System;

package Oak.Oak_Task is

   pragma Preelaborate;

   type Oak_Task is limited private;
   type Oak_Task_Handler is access all Oak_Task;

   type Task_Id is range 0 .. Max_Tasks;
   subtype Task_Name is String (
      1 .. Processor_Support_Package.Max_Task_Name_Length);

   type Task_State is (
      Activation_Pending,
      Activation_Failed,
      Activation_Successful,
      Activation_Complete,
      Running,
      Runnable,
      Sleeping,
      Blocked,
      Cycle_Completed,
      Change_Cycle_Period,
      Change_Relative_Deadline);

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

   type Oak_Task_Kind is (Regular, Scheduler);
   type Reason_For_Run is (
      Task_Yield,
      Select_Next_Task,
      Add_Task,
      Remove_Task);

   type Boolean_Access is access all Boolean;

   type Activation_Chain is limited private;

   type Activation_Chain_Access is access all Activation_Chain;

   Unspecified_Priority : constant Integer := -1;

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

   type Oak_Task (Kind : Oak_Task_Kind := Regular) is record
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
      Elaborated      : Boolean_Access := null;

      Memory_List : Memory_Region_Link := null;
      --      Registers   : Register_Store;    --  Um, this can Just go onto
      --  the stack.
      --  Duh! Actually maybe we do...
      --  Actually we don't. Registers can
      --  simple be pushed onto the top of the
      --  stack when a task losses its context
      --  and poped off when it regains.
      --  need it...

      case Kind is
         when Regular =>
            State           : Task_State   := Sleeping;
            Normal_Priority : Any_Priority := Default_Priority;
            Deadline        : Time_Span    := Time_Span_Zero;
            Cycle_Period    : Time_Span    := Time_Span_Zero;
            Phase           : Time_Span    := Time_Span_Zero;

            Next_Deadline  : Time := Time_Last;
            Next_Run_Cycle : Time := Time_Last;
            Wake_Time      : Time := Time_Last;

            Scheduler_Agent : Oak_Task_Handler  := null;
            Scheduler_Queue : Task_Link_Element;
            Deadline_List   : Task_Link_Element;

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
