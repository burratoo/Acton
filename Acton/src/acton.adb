with Oak.Scheduler;

with Oak.Core;
with Oak.Oak_Task;                                   use Oak.Oak_Task;
with Oak.Oak_Task.Data_Access;                       use Oak.Oak_Task.Data_Access;
with Ada.Real_Time;                                  use Ada.Real_Time;
with System;                                         use System;
with Oak.Memory.Call_Stack;                          use Oak.Memory.Call_Stack;
with Oak.Memory.Call_Stack.Ops;                      use Oak.Memory.Call_Stack.Ops;
with Acton.Scheduler_Agent.Fixed_Priority_Scheduler;
with Oak.Oak_Task.Scheduler_Agent;                   use Oak.Oak_Task.Scheduler_Agent;
with Oak.Scheduler.Agent_List;
with Oak.Processor_Support_Package;
with MPC5554.Init;
pragma Unreferenced (MPC5554.Init);

with MPC5554;                                       use MPC5554;
with MPC5554.SIU;                                   use MPC5554.SIU;
with Oak.Oak_Task.Context;
with Oak.Processor_Support_Package.Task_Interrupts;

package body Acton is
   package FPS renames Acton.Scheduler_Agent.Fixed_Priority_Scheduler;

   My_Task_Data : aliased Oak_Task;
   SA_Data      : aliased Oak_Task;
   --  A_Task : aliased Oak_Task;

   My_Task : constant Oak_Task_Handler := My_Task_Data'Access;
   SA      : constant Oak_Task_Handler := SA_Data'Access;
   --   A_Task                    : constant Oak_Task_Handler := A_Task_Data'Access;

   My_T_Call_Stack_Access : Call_Stack_Handler;
   SA_Call_Stack_Access   : Call_Stack_Handler;
   --   A_T_Call_Stack_Access : Call_Stack_Handler;

   LED_GPIO_Pad : constant Pad_Configuration_Pointer :=
      Pad_Configuration_Register_Array (179)'Access;
   LED          : constant GPO_Data_Register_Pointer :=
      GPO_Data_Register_Array (179)'Access;

   procedure LED_Flasher is
   begin
      loop
         LED.all := High;
         delay until Clock + Seconds (1);
         LED.all := Low;
         Oak.Oak_Task.Context.Task_Cycle_Completed;
      end loop;
   end LED_Flasher;

   procedure Start is
      OI        : constant access Oak.Core.Oak_Data                :=
         Oak.Core.Get_Oak_Instance;
      Scheduler : constant access Oak.Scheduler.Oak_Scheduler_Info :=
         Oak.Core.Get_Scheduler_Info (OI);
   begin
      Oak.Processor_Support_Package.Task_Interrupts.Initialise_Task_Enviroment;

      LED_GPIO_Pad.all :=
        (Pin_Assignment           => GPIO,
         Output_Buffer_Enable     => Enable,
         Input_Buffer_Enable      => Disable,
         Drive_Strength_Control   => ds_10pf,
         Open_Drain_Output_Enable => Disable,
         Input_Hysteresis         => Enable,
         Slew_Rate_Control        => Minimum,
         Weak_Pullup_Down_Enable  => Disable,
         Weak_Pullup_Down_Select  => Pulldown);

      LED.all := Low;

      Allocate_Call_Stack (Stack => SA_Call_Stack_Access);  --  dummy to allocate the
                                                            --  Main stack
      Allocate_Call_Stack (Stack => SA_Call_Stack_Access);
      Initialise_Agent
        (Agent        => SA,
         Name         => "SA",
         Call_Stack   => SA_Call_Stack_Access,
         Max_Priority => 5,
         Min_Prioirty => 1,
         Run_Loop     => FPS.Run_Loop'Address);
      Allocate_Call_Stack (Stack => My_T_Call_Stack_Access);
      Initialise_Task
        (T               => My_Task,
         Name            => "b1",
         Normal_Priority => 1,
         Deadline        => Minutes (50),
         Cycle_Period    => Seconds (3),
         Phase           => Time_Span_Zero,
         Run_Loop        => LED_Flasher'Address,
         Stack_Access    => My_T_Call_Stack_Access);
      --        Allocate_Call_Stack (Stack => A_T_Call_Stack_Access);
      --        Initialise_Task
      --          (T               => A_Task,
      --           Name            => "b2",
      --           Normal_Priority => 2,
      --           Deadline        => Minutes (50),
      --           Cycle_Period    => Seconds (5),
      --           Phase           => Seconds (2),
      --           Run_Loop     => Null_Address,
      --           Stack_Access => A_T_Call_Stack_Access);
      Oak.Scheduler.Agent_List.Add_Scheduler_Agent
        (Scheduler_Info => Scheduler.all,
         New_Agent      => SA);
      Oak.Scheduler.Add_Task_To_Scheduler
        (Scheduler_Info => Scheduler.all,
         T              => My_Task);
      Oak.Core.Start;
   end Start;
end Acton;
