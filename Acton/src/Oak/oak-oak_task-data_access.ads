with System.Storage_Elements;

package Oak.Oak_Task.Data_Access is

   pragma Preelaborate;

   procedure Initialise_Task
     (T                 : in Oak_Task_Handler;
      Stack_Address     : in System.Address;
      Stack_Size        : in System.Storage_Elements.Storage_Count;
      Name              : in String;
      Normal_Priority   : in Integer;
      Relative_Deadline : in Time_Span;
      Cycle_Period      : in Time_Span;
      Phase             : in Time_Span;
      Run_Loop          : in Address;
      Task_Value_Record : in System.Address;
      Chain             : in out Activation_Chain;
      Elaborated        : in Boolean_Access);

   procedure Initialise_Main_Task
     (Stack_Size      : in System.Storage_Elements.Storage_Count;
      Name            : in String;
      Normal_Priority : in Integer;
      Run_Loop        : in Address);

   procedure Set_Scheduler_Agent
     (T               : access Oak_Task;
      Scheduler_Agent : Oak_Task_Handler);

   function Get_Id (T : access Oak_Task) return Task_Id;
   function Get_Name (T : access Oak_Task) return Task_Name;

   function Get_State (T : access Oak_Task) return Task_State;
   procedure Set_State (T : access Oak_Task; State : Task_State);

   function Get_Normal_Priority (T : access Oak_Task) return Any_Priority;
   function Get_Deadline (T : access Oak_Task) return Time_Span;
   function Get_Cycle_Period (T : access Oak_Task) return Time_Span;
   function Get_Phase (T : access Oak_Task) return Time_Span;
   function Get_Next_Run_Time (T : access Oak_Task) return Time;

   function Get_Wake_Time (T : access Oak_Task) return Time;
   procedure Set_Wake_Time (T : access Oak_Task; WT : Time);

   function Is_Elaborated (T : access Oak_Task) return Boolean;
   procedure Set_Activation_List
     (T     : access Oak_Task;
      Chain : in Activation_Chain_Access);
   function Get_Activation_List
     (T    : access Oak_Task)
      return access Oak_Task;
end Oak.Oak_Task.Data_Access;
