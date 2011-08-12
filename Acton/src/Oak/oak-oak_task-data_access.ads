with System.Storage_Elements;

package Oak.Oak_Task.Data_Access is

   pragma Preelaborate;

   procedure Initialise_Task
     (T                 : Oak_Task_Handler;
      Stack_Address     : System.Address;
      Stack_Size        : System.Storage_Elements.Storage_Count;
      Name              : String;
      Normal_Priority   : Integer;
      Relative_Deadline : Time_Span;
      Cycle_Period      : Time_Span;
      Phase             : Time_Span;
      Run_Loop          : Address;
      Elaborated        : Boolean_Access);

   procedure Initialise_Main_Task
     (T                 : Oak_Task_Handler;
      Stack_Size        : System.Storage_Elements.Storage_Count;
      Name              : String;
      Normal_Priority   : Integer;
      Run_Loop          : Address);

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
   function Get_Activation_List
     (T    : access Oak_Task)
      return access Oak_Task;
end Oak.Oak_Task.Data_Access;
