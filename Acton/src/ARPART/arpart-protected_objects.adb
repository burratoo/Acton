with Oak.Processor_Support_Package.Task_Support;
with Oak.Oak_Task.Data_Access;
with Oak.Oak_Task.Protected_Object;
with Oak.Core;
use Oak.Oak_Task;

package body ARPART.Protected_Objects is

   package OTS renames Oak.Processor_Support_Package.Task_Support;
   package OTP renames Oak.Oak_Task.Protected_Object;

   procedure Enter_Protected_Object
     (PO                    : Oak.Oak_Task.Oak_Task_Handler;
      Subprogram_Kind       : Oak.Oak_Task.Protected_Subprogram_Type;
      Entry_Id              : Oak.Oak_Task.Entry_Index) is
      Self : constant access Oak_Task       :=
         Oak.Core.Get_Current_Task;
      Message : constant Oak_Task_Message :=
                (Message_Type    => Entering_PO,
                 PO_Enter        => PO,
                 Subprogram_Kind => Subprogram_Kind,
                 Entry_Id_Enter  => Entry_Id);
   begin
      OTS.Yield_Processor_To_Kernel (Task_Message => Message);
      if Data_Access.Get_State (T => Self) = Enter_PO_Refused then
         raise Program_Error;
      end if;
   end Enter_Protected_Object;

   procedure Exit_Protected_Object (PO : Oak.Oak_Task.Oak_Task_Handler) is
      Self     : constant access Oak_Task       :=
                   Oak.Core.Get_Current_Task;
      Message  : constant Oak_Task_Message :=
                (Message_Type  => Exiting_PO,
                 PO_Exit       => PO);
   begin
      OTS.Yield_Processor_To_Kernel (Task_Message => Message);
      if Data_Access.Get_State (T => Self) = Exit_PO_Error then
         raise Program_Error;
      end if;
   end Exit_Protected_Object;

   function Entry_Count
     (PO       : Oak.Oak_Task.Oak_Task_Handler;
      Entry_Id : Oak.Oak_Task.Entry_Index) return Natural is
   begin
      return OTP.Entry_Queue_Length (PO => PO, Entry_Id => Entry_Id);
   end Entry_Count;

end ARPART.Protected_Objects;
