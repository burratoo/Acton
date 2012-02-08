with Oak.Oak_Task;

package ARPART.Protected_Objects is

   procedure Enter_Protected_Object
     (PO              : Oak.Oak_Task.Oak_Task_Handler;
      Subprogram_Kind : Oak.Oak_Task.Protected_Subprogram_Type;
      Entry_Id        : Oak.Oak_Task.Entry_Index := Oak.Oak_Task.No_Entry);

   procedure Exit_Protected_Object
     (PO                : Oak.Oak_Task.Oak_Task_Handler;
      Barrier_Exception : Boolean := False);

   function Entry_Count
     (PO       : Oak.Oak_Task.Oak_Task_Handler;
      Entry_Id : Oak.Oak_Task.Entry_Index) return Natural;

end ARPART.Protected_Objects;
