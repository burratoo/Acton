with Oak.Project_Support_Package;
use Oak.Project_Support_Package;

package Oak.Indices with Pure is
   No_Entry     : constant := 0;
   Single_Entry : constant := 1;
   No_Index     : constant := 0;

   Max_Entries : constant :=
     (if Max_Protected_Entries > Max_Task_Entries then
                Max_Protected_Entries else Max_Task_Entries);

   type Entry_Index is range No_Entry .. Max_Entries;

   subtype Protected_Entry_Index is Entry_Index
   range No_Entry .. Max_Protected_Entries;

end Oak.Indices;
