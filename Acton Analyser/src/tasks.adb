with Ada.Real_Time;    use Ada.Real_Time;
with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;
with Ada.Strings.Wide_Unbounded.Wide_Text_IO; use Ada.Strings.Wide_Unbounded.Wide_Text_IO;

with Ada.Containers; use Ada.Containers;

package body Tasks is

   procedure Add_Cycle_Behaviour
     (Name      : in Wide_String;
      Behaviour : in Cyclic_Kind)
   is
      C : constant Task_Unit_Store.Cursor := Task_Units.Find (To_Unbounded_Wide_String (Name));
   begin
      Task_Units (C).Cyclic_Behaviour := Behaviour;
   end Add_Cycle_Behaviour;

   procedure Add_Priority
     (Name     : in Wide_String;
      Priority : in Integer)
   is
      C : constant Task_Unit_Store.Cursor := Task_Units.Find (To_Unbounded_Wide_String (Name));
   begin
      Task_Units (C).Priority := Priority;
   end Add_Priority;

   procedure Add_Storage_Size
     (Name : in Wide_String;
      Size : in Integer)
   is
      C : constant Task_Unit_Store.Cursor := Task_Units.Find (To_Unbounded_Wide_String (Name));
   begin
      Task_Units (C).Storage_Size := Size;
   end Add_Storage_Size;

   procedure Add_Cycle_Period_Span
     (Name   : in Wide_String;
      Period : in Ada.Real_Time.Time_Span)
   is
      C : constant Task_Unit_Store.Cursor := Task_Units.Find (To_Unbounded_Wide_String (Name));
   begin
      Task_Units (C).Cycle_Period := Period;
   end Add_Cycle_Period_Span;

   procedure Add_Cycle_Phase_Span
     (Name  : in Wide_String;
      Phase : in Ada.Real_Time.Time_Span)
   is
      C : constant Task_Unit_Store.Cursor := Task_Units.Find (To_Unbounded_Wide_String (Name));
   begin
      Task_Units (C).Cycle_Phase := Phase;
   end Add_Cycle_Phase_Span;

   procedure Add_Relative_Deadline
     (Name     : in Wide_String;
      Deadline : in Ada.Real_Time.Time_Span)
   is
      C : constant Task_Unit_Store.Cursor := Task_Units.Find (To_Unbounded_Wide_String (Name));
   begin
      Task_Units (C).Relative_Deadline := Deadline;
   end Add_Relative_Deadline;

   procedure Add_Exceution_Deadline
     (Name     : in Wide_String;
      Deadline : in Ada.Real_Time.Time_Span)
   is
      C : constant Task_Unit_Store.Cursor := Task_Units.Find (To_Unbounded_Wide_String (Name));
   begin
      Task_Units (C).Execution_Budget := Deadline;
   end Add_Exceution_Deadline;


   procedure Add_Task_Body_Information
     (Name               : in Wide_String;
      Has_Cyclic_Section : in Boolean;
      Is_Main_Task       : in Boolean;
      Source             : in Wide_String;
      Line               : in Asis.Text.Line_Number)
   is
      Task_Identifier : constant Unbounded_Wide_String :=
                          To_Unbounded_Wide_String (Name);
      Body_Location   : constant Element_Location :=
                          (Source_Name => To_Unbounded_Wide_String (Source),
                           Line_Number => Line);
      Storage_Id      : Task_Unit_Store.Cursor := Task_Units.Find (Task_Identifier);

   begin

      if Storage_Id = Task_Unit_Store.No_Element then
         declare
            Task_Info : Task_Unit_Info :=
                          (Body_Location      => Body_Location,
                           Has_Cyclic_Section => Has_Cyclic_Section,
                           Is_Main_Task       => Is_Main_Task,
                           others             => <>);
            Inserted  : Boolean;
         begin
            Task_Units.Insert (Key      => Task_Identifier,
                               New_Item => Task_Info,
                               Position => Storage_Id,
                               Inserted => Inserted);
         end;

      else
         Task_Units (Storage_Id).Has_Cyclic_Section := Has_Cyclic_Section;
         Task_Units (Storage_Id).Body_Location := Body_Location;
      end if;

      if Is_Main_Task then
         if Task_Units (Storage_Id).Priority = Uninitialised_Priority then
            Task_Units (Storage_Id).Priority := Default_Priority;
            Task_Units (Storage_Id).Is_Main_Task := True;
         end if;

         if Task_Units (Storage_Id).Storage_Size = Uninitialised_Storage_Size then
            Task_Units (Storage_Id).Storage_Size := Stack_Size_Main_Task;
         end if;
      end if;
   end Add_Task_Body_Information;

   procedure Add_Task_Object_Declaration
     (Name             : in Wide_String;
      Unit_Name        : in Wide_String;
      Is_Main_Task     : in Boolean;
      Number_Of_Tasks  : in Integer;
      Source           : in Wide_String;
      Line             : in Asis.Text.Line_Number)
   is
      Task_Identifier : constant Unbounded_Wide_String :=
                          To_Unbounded_Wide_String (Name);
      Decl_Location   : constant Element_Location :=
                          (Source_Name => To_Unbounded_Wide_String (Source),
                           Line_Number => Line);
      Unit_Decl       : Task_Unit_Store.Cursor := Task_Units.Find (To_Unbounded_Wide_String (Unit_Name));
      Item_Inserted   : Boolean;
   begin
      if Is_Main_Task
        and then Task_Objects.Find (Task_Identifier) /= Task_Object_Store.No_Element
      then
         return;
      end if;

      if Unit_Decl = Task_Unit_Store.No_Element then
         Task_Units.Insert (Key      => To_Unbounded_Wide_String (Unit_Name),
                            New_Item => (others => <>),
                            Position => Unit_Decl,
                            Inserted => Item_Inserted);
      end if;

      declare
         Task_Decl : constant Task_Decls :=
                       (Object_Location  => Decl_Location,
                        Unit_Declaration => Unit_Decl,
                        Number_Of_Tasks  => Number_Of_Tasks,
                        Is_Main_Task     => Is_Main_Task);
      begin
         Task_Objects.Insert (Key      => Task_Identifier,
                              New_Item => Task_Decl);
      end;
   end Add_Task_Object_Declaration;

   procedure Add_Task_Unit_Declaration
     (Name         : in Wide_String;
      Is_Main_Task : in Boolean;
      Is_Singleton : in Boolean;
      Source       : in Wide_String;
      Line         : in Asis.Text.Line_Number)
   is
      Task_Identifier     : constant Unbounded_Wide_String :=
                              To_Unbounded_Wide_String (Name);
      Definition_Location : constant Element_Location :=
                              (Source_Name => To_Unbounded_Wide_String (Source),
                               Line_Number => Line);

      Storage_Id      : constant Task_Unit_Store.Cursor := Task_Units.Find (Task_Identifier);

   begin

      if Storage_Id = Task_Unit_Store.No_Element then
         declare
            Task_Info : Task_Unit_Info :=
                          (Declaration_Location => Definition_Location,
                           Is_Main_Task         => Is_Main_Task,
                           Is_Singleton         => Is_Singleton,
                           Storage_Size         => Default_Storage_Size,
                           Priority             => Default_Priority,
                           others               => <>);
         begin
            Task_Units.Insert (Key      => Task_Identifier,
                               New_Item => Task_Info);
         end;

      else
         Task_Units (Storage_Id).Declaration_Location := Definition_Location;
         Task_Units (Storage_Id).Is_Singleton := Is_Singleton;
         Task_Units (Storage_Id).Storage_Size := Default_Storage_Size;
         Task_Units (Storage_Id).Priority     := Default_Priority;
      end if;
   end Add_Task_Unit_Declaration;

   procedure Print_Task_Units is

      function Time_Span_Image (V : Ada.Real_Time.Time_Span) return Wide_String;

      function Time_Span_Image (V : Ada.Real_Time.Time_Span) return Wide_String
      is
         D : Duration := To_Duration (V);

         function Wide_Image (Value : Duration) return Wide_String;

         function Wide_Image (Value : Duration) return Wide_String is
            S             : constant Wide_String := Duration'Wide_Image (Value);
            End_Of_String : Integer;
         begin
            for J in reverse S'Range loop
               case S (J) is
                  when '0' =>
                     null;

                  when '.' =>
                     End_Of_String := J - 1;
                     exit;

                  when ' ' =>
                     End_Of_String := J + 1;
                     exit;

                  when others =>
                     End_Of_String := J;
                     exit;
               end case;
            end loop;

            return S (2 .. End_Of_String);
         end Wide_Image;
      begin
         if V = Ada.Real_Time.Time_Span_First then
            return No_Value_Image;
         elsif V = Ada.Real_Time.Time_Span_Zero then
            return Wide_Image (D) & " seconds";
         elsif V < Microseconds (1) then
            return Wide_Image (D * 1_000_000_000) & " nanoseconds";
         elsif V < Milliseconds (1) then
            return Wide_Image (D * 1_000_000) & " microseconds";
         elsif V < Seconds (1) then
            return Wide_Image (D * 1_000) & " milliseconds";
         else
            return Wide_Image (D) & " seconds";
         end if;

      end Time_Span_Image;

      W : constant := 22;
   begin
      Put_Line (50 * "🌳 ");
      New_Line;
      Put_Line ("TASK UNITS");
      Put_Line (50 * "=");

      --  Print out each task details

      for C in Task_Units.Iterate loop
         declare
            Identifier : constant Unbounded_Wide_String := Key (C);
         begin
            if Task_Units (C).Is_Main_Task then
               Put_Line (Format_Property_String ("Main Task", W) & Identifier);
            else
               Put (Format_Property_String ("Task Identifier", W));
               Put (Identifier);
               if Task_Units (C).Is_Singleton then
                  Put (" (single task)");
               end if;
               New_Line;
            end if;

            Put_Line (Format_Property_String ("Declaration Location", W) & Element_Location_Image (Task_Units (C).Declaration_Location));

            Put_Line (Format_Property_String ("Body Location", W) & Element_Location_Image (Task_Units (C).Body_Location));

            Put (Format_Property_String ("Task Behaviour", W));
            Put (Cyclic_Kind'Wide_Image (Task_Units (C).Cyclic_Behaviour));
            New_Line;

            Put (Format_Property_String ("Task Priority", W));
            Put (Image (Task_Units (C).Priority));
            New_Line;

            Put (Format_Property_String ("Task Storage Size", W));
            Put (Image (Task_Units (C).Storage_Size));
            Put (" bytes");
            New_Line;

            if not Task_Units (C).Is_Main_Task then
               Put (Format_Property_String ("Cyclic section", W));
               if Task_Units (C).Has_Cyclic_Section then
                  Put ("Yes");
               else
                  Put ("No");
               end if;
               New_Line;

               case Task_Units (C).Cyclic_Behaviour is
               when Periodic =>

                  Put (Format_Property_String ("Cycle Period", W));
                  Put (Time_Span_Image (Task_Units (C).Cycle_Period));
                  New_Line;

                  Put (Format_Property_String ("Cycle Phase", W));
                  Put (Time_Span_Image (Task_Units (C).Cycle_Phase));
                  New_Line;

               when Sporadic =>
                  Put (Format_Property_String ("Inter-release Period", W));
                  Put (Time_Span_Image (Task_Units (C).Cycle_Period));
                  New_Line;

                  Put (Format_Property_String ("Initial Release Offset", W));
                  Put (Time_Span_Image (Task_Units (C).Cycle_Phase));
                  New_Line;

               when others =>
                  Put (Format_Property_String ("Initial Release Offset", W));
                  Put (Time_Span_Image (Task_Units (C).Cycle_Phase));
                  New_Line;
               end case;

               Put (Format_Property_String ("Execution Budget", W));
               Put (Time_Span_Image (Task_Units (C).Execution_Budget));
               New_Line;

               Put (Format_Property_String ("Relative Deadline", W)  );
               Put (Time_Span_Image (Task_Units (C).Relative_Deadline));
               New_Line;
            end if;

            New_Line;
         end;
      end loop;

      Put_Line (50 * "=");
      New_Line;
      Put_Line ("Total number of task units : " & Ada.Containers.Count_Type'Wide_Image (Task_Units.Length));
      New_Line;
      Put_Line (Line_Decoration);


      --        Put_Line ("Total stack space allocated : " & Integer'Wide_Image (Total_Storage_Size) & " bytes");
   end Print_Task_Units;

   procedure Print_Task_Objects is
      W : constant := 25;
   begin
      Put_Line (Line_Decoration);
      New_Line;
      Put_Line ("TASK OBJECTS");
      Put_Line (50 * "=");

      for C in Task_Objects.Iterate loop
         declare
            Identifier  : constant Unbounded_Wide_String := Key (C);
            Declaration : constant Task_Unit_Store.Cursor := Task_Objects (C).Unit_Declaration;
         begin
            Put (Format_Property_String ("Task Name", W) & Identifier);
            if Task_Objects (C).Is_Main_Task then
               Put (" (main task)");
            end if;
            New_Line;

            Put_Line (Format_Property_String ("Task Unit Name", W) & Key (Declaration));
            Put_Line (Format_Property_String ("Task Object Location", W) & Element_Location_Image (Task_Objects (C).Object_Location));
            Put_Line (Format_Property_String ("Task Declaration Location", W) & Element_Location_Image (Task_Units (Declaration).Declaration_Location));
            Put_Line (Format_Property_String ("Number of Task Objects", W) & Image (Task_Objects (C).Number_Of_Tasks));
            New_Line;
         end;
      end loop;

      Put_Line (50 * "=");
      New_Line;
      Put_Line ("Total number of task object declarations : " & Image (Integer (Task_Objects.Length)));
      Put_Line ("Total number of task objects             : " & Image (Total_Number_Of_Task_Objects));
      New_Line;
      Put_Line (Line_Decoration);
   end Print_Task_Objects;

   function Total_Number_Of_Task_Objects return Integer is
      Q : Integer := 0;
   begin
      for Object of Task_Objects loop
         Q := Q + Object.Number_Of_Tasks;
      end loop;
       return Q;
   end Total_Number_Of_Task_Objects;

   function Stack_Space_Allocated_To_Task_Objects return Integer is
      Total_Stack_Size : Integer := 0;
   begin
      for Object of Task_Objects loop
         Total_Stack_Size := Total_Stack_Size + Object.Number_Of_Tasks * Task_Units (Object.Unit_Declaration).Storage_Size;
      end loop;
      return Total_Stack_Size;
   end Stack_Space_Allocated_To_Task_Objects;
end Tasks;
