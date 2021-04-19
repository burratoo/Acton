------------------------------------------------------------------------------------------
--                                                                                      --
--                                    ACTON ANALYSER                                    --
--                                                                                      --
--                                  PROTECTED_OBJECTS                                   --
--                                                                                      --
--                       Copyright (C) 2016-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Ada.Real_Time;    use Ada.Real_Time;
with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;
with Ada.Strings.Wide_Unbounded.Wide_Text_IO; use Ada.Strings.Wide_Unbounded.Wide_Text_IO;

with Ada.Containers; use Ada.Containers;

package body Protected_Objects is

   procedure Add_Priority
     (Name     : in Wide_String;
      Priority : in Integer)
   is
      C : constant Protected_Unit_Store.Cursor := Protected_Units.Find (To_Unbounded_Wide_String (Name));
   begin
      Protected_Units (C).Priority := Priority;
   end Add_Priority;

   procedure Add_Protected_Body_Information
     (Name   : in Wide_String;
      Source : in Wide_String;
      Line   : in Asis.Text.Line_Number)
   is
      Task_Identifier : constant Unbounded_Wide_String :=
                          To_Unbounded_Wide_String (Name);
      Body_Location   : constant Element_Location :=
                          (Source_Name => To_Unbounded_Wide_String (Source),
                           Line_Number => Line);
      Storage_Id      : Protected_Unit_Store.Cursor := Protected_Units.Find (Task_Identifier);

   begin

      if Storage_Id = Protected_Unit_Store.No_Element then
         declare
            Task_Info : Protected_Unit_Info :=
                          (Body_Location      => Body_Location,
                           others             => <>);
            Inserted  : Boolean;
         begin
            Protected_Units.Insert (Key      => Task_Identifier,
                                    New_Item => Task_Info,
                                    Position => Storage_Id,
                                    Inserted => Inserted);
         end;

      else
         Protected_Units (Storage_Id).Body_Location := Body_Location;
      end if;
   end Add_Protected_Body_Information;

   procedure Add_Protected_Object_Declaration
     (Name              : in Wide_String;
      Unit_Name         : in Wide_String;
      Number_Of_Objects : in Integer;
      Source            : in Wide_String;
      Line              : in Asis.Text.Line_Number)
   is
      Task_Identifier : constant Unbounded_Wide_String :=
                          To_Unbounded_Wide_String (Name);
      Decl_Location   : constant Element_Location :=
                          (Source_Name => To_Unbounded_Wide_String (Source),
                           Line_Number => Line);
      Unit_Decl       : Protected_Unit_Store.Cursor := Protected_Units.Find (To_Unbounded_Wide_String (Unit_Name));
      Item_Inserted   : Boolean;
   begin
      if Unit_Decl = Protected_Unit_Store.No_Element then
         Protected_Units.Insert (Key      => To_Unbounded_Wide_String (Unit_Name),
                                 New_Item => (others => <>),
                                 Position => Unit_Decl,
                                 Inserted => Item_Inserted);
      end if;

      declare
         Task_Decl : constant Protected_Object_Info :=
                       (Object_Location   => Decl_Location,
                        Unit_Declaration  => Unit_Decl,
                        Number_Of_Objects => Number_Of_Objects);
      begin
         Protected_Objects.Insert (Key      => Task_Identifier,
                                   New_Item => Task_Decl);
      end;
   end Add_Protected_Object_Declaration;

   procedure Add_Protected_Unit_Declaration
     (Name         : in Wide_String;
      Is_Singleton : in Boolean;
      Source       : in Wide_String;
      Line         : in Asis.Text.Line_Number)
   is
      Task_Identifier     : constant Unbounded_Wide_String :=
                              To_Unbounded_Wide_String (Name);
      Definition_Location : constant Element_Location :=
                              (Source_Name => To_Unbounded_Wide_String (Source),
                               Line_Number => Line);

      Storage_Id      : constant Protected_Unit_Store.Cursor := Protected_Units.Find (Task_Identifier);

   begin

      if Storage_Id = Protected_Unit_Store.No_Element then
         declare
            Task_Info : Protected_Unit_Info :=
                          (Declaration_Location => Definition_Location,
                           Is_Singleton         => Is_Singleton,
                           Priority             => Default_Priority_Ceiling,
                           others               => <>);
         begin
            Protected_Units.Insert (Key      => Task_Identifier,
                                    New_Item => Task_Info);
         end;

      else
         Protected_Units (Storage_Id).Declaration_Location := Definition_Location;
         Protected_Units (Storage_Id).Is_Singleton := Is_Singleton;
         Protected_Units (Storage_Id).Priority     := Default_Priority_Ceiling;
      end if;
   end Add_Protected_Unit_Declaration;

   procedure Print_Protected_Units is
      W : constant := 26;
   begin
      Put_Line (50 * "🌳 ");
      New_Line;
      Put_Line ("PROTECTED UNITS");
      Put_Line (50 * "=");

      --  Print out each task details

      for C in Protected_Units.Iterate loop
         declare
            Identifier : constant Unbounded_Wide_String := Key (C);
         begin
            Put (Format_Property_String ("Protected Identifier", W));
            Put (Identifier);
            if Protected_Units (C).Is_Singleton then
               Put (" (single protected)");
            end if;
            New_Line;

            Put_Line (Format_Property_String ("Declaration Location", W) & Element_Location_Image (Protected_Units (C).Declaration_Location));

            Put_Line (Format_Property_String ("Body Location", W) & Element_Location_Image (Protected_Units (C).Body_Location));

            Put (Format_Property_String ("Protected Object Priority", W));
            Put (Image (Protected_Units (C).Priority));
            New_Line;

            New_Line;
         end;
      end loop;

      Put_Line (50 * "=");
      New_Line;
      Put_Line ("Total number of protected units : " & Ada.Containers.Count_Type'Wide_Image (Protected_Units.Length));
      New_Line;
      Put_Line (Line_Decoration);


      --        Put_Line ("Total stack space allocated : " & Integer'Wide_Image (Total_Storage_Size) & " bytes");
   end Print_Protected_Units;

   procedure Print_Protected_Objects is
      W : constant := 30;
   begin
      Put_Line (Line_Decoration);
      New_Line;
      Put_Line ("PROTECTED OBJECTS");
      Put_Line (50 * "=");

      for C in Protected_Objects.Iterate loop
         declare
            Identifier  : constant Unbounded_Wide_String := Key (C);
            Declaration : constant Protected_Unit_Store.Cursor := Protected_Objects (C).Unit_Declaration;
         begin
            Put (Format_Property_String ("Protected Object Name", W) & Identifier);
            New_Line;

            Put_Line (Format_Property_String ("Protected Unit Name", W) & Key (Declaration));
            Put_Line (Format_Property_String ("Protected Object Location", W) & Element_Location_Image (Protected_Objects (C).Object_Location));
            Put_Line (Format_Property_String ("Protected Declaration Location", W) & Element_Location_Image (Protected_Units (Declaration).Declaration_Location));
            Put_Line (Format_Property_String ("Number of Protected Objects", W) & Image (Protected_Objects (C).Number_Of_Objects));
            New_Line;
         end;
      end loop;

      Put_Line (50 * "=");
      New_Line;
      Put_Line ("Total number of protected object declarations : " & Image (Integer (Protected_Objects.Length)));
      Put_Line ("Total number of protected objects             : " & Image (Total_Number_Of_Protected_Objects));
      New_Line;
      Put_Line (Line_Decoration);
   end Print_Protected_Objects;

   function Total_Number_Of_Protected_Objects return Integer is
      Q : Integer := 0;
   begin
      for Object of Protected_Objects loop
         Q := Q + Object.Number_Of_Objects;
      end loop;
      return Q;
   end Total_Number_Of_Protected_Objects;

end Protected_Objects;
