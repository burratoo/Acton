------------------------------------------------------------------------------------------
--                                                                                      --
--                                    ACTON ANALYSER                                    --
--                                                                                      --
--                                      SCHEDULERS                                      --
--                                                                                      --
--                       Copyright (C) 2016-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;
with Ada.Strings.Wide_Unbounded.Wide_Text_IO; use Ada.Strings.Wide_Unbounded.Wide_Text_IO;

package body Schedulers is
   procedure Add_Scheduler
     (Name                          : in Wide_String;
      Low_Priority                  : in Integer;
      High_Priority                 : in Integer;
      Storage_Size                  : in Integer;
      Source                        : in Wide_String;
      Line                          : in Asis.Text.Line_Number)
   is
      S : constant Scheduler_Info :=
            (Scheduler_Name  => To_Unbounded_Wide_String (Name),
             Low_Priority    => Low_Priority,
             High_Priority   => High_Priority,
             Storage_Size    => Storage_Size,
             Pragma_Location => (Source_Name => To_Unbounded_Wide_String (Source),
                                 Line_Number => Line));
   begin
      Schedulers.Insert (S);
   end Add_Scheduler;

   procedure Print_Schedulers is
      W : constant := 15;
   begin
      Put_Line (Line_Decoration);
      New_Line;
      Put_Line ("SCHEDULERS");
      Put_Line (50 * "=");

      for Scheduler of Schedulers loop
         Put_Line (Format_Property_String ("Scheduler Name", W) & Scheduler.Scheduler_Name);
         Put_Line (Format_Property_String ("Pragma Location", W) & Element_Location_Image (Scheduler.Pragma_Location));
         Put_Line (Format_Property_String ("Priority_Range", W) & Image (Scheduler.Low_Priority) & " .. " & Image (Scheduler.High_Priority));
         Put_Line (Format_Property_String ("Storage Size", W) & Image (Scheduler.Storage_Size) & " bytes");
      end loop;

      Put_Line (50 * "=");
      New_Line;
      Put_Line ("Total number of schedulers : " & Image (Integer (Schedulers.Length)));
      New_Line;
      Put_Line (Line_Decoration);
   end Print_Schedulers;


end Schedulers;
