------------------------------------------------------------------------------------------
--                                                                                      --
--                                    ACTON ANALYSER                                    --
--                                                                                      --
--                                  PROGRAM_STATISTICS                                  --
--                                                                                      --
--                       Copyright (C) 2016-2021, Patrick Bernardi                      --
--                                                                                      --
------------------------------------------------------------------------------------------

package Program_Statistics is
   procedure Calculate_Statistics;
   procedure Print_Statistics;

private
   Number_Of_Task_Units   : Integer;
   Number_Of_Task_Objects : Integer;

   Number_Of_Protected_Units   : Integer;
   Number_Of_Protected_Objects : Integer;

   Program_Stack_Size   : Integer;

end Program_Statistics;
