with ASIS_Application_Driver_1;
with Protected_Objects;
with Tasks;
with Acton_Properties;
with Program_Statistics;
with Schedulers;

procedure Acton_Analyser is

begin
   ASIS_Application_Driver_1;
   Acton_Properties.Print_Acton_Properties;
   Tasks.Print_Task_Units;
   Tasks.Print_Task_Objects;
   Protected_Objects.Print_Protected_Units;
   Protected_Objects.Print_Protected_Objects;
   Schedulers.Print_Schedulers;
   Program_Statistics.Calculate_Statistics;
   Program_Statistics.Print_Statistics;

end Acton_Analyser;
