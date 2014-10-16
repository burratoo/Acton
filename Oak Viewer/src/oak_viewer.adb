with GNAT.Sockets; use GNAT.Sockets;
with Ada.Streams;
with Ada.Text_IO;
with Interfaces; use Interfaces;

with Cortex_Traces; use Cortex_Traces;
with Cortex_Traces.Exceptions; use Cortex_Traces.Exceptions;
with ISA.ARM.Cortex_M4.Exceptions; use ISA.ARM.Cortex_M4.Exceptions;
procedure Oak_Viewer is

   SWO_Server_Name : constant String := Host_Name;
   SWO_Server_Port : constant := 2332;

   Client_Socket  : Socket_Type;
   SWO_Address    : Sock_Addr_Type;
   SWO_Channel    : Stream_Access;

--     Offset         : Ada.Streams.Stream_Element_Offset;
--     Data           : Ada.Streams.Stream_Element_Array (1 .. 1);
--
--     Byte : Unsigned_8;
   package Unsigned_IO is new Ada.Text_IO.Modular_IO (Unsigned_8);
   use Unsigned_IO;
begin
   SWO_Address := (Family => Family_Inet,
                   Addr   => Addresses (Get_Host_By_Name (SWO_Server_Name), 1),
                   Port   => SWO_Server_Port);
   Create_Socket (Client_Socket);
   Set_Socket_Option (Socket => Client_Socket,
                      Level  => Socket_Level,
                      Option => (Name    => GNAT.Sockets.Reuse_Address,
                                 Enabled => True));
   Connect_Socket (Socket => Client_Socket,
                   Server => SWO_Address);

   SWO_Channel := Stream (Client_Socket);

   loop

--        Put (Unsigned_8'Input (SWO_Channel), Base => 16);
--        Ada.Text_IO.New_Line;

--        loop
--           Receive_Socket (Socket => Client_Socket,
--                           Item   => Data,
--                           Last   => Offset,
--                           Flags  => Peek_At_Incoming_Data);
--
--           Byte := Unsigned_8 (Data (1));
--
--           exit when Byte /= 0;
--           Receive_Socket (Socket => Client_Socket,
--                           Item   => Data,
--                           Last   => Offset);
--        end loop;
--
--        Put (Byte, Base => 16);
--        Ada.Text_IO.New_Line;

      declare
         Event : Cortex_Event'Class := Cortex_Event'Class'Input (SWO_Channel);
      begin
--           if not (Event in Exception_Event and then Exception_Event (Event).Exception_Number in SysTick | Thread_Mode) then
         if Event not in Cortex_Event then
            Ada.Text_IO.Put_Line (Event.Image);
         end if;
      end;
   end loop;
end Oak_Viewer;


