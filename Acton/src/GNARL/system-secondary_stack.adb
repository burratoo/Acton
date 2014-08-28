------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This is the HI-E version of this package

with Ada.Unchecked_Conversion;

with Oak.Agent;           use Oak.Agent;
with Oak.Agent.Oak_Agent; use Oak.Agent.Oak_Agent;
with Oak.Agent.Kernel;    use Oak.Agent.Kernel;

with System.Storage_Elements; use System.Storage_Elements;

package body System.Secondary_Stack is

   -----------------
   -- SS_Allocate --
   -----------------

   procedure SS_Allocate
     (Address      : out System.Address;
      Storage_Size : SSE.Storage_Count)
   is
      Max_Align : constant Storage_Count := Standard'Maximum_Alignment;
      Max_Size  : constant Storage_Count :=
                       ((Storage_Size + Max_Align - 1) / Max_Align)
                       * Max_Align;

      Self      : constant Oak_Agent_Id := Current_Agent (This_Oak_Kernel);

   begin
      Address := Secondary_Stack_Pointer (Self);

      if Address + Max_Size > Secondary_Stack_Limit (Self) then
         raise Storage_Error;
      end if;

      Set_Secondary_Stack_Pointer (Self, Address + Max_Size);
   end SS_Allocate;

   -------------
   -- SS_Init --
   -------------

   procedure SS_Init
     (Stk  : System.Address;
      Size : Natural := Default_Secondary_Stack_Size)
   is null;

   -------------
   -- SS_Mark --
   -------------

   function SS_Mark return Mark_Id is
   begin
      return Mark_Id
        (Secondary_Stack_Pointer (Current_Agent (This_Oak_Kernel)));
   end SS_Mark;

   ----------------
   -- SS_Release --
   ----------------

   procedure SS_Release (M : Mark_Id) is
   begin
      Set_Secondary_Stack_Pointer (Current_Agent (This_Oak_Kernel),
                                   Address (M));
   end SS_Release;

end System.Secondary_Stack;
