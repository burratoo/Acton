------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                               S Y S T E M                                --
--                                                                          --
--                                 S p e c                                  --
--                            (PPC ELF Version)                             --
--                                                                          --
--          Copyright (C) 1992-2008, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Exception_Propagation);
--  Only local exception handling is supported in this profile

pragma Restrictions (No_Exception_Registration);
--  Disable exception name registration. This capability is not used because
--  it is only required by exception stream attributes which are not supported
--  in this run time.

pragma Restrictions (No_Implicit_Dynamic_Code);
--  Pointers to nested subprograms are not allowed in this run time, in order
--  to prevent the compiler from building "trampolines".

pragma Restrictions (No_Finalization);
--  Controlled types are not supported in this run time

--  pragma Restrictions (No_Tasking);
--  Tasking is not supported in this run time

--  pragma Discard_Names;
--  Disable explicitly the generation of names associated with entities in
--  order to reduce the amount of storage used. These names are not used anyway
--  (attributes such as 'Image and 'Value are not supported in this run time).

package System is
   pragma Pure;
   --  Note that we take advantage of the implementation permission to make
   --  this unit Pure instead of Preelaborable; see RM 13.7.1(15). In Ada
   --  2005, this is Pure in any case (AI-362).

   type Name is (SYSTEM_NAME_GNAT);
   System_Name : constant Name := SYSTEM_NAME_GNAT;

   --  System-Dependent Named Numbers

   Min_Int               : constant := Long_Long_Integer'First;
   Max_Int               : constant := Long_Long_Integer'Last;

   Max_Binary_Modulus    : constant := 2 ** Long_Long_Integer'Size;
   Max_Nonbinary_Modulus : constant := 2 ** Integer'Size - 1;

   Max_Base_Digits       : constant := Long_Long_Float'Digits;
   Max_Digits            : constant := Long_Long_Float'Digits;

   Max_Mantissa          : constant := 63;
   Fine_Delta            : constant := 2.0 ** (-Max_Mantissa);

   Tick                  : constant := 1.0 / 60.0;

   --  Storage-related Declarations

   type Address is private;
   pragma Preelaborable_Initialization (Address);
   Null_Address : constant Address;

   Storage_Unit : constant := 8;
   Word_Size    : constant := 32;
   Memory_Size  : constant := 2 ** 32;

   --  Address comparison

   function "<"  (Left, Right : Address) return Boolean;
   function "<=" (Left, Right : Address) return Boolean;
   function ">"  (Left, Right : Address) return Boolean;
   function ">=" (Left, Right : Address) return Boolean;
   function "="  (Left, Right : Address) return Boolean;

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "=");

   --  Other System-Dependent Declarations

   type Bit_Order is (High_Order_First, Low_Order_First);
   Default_Bit_Order : constant Bit_Order := High_Order_First;
   pragma Warnings (Off, Default_Bit_Order); -- kill constant condition warning

   --  Priority-related Declarations (RM D.1)

   --  ??? need comments explaining the values below

   Max_Priority           : constant Positive := 25;
   Max_Interrupt_Priority : constant Positive := 30;

   --  Need to provide the range in numbers to make the binder happy.
   --  Don't change the spacing of Any_Priority subtype definition!

   subtype Any_Priority       is Integer      range   0 .. 30;
   subtype Priority           is Any_Priority range   0 .. 25;
   subtype Interrupt_Priority is Any_Priority range
     Priority'Last + 1 .. Any_Priority'Last;

   subtype Oak_Priority           is Integer      range
     Any_Priority'First .. Any_Priority'Last + 1;
   subtype Oak_Interrupt_Priority is Oak_Priority range
     Interrupt_Priority'First .. Oak_Priority'Last;

   Default_Priority : constant Priority := 14;

private

   type Address is mod Memory_Size;
   Null_Address : constant Address := 0;

   --------------------------------------
   -- System Implementation Parameters --
   --------------------------------------

   --  These Parameters provide information about the target that is used
   --  by the compiler. They are in the private part of System, where they
   --  can be accessed using the special circuitry in the Targparm unit
   --  whose source should be consulted for more detailed descriptions
   --  of the individual switch values.

   Backend_Divide_Checks     : constant Boolean := False;
   Backend_Overflow_Checks   : constant Boolean := True;
   Command_Line_Args         : constant Boolean := False;
   Configurable_Run_Time     : constant Boolean := True;
   Denorm                    : constant Boolean := True;
   Duration_32_Bits          : constant Boolean := False;
   Exit_Status_Supported     : constant Boolean := False;
   Fractional_Fixed_Ops      : constant Boolean := False;
   Frontend_Layout           : constant Boolean := False;
   Machine_Overflows         : constant Boolean := False;
   Machine_Rounds            : constant Boolean := True;
   Preallocated_Stacks       : constant Boolean := False;
   Signed_Zeros              : constant Boolean := True;
   Stack_Check_Default       : constant Boolean := False;
   Stack_Check_Probes        : constant Boolean := False;
   Stack_Check_Limits        : constant Boolean := False;
   Support_64_Bit_Divides    : constant Boolean := True;
   Support_Aggregates        : constant Boolean := True;
   Support_Composite_Assign  : constant Boolean := True;
   Support_Composite_Compare : constant Boolean := True;
   Support_Long_Shifts       : constant Boolean := True;
   Always_Compatible_Rep     : constant Boolean := True;
   Suppress_Standard_Library : constant Boolean := True;
   Use_Ada_Main_Program_Name : constant Boolean := False;
   ZCX_By_Default            : constant Boolean := False;
   GCC_ZCX_Support           : constant Boolean := False;

end System;
