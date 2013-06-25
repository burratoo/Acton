with System; use System;
with Interfaces.C; use Interfaces.C;

package Oak.Memory.Ops with Pure is
   function Mem_Compare (dest, src : Address;
                         n         : size_t) return unsigned_char;
   pragma Export (C, Mem_Compare, "memcmp");

   function Mem_Copy (dest, src : Address;
                      n         : size_t) return Address;
   pragma Export (C, Mem_Copy, "memcpy");

   function Mem_Move (dest, src : Address;
                      n         : size_t) return Address;
   pragma Export (C, Mem_Move, "memmove");

   function Mem_Set (Mem_Loc    : Address;
                     With_Value : char;
                     Length     : size_t) return Address
     with Export, Convention => C, External_Name => "memset";
end Oak.Memory.Ops;
