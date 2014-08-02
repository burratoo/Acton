with Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;

--  From AdaCore's website I think.

package body Oak.Memory.Ops is
   pragma Suppress (All_Checks);

   function Mem_Compare (dest, src : Address;
                         n         : size_t) return unsigned_char is
      subtype mem is char_array (size_t);
      type memptr is access mem;
      function to_memptr is
         new Ada.Unchecked_Conversion (Address, memptr);
      function To_Unsigned_Char is
         new Ada.Unchecked_Conversion (char, unsigned_char);
      dest_p : constant memptr := to_memptr (dest);
      src_p  : constant memptr := to_memptr (src);

   begin
      if n > 0 then
         --  need to guard against n=0 since size_t is a modular type
         for J in 0 .. n - 1 loop
            if dest_p (J) /= src_p (J) then
               return To_Unsigned_Char (dest_p (J)) -
                 To_Unsigned_Char (src_p (J));
            end if;
         end loop;
      end if;

      return 0;

   end Mem_Compare;

   function Mem_Copy (dest, src : Address;
                      n         : size_t) return Address is
   begin
      if n > 0 then
         if src mod 4 /= 0 or else dest mod 4 /= 0 or else n mod 4 /= 0 then
            --  need to guard against n=0 since size_t is a modular type
            declare
               subtype Mem is char_array (size_t);
               type Memptr is access Mem;
               function To_Memptr is
                 new Ada.Unchecked_Conversion (Address, Memptr);
               Dest_P : constant Memptr := To_Memptr (dest);
               Src_P  : constant Memptr := To_Memptr (src);
            begin
               for J in 0 .. n - 1 loop
                  Dest_P (J) := Src_P (J);
               end loop;
            end;

         else
            declare
               type Mem is array (size_t) of Unsigned_32;
               type Memptr is access Mem;
               function To_Memptr is
                 new Ada.Unchecked_Conversion (Address, Memptr);
               Dest_P : constant Memptr := To_Memptr (dest);
               Src_P  : constant Memptr := To_Memptr (src);
            begin
               for J in 0 .. n / 4 - 1 loop
                  Dest_P (J) := Src_P (J);
               end loop;
            end;
         end if;
      end if;

      return dest;
   end Mem_Copy;

   function Mem_Move (dest, src : Address;
                      n         : size_t) return Address is
      subtype mem is char_array (size_t);
      type memptr is access mem;
      function to_memptr is
         new Ada.Unchecked_Conversion (Address, memptr);
      dest_p : constant memptr := to_memptr (dest);
      src_p  : constant memptr := to_memptr (src);

   begin
      if n > 0 then
         --  need to guard against n=0 since size_t is a modular type

         if (src < dest) and (dest < src + Storage_Offset (n)) then
            --  Copy backwards
            for J in reverse 0 .. n - 1 loop
               dest_p (J) := src_p (J);
            end loop;
         else
            for J in 0 .. n - 1 loop
               dest_p (J) := src_p (J);
            end loop;
         end if;
      end if;

      return dest;
   end Mem_Move;

   function Mem_Set (Mem_Loc    : Address;
                     With_Value : char;
                     Length     : size_t) return Address is
      subtype mem is char_array (size_t);
      type memptr is access mem;
      function to_memptr is
        new Ada.Unchecked_Conversion (Address, memptr);
      Mem_Access : constant memptr := to_memptr (Mem_Loc);
   begin
      if Length > 0 then
         --  need to guard against Length=0 since size_t is a modular type
         for J in 0 .. Length - 1 loop
            Mem_Access (J) := With_Value;
         end loop;
      end if;

      return Mem_Loc;
   end Mem_Set;
end Oak.Memory.Ops;
