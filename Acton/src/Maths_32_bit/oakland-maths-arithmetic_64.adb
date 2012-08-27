with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;

package body Oak.Maths.Arithmetic_64 is
   pragma Suppress (Overflow_Check);
   pragma Suppress (Range_Check);

   function To_Uns is new Ada.Unchecked_Conversion (Integer_64, Unsigned_64);
   function To_Int is new Ada.Unchecked_Conversion (Unsigned_64, Integer_64);

   function "&" (Hi, Lo : Unsigned_32) return Unsigned_64 with Inline;
   --  Concatenate hi, lo values to form 64-bit result

   function Count_Trailing_Zeros
     (X : Unsigned_32)
      return Unsigned_32 with Inline;

   pragma Import (Intrinsic, Count_Trailing_Zeros, "__builtin_ctz");

   function Count_Leading_Zeros
     (X : Unsigned_32)
      return Unsigned_32 with Inline;

   pragma Import (Intrinsic, Count_Leading_Zeros, "__builtin_clz");

   function Hi (X : Unsigned_64) return Unsigned_32 with Inline;

   function Lo (X : Unsigned_64) return Unsigned_32 with Inline;

   function "&" (Hi, Lo : Unsigned_32) return Unsigned_64 is
   begin
      return Shift_Left (Unsigned_64 (Hi), 32) or Unsigned_64 (Lo);
   end "&";

   function Hi (X : Unsigned_64) return Unsigned_32 is
   begin
      return Unsigned_32 (Shift_Right (X, 32));
   end Hi;

   function Lo (X : Unsigned_64) return Unsigned_32 is
   begin
      return Unsigned_32 (X and 16#FFFF_FFFF#);
   end Lo;

   function Integer_64_Divide
     (A, B : Interfaces.Integer_64)
      return Interfaces.Integer_64
   is
      Bits_In_Double_Word : constant := Integer_64'Size;

      X, Y                : Unsigned_64;

      S_A : Unsigned_64 := Shift_Right (To_Uns (A), Bits_In_Double_Word);
      S_B : constant Unsigned_64 := Shift_Right (To_Uns (B),
                                                  Bits_In_Double_Word);
      --  S_A = if A < 0 then -1 else 0
      --  S_B = if A < 0 then -1 else 0

   begin
      X := (To_Uns (A) xor S_A) - S_A; --  negate if S_A = -1
      Y := (To_Uns (B) xor S_B) - S_B; --  negate if S_B = -1
      S_A := S_A xor S_B;              --  sign of quotient

      --  negate if s_a = - 1
      return To_Int ((Unsigned_64_Divide (X, Y, null) xor S_A) - S_A);
   end Integer_64_Divide;

   function Unsigned_64_Divide
     (A, B      : Interfaces.Unsigned_64;
      Remainder : access Interfaces.Unsigned_64 := null)
      return Interfaces.Unsigned_64
   is

      procedure Remaind (R : Unsigned_64);
      --  Stores the remainder R if it is present.

      procedure Remaind (R : Unsigned_64) is
      begin
         if Remainder /= null then
            Remainder.all := R;
         end if;
      end Remaind;

      Ahi : constant Unsigned_32 := Hi (A);
      Alo : constant Unsigned_32 := Lo (A);

      Bhi : constant Unsigned_32 := Hi (B);
      Blo : constant Unsigned_32 := Lo (B);

      Q, R : Unsigned_64;
      SR   : Unsigned_32;

      A_Word_Bits        : constant Unsigned_32 := Unsigned_32'Size;
      A_Double_Word_Bits : constant Unsigned_32 := Unsigned_32'Size;

   begin
      --  Special cases where X is unkown and K /= 0

      if Ahi = 0 then
         if Bhi = 0 then
            --  case:   0 X
            --        ÷ 0 X

            Remaind (0 & (Alo rem Blo));
            return (0 & (Alo / Blo));
         end if;

         --  case:   0 X
         --        ÷ K X

         Remaind (0 & Alo);
         return 0;
      end if;

      --  Case when Ahi /= 0

      if Blo = 0 then
         if Bhi = 0 then
            --  case:   K X
            --        ÷ 0 0

            Remaind (0 & (Ahi rem Blo));
            return (0 & (Ahi / Blo));
         end if;

         if Alo = 0 then
            --  case:   K 0
            --        ÷ K 0
            Remaind ((Ahi rem Bhi) & 0);
            return ((Ahi / Bhi) & 0);
         end if;

         --  case:   K K
         --        ÷ K 0

         --  case B is a power of 2
         if B mod 2 = 0 then
            Remaind ((Ahi and (Bhi - 1)) & Alo);
            return ((Shift_Right (Ahi, Natural (Count_Trailing_Zeros (Bhi))))
                    & 0);
         end if;

         --  case B not a power of 2
         SR := Count_Leading_Zeros (Bhi) - Count_Leading_Zeros (Ahi);

         --  0 <= sr <= A_Word_Bits - 2 or sr large
         if SR  > A_Word_Bits - 2 then
            Remaind (A);
            return 0;
         end if;

         SR := SR + 1;
         --  1 <= sr <= A_Word_Bits - 1
         Q := Shift_Right (A, Natural (A_Double_Word_Bits - SR));
         R := Shift_Left (A, Natural (SR));

      --  Blo /= 0
      else

         if Bhi = 0 then
            --  case:   K K
            --        ÷ 0 K

            --  case B is a power of 2
            if B mod 2 = 0 then
               Remaind (0 & (Alo and (Blo - 1)));
               if Blo = 1 then
                  return A;
               end if;

               return Shift_Left (A, Natural (Count_Trailing_Zeros (Blo)));
            end if;

            --  case B not a power of 2

            SR := 1 + A_Word_Bits + Count_Leading_Zeros (Blo) -
              Count_Leading_Zeros (Ahi);
            Q := Shift_Left (A, Natural (A_Double_Word_Bits - SR));
            R := Shift_Right (A, Natural (SR));

         else
            --  case:   K X
            --        ÷ K K

            SR := Count_Leading_Zeros (Bhi) - Count_Leading_Zeros (Ahi);

            --  0 <= sr <= A_Word_Bits - 1 or sr large
            if SR > A_Word_Bits - 1 then
               Remaind (A);
               return 0;
            end if;

            SR := SR + 1;

            --  1 <= sr <= A_Word_Bits
            Q := Shift_Left (A, Natural (A_Double_Word_Bits - SR));
            R := Shift_Right (A, Natural (SR));
         end if;
      end if;

      --  Not a special case.
      --  Q and R are initalised with:
      --    Q := Shift_Left (A, A_Double_Word_Bits - SR);
      --    R := Shift_Right (A >> SR);

      declare
         Carry : Unsigned_64 := 0;
      begin
         while SR > 0 loop
            R := Shift_Left (R, 1) or Carry;
            Q := Shift_Left (Q, 1) or Carry;
            Carry := 0;
            if R >= B then
               R := R - B;
               Carry := 1;
            end if;

            SR := SR - 1;
         end loop;

         Q  := Shift_Left (Q, 1) or Carry;

         Remaind (R);
      end;
      return Q;
   end Unsigned_64_Divide;

end Oak.Maths.Arithmetic_64;
