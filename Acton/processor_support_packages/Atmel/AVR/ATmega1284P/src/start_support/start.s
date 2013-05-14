/* Copyright (c) 2002, Marek Michalkiewicz <marekm@amelek.gda.pl>
   Copyright (c) 2007, 2008 Eric B. Weddington
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in
     the documentation and/or other materials provided with the
     distribution.

   * Neither the name of the copyright holders nor the names of
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
   LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE. */

/* Start file for ATmega1284P */

        .equ SPL,    0x3d
        .equ SPH,    0x3e
        .equ SREG,   0x3f
        .equ RAMPZ,  0x3b

        .equ RAMEND, (16 * 1024)

      .section ".vectors","ax"
      .global __reset
__vectors:
__reset:
      jmp  __init
      jmp  __PSP_Vector_2
      jmp  __PSP_Vector_3
      jmp  __PSP_Vector_4
      jmp  __PSP_Vector_5
      jmp  __PSP_Vector_6
      jmp  __PSP_Vector_7
      jmp  __PSP_Vector_8
      jmp  __PSP_Vector_9
      jmp  __Clock_Interrupt
      jmp  __PSP_Vector_11
      jmp  __PSP_Vector_12
      jmp  __PSP_Vector_13
      jmp  __PSP_Vector_14
      jmp  __PSP_Vector_15
      jmp  __PSP_Vector_16
      jmp  __PSP_Vector_17
      jmp  __PSP_Vector_18
      jmp  __PSP_Vector_19
      jmp  __PSP_Vector_20
      jmp  __PSP_Vector_21
      jmp  __PSP_Vector_22
      jmp  __PSP_Vector_23
      jmp  __PSP_Vector_24
      jmp  __PSP_Vector_25
      jmp  __PSP_Vector_26
      jmp  __PSP_Vector_27
      jmp  __PSP_Vector_28
      jmp  __PSP_Vector_29
      jmp  __PSP_Vector_30
      jmp  __PSP_Vector_31
      jmp  __PSP_Vector_32
      jmp  __PSP_Vector_33
      jmp  __PSP_Vector_34
      jmp  __PSP_Vector_35

	.section .init0,"ax",@progbits
	.weak	__init
__init:
	.weak	__stack

	/* By default, malloc() uses the current value of the stack pointer
	   minus __malloc_margin as the highest available address.

	   In some applications with external SRAM, the stack can be below
	   the data section (in the internal SRAM - faster), and __heap_end
	   should be set to the highest address available for malloc().  */
	.weak	__heap_end
	.set	__heap_end, 0

	.section .init2,"ax",@progbits
	clr	r1
	out	SREG, r1
	ldi	r28,lo8(__stack)
	ldi	r29,hi8(__stack)
	out	SPH, r29
	out	SPL, r28

	/* Only for >64K devices with RAMPZ, replaces the default code
	   provided by libgcc.S which is only linked in if necessary.  */

	.section .init4,"ax",@progbits
	.global __do_copy_data
__do_copy_data:
	ldi	r17, hi8(__data_end)
	ldi	r26, lo8(__data_start)
	ldi	r27, hi8(__data_start)
	ldi	r30, lo8(__data_load_start)
	ldi	r31, hi8(__data_load_start)
	ldi	r16, hh8(__data_load_start)
	out	RAMPZ, r16
	rjmp	.L__do_copy_data_start

.L__do_copy_data_loop:
	elpm	r0, Z+
	st	X+, r0

.L__do_copy_data_start:
	cpi	r26, lo8(__data_end)
	cpc	r27, r17
	brne	.L__do_copy_data_loop

	.set	__stack, RAMEND

	.section .init9,"ax",@progbits
/* Reset the MCU status register - Crude, but works, should really
   set latter on */
        out     0x24, r1
	call	main
	jmp	exit


