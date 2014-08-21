//
//  init.s
//  
//
//  Created by Patrick Bernardi on 15/08/2014.
//
//

	.syntax unified
	.cpu cortex-m4
	.thumb

    .global _start

	/* load .data into ram */
_start:
	movw	r0,#:lower16:__data_start
	movt	r0,#:upper16:__data_start
	movw	r1,#:lower16:__data_size
	movw	r2,#:lower16:__data_load
	movt	r2,#:upper16:__data_load
	cbz	r1,clear_bss
load_loop:
	ldr	r4,[r2],#4
	str	r4,[r0],#4
	subs	r1,r1,#1
	bne	load_loop

clear_bss:
	/* Clear .bss */
	movw	r0,#:lower16:__bss_start
	movt	r0,#:upper16:__bss_start
	movw	r1,#:lower16:__bss_size
	mov	r2,#0
	cbz	r1,branch_to_main
clear_loop:
	str	r2,[r0],#4
	subs	r1,r1,#1
	bne	clear_loop

branch_to_main:
    bl  microcontroller_setup
    ldr r0, =main
	bx  r0
    b  .
