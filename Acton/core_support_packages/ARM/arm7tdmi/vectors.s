@  This is the vector set up for an AT91SAM7

            .section .arm_vector_table,"ax"
	        .code 32
	        .align 0

            .global swi_vector

            ldr   pc,v0	 	 	@ reset vector
            ldr   pc,v1	  		@ Undefined Instruction
            ldr   pc,swi_vector @ Software Interrupt
            ldr   pc,v3	  		@ Prefetch Abort
            ldr   pc,v4		  	@ Data Abort
            ldr   pc,v5		  	@ reserved
            ldr   pc,irq_vector @ IRQ : read the AIC
            ldr   pc,fiq_vector @ FIQ : read the AIC
v0:         .long _start
v1:         .long undef_handler
swi_vector: .long 0 @ will be loaded later
v3:         .long prefetch_abort_handler
v4:         .long data_abort_handler
v5:         .long reserved_handler
irq_vector: .long irq_handler
fiq_vector: .long fiq_handler
