/*
 * Interrupt_Vectors.s
 *
 *
 * Created by Patrick Bernardi on 15/04/11.
 * Copyright 2011 Patrick Bernardi All rights reserved.
 */
		.extern __OTS_Context_Switch_To_Kernel
		.extern __OTS_Context_Switch_To_Task
		.extern __OTS_Decrementer_Interrupt
		.extern __OTS_External_Interrupt

		.global __OTS_CSTT
		.global __OTS_CSTK
		.global __OTS_DI
                .global __OTS_EI

		.section ".acton_intr_branch_table", "ax"
		.equ ALIGN_OFFSET, 4

ActonInterruptBranchTable:
__IVPR:
		.align ALIGN_OFFSET
__OTS_CSTK:	b	__OTS_Context_Switch_To_Kernel
		.align ALIGN_OFFSET
__OTS_CSTT:	b	__OTS_Context_Switch_To_Task
		.align ALIGN_OFFSET
__OTS_DI:	b	__OTS_Decrementer_Interrupt
		.align ALIGN_OFFSET
__OTS_EI:       b       __OTS_General_Interrupt_Handler
