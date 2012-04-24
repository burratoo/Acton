/*
 * Default_Handler.s
 *
 *
 * Created by Patrick Bernardi on 24/04/12.
 * Copyright 2012 Patrick Bernardi All rights reserved.
 */

		.global __OI_Default_Handler

		.section ".acton_def_ext_handler", "ax"
__OI_Default_Handler:	b	__OI_Default_Handler
