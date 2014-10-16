# Oak_Get_Clock, derived from EREF: Reference Manual for
# Freescale Embedded Processors (Including the e200 and
# e500 Families) - EREFRM Rev 1, p6-4

# GCC on the Power Architecture returns double word
# integers with the most signifcant bit in r3
# and the least significant bit in r4

.global oak_get_clock

# Note below, even though we only use 4 bytes, we take 8 for the case where
# a decrementer interrupt occurs, as it requires the stack to be double word
# aligned.

oak_get_clock:
	mftbu  r3                   # load from TBU into r3
	mftbl  r4                   # load from TBL into r4
	mftbu  r5                   # load from TBU into r5
	cmp    CR0, 0, r5, r3       # see if 'old TBU' = 'new TBU'
	bc     4, 2, oak_get_clock  # loop if carry has occured
	blr
