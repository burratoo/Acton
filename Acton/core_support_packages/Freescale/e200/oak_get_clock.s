# Oak_Get_Clock, derived from EREF: Reference Manual for
# Freescale Embedded Processors (Including the e200 and
# e500 Families) - EREFRM Rev 1, p6-4

# GCC on the Power Architecture returns double word
# integers with the most signifcant bit in r3
# and the least significant bit in r4

.global oak_get_clock
oak_get_clock:
        stwu   r1,-4(r1)
        stw    r14,0(r1)
oak_get_clock_loop:
	mftbu  r3                       # load from TBU into r3
	mftbl  r4                       # load from TBL into r4
	mftbu  r14                      # load from TBU into r14
	cmp    CR0, 0, r14, r3          # see if 'old TBU' = 'new TBU'
	bc     4, 2, oak_get_clock_loop # loop if carry has occured
        lwz    r14,0(r1)
        stwu   r1,4(r1)
	blr
