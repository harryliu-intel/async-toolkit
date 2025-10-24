#!/bin/sh -x


# this doesn't work:
#../AMD64_LINUX/parsetiming -f tm_core.tim.max.sum.internal.merged.valid -multi 'End Point' -csv -

# this should work:
../AMD64_LINUX/parsetiming -f tm_core.tim.max.sum.internal.merged.valid -span 'End Point' 3 -span 'Start Point' 3 -csv -
