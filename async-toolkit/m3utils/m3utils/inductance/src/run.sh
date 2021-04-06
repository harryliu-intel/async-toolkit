#!/bin/sh -x

# this uses a random Scheme interpreter with the correct M3 bindings

#EXIT=""
EXIT=exit

../../yield/diesplit/AMD64_LINUX/diesplit -scm make-tests.scm ${EXIT}

mv dut_model.inc ../hspice

