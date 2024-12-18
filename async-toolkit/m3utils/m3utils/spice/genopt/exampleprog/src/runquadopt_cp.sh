#!/bin/sh -x

genopt=${M3UTILS}/spice/genopt/chopstix/AMD64_LINUX/chopstix

${genopt} -checkpoint $1 -setparam silly 12 -S defs.scm examplequadopt.scm

