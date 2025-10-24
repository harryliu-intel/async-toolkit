#!/bin/sh -x

genopt=${M3UTILS}/spice/genopt/AMD64_LINUX/chopstix

${genopt} -setparam silly 12 -S defs.scm exampleopt.scm

