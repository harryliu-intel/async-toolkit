#!/bin/sh -x
schmoozer -design sdg64 -schmooze leakvar 2>&1 | tee leak.out
