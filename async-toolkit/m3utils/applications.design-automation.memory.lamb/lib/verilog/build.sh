#!/bin/sh -x

/p/cth/bin/cth_psetup -p tfc -cfg tfc_n3.cth -tool vcsmx -cmd "vcs -sverilog lambmodel_tb.sv"
