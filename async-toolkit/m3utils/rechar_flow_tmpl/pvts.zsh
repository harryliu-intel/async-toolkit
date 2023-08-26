# the characterization PVTs to run

# for now we support: a list of voltages (in millivolts)
# a single temperature

# can change in future!

# corners are given in temp/millivolts/trancorner/capcorner/metaltemp format
# metaltemp given as "m40" or "100" or whatnot
# note that corners given must also be mapped in run_postprocess

export corners_list=(100/300/tttt/cmax/100)

export cell_list_fn="cell_list"
