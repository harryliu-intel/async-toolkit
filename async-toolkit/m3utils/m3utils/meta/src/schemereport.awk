#!/usr/bin/awk -f
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


{
	path=$1;
	margin=$2;

	printf("MARGIN %80s %10.4f\n","",margin);
	
	scm_path=path ".scm"
	out_path=scm_path ".out"

	print "\nScheme code:\n"

	while (getline x < scm_path > 0) print x;

	scr_path=path ".script"
		
	print "\nrun script:\n"

	cmd = "grep go.scm " scr_path;

        while (cmd | getline x) print x;

	print "\npredecessors:\n"

	cmd2 = "grep PREDECESSORS " out_path;

        while (cmd2 | getline x) print x;

	printf("\n\n\n");

	close(scm_path);
	close(cmd);
	close(cmd2);


}	


