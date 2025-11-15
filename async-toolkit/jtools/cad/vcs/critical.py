#!/usr/intel/bin/python
# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0


import sys
import os   
import glob
import argparse
import pprint

class OutputNode:
    def __init__(self, name):
        self.name = name
        self.times = list()
        self.last_idx = 0
        self.worst_idx = 0

    def __str__(self):
        return "%s with %d transistions, worst case @ %d (idx=%d)" % (self.name, len(self.times), self.times[self.worst_idx][0], self.worst_idx)

    def add_transition(self, output_time, input_name, input_time):
        self.times.append((output_time, input_name, input_time))
        #print("Adding transition to %s: %d caused %d (%d)" % (self.name, output_time, input_time, output_time - input_time))
        if (output_time - input_time) > (self.times[self.worst_idx][0] - self.times[self.worst_idx][2]) or self.times[self.worst_idx][2] == 0:
            self.worst_idx = self.last_idx
        if len(self.times) > 1:
            self.last_idx += 1
    
    def get_transition(self, time):
        #print("  Getting transition for %s at time %d" % (convert_vcs_to_apr_name(self.name), time))
        for idx in range(self.last_idx, -1, -1):
            #print("    Comparing times[%d] = %d" % (idx, self.times[idx][0]))
            if self.times[idx][0] <= time:
                self.last_idx = idx
                #print("      Found Critical transition %s: %d -> %d (%d)" % (convert_vcs_to_apr_name(self.times[idx][1]), self.times[idx][2], self.times[idx][0], self.times[idx][0] - self.times[idx][2]))
                return self.times[idx]
        print("WARNING: No more transitions found for time %f" % time)
        return None
    
    def get_forward_transition_count(self, time):
        for idx in range(self.last_idx, len(self.times)):
            if self.times[idx][0] >= time:
                return idx - self.last_idx
        return 0
        
    def get_worst_transition(self):
        return self.times[self.worst_idx]

    def get_first_transition(self):
        return self.times[0]
    
    def get_last_transition(self):
        return self.times[self.last_idx]


def convert_apr_to_vcs_name(apr_name):
    apr_name = apr_name.strip()
    vcs_name = "TESTBENCH.x._$0"
    for tok in apr_name.split('/'):
        if '.' in tok or '[' in tok or ']' in tok:
            #Need to escape this token
            tok = "\\" + tok + " "
        vcs_name = vcs_name + "." + tok
    #print("Converted %s to %s" % (apr_name, vcs_name))
    return vcs_name

def convert_vcs_to_apr_name(vcs_name):
    vcs_name = vcs_name.replace("TESTBENCH.x._$0.", "")
    apr_toks = list()
    newtok = ""
    for tok in vcs_name.split('.'):
        if tok.startswith("\\"):
            if tok.endswith(" "):
                apr_toks.append(tok[1:-1])
                newtok = ""
            else:
                newtok = tok[1:]
            continue
        if len(newtok) > 0:
            if tok.endswith(" "):
                newtok += ".%s" % tok[:-1]
                apr_toks.append(newtok)
                newtok = ""
            else:
                newtok += ".%s" % tok
            continue
        else:
            apr_toks.append(tok)
    apr_name = '/'.join(apr_toks)
    #print("Converted %s to %s" % (vcs_name, apr_name))
    return apr_name

def convert_vcs_to_apr_pin_name(vcs_pin):
    apr_pin = convert_vcs_to_apr_name(vcs_pin)
    apr_pin = apr_pin.split('/')[-1]
    return apr_pin

def cycle_check(path, cur_node, cur_time, verbose=False):
    for node in reversed(path):
        if cur_node == node[0]:
            #print("  Cycle detected at %s: %d - %d = %d" % (cur_node, node[1], cur_time, node[1] - cur_time))
            return (node[1] - cur_time)
    return -1


def parse_hist_file(hist_file, verbose=False):
    """
    Processes the transition history log file.

    Args:
        hist_file (file): The file of the transition history, which contains records
                         of output and input data with their respective times.

    The function reads the transition history file, parses each line to extract
    output and input names along with their times, and prints this information.
    """
    node_dict = dict()
    line_num = 1
    for line in hist_file:
        line = line.strip()
        (output_data,input_data) = line.split(':')
        (output_name, output_time) = output_data.split('@')
        (input_name, input_time) = input_data.split('@')
        #print("%s %s %s %s" % (output_name, output_time, input_name, input_time))
        if output_name not in node_dict:
            node_dict[output_name] = OutputNode(output_name)
        node_dict[output_name].add_transition(int(output_time), input_name, int(input_time))
        line_num += 1
    return node_dict


def parse_conn_file(conn_file, verbose=False):
    """
    Processes the connectivity file.

    Args:
        conn_file (str): The file of the connectivity information, which contains
                         records input nodes and their fanin

    The function reads the connectivity file, parses each line to extract input nodes
    and their fanin information, returning a dictionary of input nodes and their fanin
    """
    conn_dict = dict()
    for line in conn_file:
        line = line.strip('\n')
        #If line starts with comment skip it
        if line.startswith('//'):
            continue
        if ':' in line:
            (input_node,fanin_node) = line.split(':')
        else:
            (input_node,fanin_node) = line.split(' ')
        #print(" Connectivity %s -> %s" % (input_node,fanin_node))
        if "TESTBENCH" in input_node:
            if input_node not in conn_dict:
                conn_dict[input_node] = fanin_node
            else:
                print("WARNING: Duplicate input node %s" % convert_apr_to_vcs_name(input_node))
        else:
            if convert_apr_to_vcs_name(input_node) not in conn_dict:
                conn_dict[convert_apr_to_vcs_name(input_node)] = convert_apr_to_vcs_name(fanin_node)
            else:
                print("WARNING: Duplicate input node %s" % convert_apr_to_vcs_name(input_node))
    return conn_dict


def critical_path(hist_name, connect_name, start, depth, log_name, verbose=False):
    #Read the transition history file
    hist_file = open(hist_name, 'r')
    node_dict = parse_hist_file(hist_file, verbose)
    hist_file.close()

    #Read the connectivity file
    connect_file = open(connect_name, 'r')
    conn_dict = parse_conn_file(connect_file, verbose)
    connect_file.close()

    #Create log file
    log_file = open(log_name, 'w')

    if '@' in start:
        (start_node, start_time) = start.split('@')
        start_time = int(start_time)
    else:
        start_node = start
        start_time = -1
    if "TESTBENCH" not in start_node:
        start_node = convert_apr_to_vcs_name(start_node)
    if start_node not in node_dict:
        print("ERROR: Start node %s not found" % start_node)
        return
    print("Crtitical path starting at %s @ %d" % (start_node, start_time))
    log_file.write("Crtitical path starting at %s @ %d\n" % (start_node, start_time))
    print(node_dict[start_node])
    if start_time == -1:
        (cur_time, cur_in_node, cur_in_time) = node_dict[start_node].get_last_transition()
        cur_node = start_node
    else:
        first_time = node_dict[start_node].get_first_transition()[0]
        if start_time < first_time:
            print("ERROR: Given start time %d is earlier than first logged transition time %d for node %s" % (start_time, first_time, start_node))
            return
        else:
            (cur_time, cur_in_node, cur_in_time) = node_dict[start_node].get_transition(start_time)
            cur_node = start_node

    # Find the critical path
    max_depth = int(depth)
    cur_depth = 0
    path = list()
    while cur_depth < max_depth:
        cycle_time = cycle_check(path, cur_node, cur_time)
        if cycle_time > 0:
            transition_count = node_dict[cur_node].get_forward_transition_count(cur_time+cycle_time)
            log_file.write("%2d: %d, %4d, %4d, %s, %s +Cycle %d/%d=%.1f\n" % (cur_depth, cur_time, path[-1][2]-cur_time, cur_time-cur_in_time, convert_vcs_to_apr_name(cur_node), convert_vcs_to_apr_pin_name(cur_in_node), cycle_time, transition_count, 1.0*cycle_time/transition_count))
        else:
            if cur_depth == 0:
                log_file.write("%2d: %d, %4d, %4d, %s, %s\n" % (cur_depth, cur_time, 0, cur_time-cur_in_time, convert_vcs_to_apr_name(cur_node), convert_vcs_to_apr_pin_name(cur_in_node)))
            else:
                log_file.write("%2d: %d, %4d, %4d, %s, %s\n" % (cur_depth, cur_time, path[-1][2]-cur_time, cur_time-cur_in_time, convert_vcs_to_apr_name(cur_node), convert_vcs_to_apr_pin_name(cur_in_node)))
        path.append((cur_node, cur_time, cur_in_time))
        cur_depth += 1
        # Check if the current node exists in the connectivity dictionary
        if cur_in_node not in conn_dict:
            # Current node doesn't exist, gracefully exit
            log_file.write("%2d: No connectivitiy found for %s\n" % (cur_depth, convert_vcs_to_apr_name(cur_in_node)))
            print("WARNING: Input node %s not found in connectivity dictionary. Ending trace..." % cur_in_node)
            break
        cur_node = conn_dict[cur_in_node]
        (cur_time, cur_in_node, cur_in_time) = node_dict[cur_node].get_transition(cur_in_time)
    log_file.close()



def main():
    parser = argparse.ArgumentParser(description='Identify the critical path.')
    parser.add_argument('hist_file', type=str, help="Pointer to the transition history file")
    parser.add_argument('connect_file', type=str, help="Pointer to the netlist connectivity file")
    parser.add_argument('start', type=str, help="Starting (output) node at starting time")
    parser.add_argument('-d', '--depth', type=int, default=100, help="Max critical path trace depth")
    parser.add_argument('-v', '--verbose', action="store_true", default=False, help="Verbose output")
    parser.add_argument('-l', '--log', type=str, default="crit.log", help="Print output to log file")
    args = parser.parse_args()

    #Call the main function
    critical_path(args.hist_file, args.connect_file, args.start, args.depth, args.log, args.verbose)


if __name__ == "__main__":
    main()

