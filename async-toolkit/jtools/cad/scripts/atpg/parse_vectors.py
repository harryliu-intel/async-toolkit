#!/usr/bin/env python

import sys
import re
import os
import shutil
import argparse

class ScanChain:
    def __init__(self, name):
        self.name = name
        self.chain = list()
    
    def __str__(self):
        return "Chain %s with length %d" % (self.name, len(self.chain))
    
    def __len__(self):
        return len(self.chain)
    
    def add_cell(self, cell):
        self.chain.append(cell)
    
    def print_all_cells(self):
        i = 0
        for cell in self.chain:
            print(f"Cell {i:05d}/{len(self.chain)}: {cell}")
            i += 1
    
    def get_cell_at_index(self, idx):
        return self.chain[idx]
    
    def get_index_of_cell(self, cell): 
        return self.chain.index(cell) #get_last_cell(self):
    

class Vector:
    def __init__(self, num):
        self.num = num
        self.is_unload = False
        self.frc_si_vector = ""
        self.exp_so_vector = ""
        self.msk_so_vector = ""
    
    def __str__(self):
        return f'Vector {self.num} {"[UNLOAD]" if self.is_unload==True else ""} frc_si_vector {len(self.frc_si_vector)} exp_so_vector {len(self.exp_so_vector)} msk_so_vector {len(self.msk_so_vector)}'
    
    def print_stats(self):
        set_fsi = set(self.frc_si_vector)
        set_eso = set(self.exp_so_vector)
        set_mso = set(self.msk_so_vector)

        print(f'Vector {self.num} frc_si_vector contains {sorted(set_fsi)} exp_so_vector contains {sorted(set_eso)} msk_so_vector contains {sorted(set_mso)}')


def parse_scandef_file(scandef_name, log, verbose):
    print("Parsing scandef file: %s" % scandef_name)
    design_name = ""
    num_chains = 0
    line_num = 0
    #Assume a single scan chain
    #chain_dict = dict()
    scan_chain = ScanChain("scan_chain")
    chain_name = ""
    partition_name = ""
    start_point = ""
    stop_point = ""
    cell = ""
    is_in_chain = False
    with open(scandef_name, 'r') as scandef_file:
        for line in scandef_file:
            line_num = line_num + 1
            line = line.strip()
            if len(line) == 0:
                continue
            if line.startswith('//'):
                continue
            if line.startswith("DESIGN"):
                design_name = line.split()[1]
                if verbose:
                    print("Found design %s" % design_name)  
            if line.startswith("SCANCHAINS"):
                num_chains = int(line.split()[1])
                if verbose:
                    print("Found %d chains" % num_chains)
            if line.startswith("END"):
                if line.split()[1] == "SCANCHAINS":
                    print("Finished %s chain(s)" % num_chains)
                if line.split()[1] == "DESIGN":
                    print("Finished %s design" % design_name)
            if line.startswith("-"):
                chain_name = line.split()[1]
                #chain_dict[chain_name] = ScanChain(chain_name)
                if verbose:
                    print("Found chain %s" % chain_name)
            if line.startswith("+ PARTITION"):
                partition_name = line.split()[2]
                if verbose:
                    print("Found partition %s" % partition_name)
            if line.startswith("+ START"):
                is_in_chain = True
                if line.split()[2] == "PIN":
                    start_point = line.split()[3]
                else:
                    start_point = "%s/%s" % (line.split()[2], line.split()[3])
                    cell = line.split()[2]
                if verbose:
                    print("Found start point %s of chain %s, partition %s" % (start_point, chain_name, partition_name))
                if not cell.startswith("BUF_SCAN"):
                    if scan_chain.chain[-1] != cell:
                        scan_chain.add_cell(cell)
                continue
            if line.startswith("+ STOP"):
                is_in_chain = False
                if line.split()[2] == "PIN":
                    stop_point = line.split()[3]
                else:
                    stop_point = "%s/%s" % (line.split()[2], line.split()[3])
                    cell = line.split()[2]
                if verbose:
                    print("Found end point %s of chain %s, partition %s" % (stop_point, chain_name, partition_name))
                if not cell.startswith("BUF_SCAN"):
                    if scan_chain.chain[-1] != cell:
                        scan_chain.add_cell(cell)
                continue
            if is_in_chain:
                if line.startswith("+ ORDERED") or line.startswith("+ FLOATING"):
                    cell  = line.split()[2]
                else:
                    cell = line.split()[0]
                scan_chain.add_cell(cell)
                #print("Adding cell %s to chain %s" % (cell, chain_name))
    scandef_file.close()
    return scan_chain
    
def parse_vector_file(vector_cfg_name, scan_chain, log, verbose):
    print("Parsing vector CFG file: %s" % vector_cfg_name)
    vector_file_name = ""
    vector_length = 0
    vector_count = 0
    with open(vector_cfg_name, 'r') as vector_cfg_file:
        for line in vector_cfg_file:
            line = line.strip()
            if len(line) == 0:
                continue
            if line.startswith('//'):
                continue
            vector_file_name = line.split()[0]
            vector_length = int(line.split()[1])
            vector_count = int(line.split()[2])
    vector_cfg_file.close()
    print("Found vector file %s with length %d and count %d" % (vector_file_name, vector_length, vector_count))

    in_chain_test = False
    in_scan_test = False
    is_unload = False
    cur_vector = ""
    chain_length = len(scan_chain)
    MAX_LINE_LEN = 1024
    pattern_count = 0
    pattern_number = 0
    vector_count = 0
    line_count = 0
    line_num = 0
    end_of_vector = False
    vectors = list()

    with open(vector_file_name, 'r') as vector_file:
        for line in vector_file:
            line_num = line_num + 1
            line = line.strip()
            if len(line) == 0:
                continue
            if line.startswith('//'):
                if line.startswith('// Chain test'):
                    in_chain_test = True
                if line.startswith('// End chain test'):
                    in_chain_test = False
                if line.startswith('// Scan test block'):
                    in_scan_test = True
                if line.startswith('// End scan test block'):
                    in_scan_test = False
                if line.startswith('// Pattern'):
                    pattern_number = int(line.split()[2])
                if line.startswith('// Last unload'):
                    is_unload = True
                    pattern_count = pattern_count + 1
                    vector_count = 0
                    cur_vector = ""
                    line_count = 0
                continue
            
            try:
                comment_idx = line.split().index("//")
                line_vector = line.split()[0]
                line_comment = " ".join(line.split()[comment_idx+1:])
                if verbose:
                    print("Found vector (line %d) with comment \"%s\"" % (line_num, line_comment))
                if line_comment.startswith("Start Pattern"):
                    is_unload = False
                    pattern_count = pattern_count + 1
                    vector_count = 0
                    cur_vector = ""
                    line_count = 0
                    continue
            except ValueError:
                line_vector = line
                line_count = line_count + 1
                if end_of_vector:
                    cur_vector = ""
                    end_of_vector = False
                        
            cur_vector = cur_vector + line_vector
            if len(line_vector) < MAX_LINE_LEN:
                end_of_vector = True
                vector_count = vector_count + 1
                #if verbose:
                #    print("Found a vector (pattern %d, count %d, line %d) with length %d" % (pattern_count, vector_count, line_count, len(cur_vector)))
            
            if vector_count == 3:
                #This is the scan chain vector
                if len(cur_vector) < chain_length:
                    # Vector too short, skipping
                    continue
                vector = Vector(pattern_number)
                vector.frc_si_vector = cur_vector[0:chain_length]
                vector.exp_so_vector = cur_vector[chain_length:2*chain_length]
                vector.msk_so_vector = cur_vector[2*chain_length:3*chain_length]
                #Reverse the vectors to match the scandef
                vector.frc_si_vector = vector.frc_si_vector[::-1]
                vector.exp_so_vector = vector.exp_so_vector[::-1]
                vector.msk_so_vector = vector.msk_so_vector[::-1]
                vector.is_unload = is_unload
                leftover = cur_vector[3*chain_length:]
                if verbose:
                    print(vector)
                    #vector.print_stats()
                vectors.append(vector)
    
    return vectors
            

def print_sequential_forces(scan_chain, vectors, seq_name):
    print("Looking at forces for sequential: %s" % seq_name)
    seq_index = scan_chain.get_index_of_cell(seq_name)

    for vector in vectors:
        if vector.is_unload:
            continue
        print("Vector %d: Sequential %s: Force=%s" % (vector.num, seq_name, vector.frc_si_vector[seq_index]))


def print_ctrl_forces(scan_chain, vectors, ctrl_name):
    print("Looking at forces for BD Controller: %s" % ctrl_name)
    q_fe_name = ctrl_name.replace(".ctrl", ".injectQ._FE")
    q_fe_idx = scan_chain.get_index_of_cell(q_fe_name)
    q_fd_name = ctrl_name.replace(".ctrl", ".injectQ._FD")
    q_fd_idx = scan_chain.get_index_of_cell(q_fd_name)
    a_fe_name = ctrl_name.replace(".ctrl", ".injectA._FE")
    a_fe_idx = scan_chain.get_index_of_cell(a_fe_name)
    a_fd_name = ctrl_name.replace(".ctrl", ".injectA._FD")
    a_fd_idx = scan_chain.get_index_of_cell(a_fd_name)

    for vector in vectors:
        if vector.is_unload:
            continue
        print("Vector %d: Ctrl %s: Q._FE=%s, Q._FD=%s, A._FE=%s, A._FD=%s" % (vector.num, ctrl_name, vector.frc_si_vector[q_fe_idx], vector.frc_si_vector[q_fd_idx], vector.frc_si_vector[a_fe_idx], vector.frc_si_vector[a_fd_idx])) 


def parse_vectors(scandef_file_name, vector_cfg_file_name, ctrl_name, seq_name,log, verbose):
    print("Parsing Scandef file: %s" % scandef_file_name)
    scan_chain = parse_scandef_file(scandef_file_name, log, verbose)
    print(scan_chain)
    #scan_chain.print_all_cells()
    print("Parsing Vector file: %s" % vector_cfg_file_name)
    vectors = parse_vector_file(vector_cfg_file_name, scan_chain, log, verbose)
    if ctrl_name != "":
        print_ctrl_forces(scan_chain, vectors, ctrl_name)
    if seq_name != "":
        print_sequential_forces(scan_chain, vectors, seq_name)



def main():
    parser = argparse.ArgumentParser(description='Parse ATPG vectors.')
    parser.add_argument('scandef_file', type=str, help="Pointer to the scandef file")
    parser.add_argument('vector_cfg_file', type=str, help="Pointer to the ATPG vector CFG file")
    parser.add_argument('--ctrl', type=str, default="",help="BD Controller name")
    parser.add_argument('--seq', type=str, default="",help="Sequential name")
    parser.add_argument('-v', '--verbose', action="store_true", default=False, help="Verbose output")
    parser.add_argument('-l', '--log', type=str, default="atpg_vec.log", help="Print output to log file")
    args = parser.parse_args()

    #Call the main function
    parse_vectors(args.scandef_file, args.vector_cfg_file, args.ctrl, args.seq, args.log, args.verbose)


if __name__ == "__main__":
    main()
