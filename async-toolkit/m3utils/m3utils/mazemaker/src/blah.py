# Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
# SPDX-License-Identifier: Apache-2.0

import sys

class P:
    def _init_(self, x, y):
        self.x = x
        self.y = y

def U(t):
    sys.stdout.write(t + " ")

def CR():
    sys.stdout.write("\n")

class Mode:
    Maze = 0
    Empty = 1

rs = int(input("Enter the number of rows: "))
cs = int(input("Enter the number of columns: "))
mode = Mode.Empty

print(rs)
CR()
print(cs)
CR()

for r in range(rs):
    for c in range(cs):
        if P(r, c) == P(0, 0):
            U("s")
        elif r == rs - 1 and ((r % 4 == 0 and c == cs - 1) or
                              (r % 4 == 1 and c == cs - 1) or
                              (r % 4 == 2 and c == 0) or
                              (r % 4 == 3 and c == 0)):
            U("g")
        elif mode == Mode.Maze and r % 4 == 1 and c != cs - 1:
            U("x")
        elif mode == Mode.Maze and r % 4 == 3 and c != 0:
            U("x")
        else:
            U("_")
    CR()
