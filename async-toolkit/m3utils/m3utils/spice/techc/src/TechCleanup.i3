(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TechCleanup;
IMPORT Pathname;

PROCEDURE DeleteMatching(dir     : Pathname.T;
                         regEx   : TEXT);

PROCEDURE DeleteRecursively(workdir, subdir : Pathname.T);

PROCEDURE CompressFilesWithExtension(dir : Pathname.T; ext : TEXT);

PROCEDURE CompressFile(fn : Pathname.T);

END TechCleanup.
