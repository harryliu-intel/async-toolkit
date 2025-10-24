(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE FileUpdateWr;
IMPORT Wr;
IMPORT OSError;
IMPORT Pathname;

TYPE T <: Wr.T;

(* A FileUpdateWr.T is like a FileWr.T with the exception that it saves
   the previous contents of the file it is writing to with a different 
   filename until it is done writing the new version.

   When you Wr.Close() the new file, it will perform a comparison and
   check whether the contents have changed.  If they have not, it will
   move the old version back.  

   This serves to retain the modification time, ownership, etc., of the
   original version in case the file has not changed at all.
   
   The code is relatively robust to all manner of errors, and most 
   unexpected situations will simply cause the new file to be retained
   (and modification time, etc., to be updated accordingly).

   Author : Mika Nystrom <mika.nystroem@intel.com>
   January, 2021 (but derived from earlier implementations)
*)
     
PROCEDURE Open(p : Pathname.T; suffix := ".temp") : T RAISES { OSError.E };

CONST Brand = "FileUpdateWr";

END FileUpdateWr.

  
  
