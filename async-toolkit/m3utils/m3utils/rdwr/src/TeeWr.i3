(* 
 * This library is free software; you can redistribute it and/or          
 * modify it under the terms of the GNU Library General Public            
 * License as published by the Free Software Foundation.                  
 * This library is distributed in the hope that it will be useful,        
 * but WITHOUT ANY WARRANTY; without even the implied warranty of         
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      
 * Library General Public License for more details.                       
 * If you do not have a copy of the GNU Library General Public            
 * License, write to The Free Software Foundation, Inc.,                  
 * 675 Mass Ave, Cambridge, MA 02139, USA.                                
 *                                                                        
 * For more information on this program, contact Blair MacIntyre          
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 500 W 120th St, Room 450, New York, NY, 10027.                         
 *                                                                        
 * Copyright (C) Blair MacIntyre 1995, Columbia University 1995           
 * 
 * Author          : Blair MacIntyre
 * Created On      : Wed Mar  1 20:04:22 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sun Feb  4 14:01:28 1996
 * Update Count    : 11
 * 
 * SCCS Status     : @(#)TeeWr.i3	1.1	02/04/96
 * 
 * HISTORY
 *)

INTERFACE TeeWr;

IMPORT Text, Wr, Thread;

TYPE
  T <: Public;
  Public = Wr.T OBJECT 
  METHODS 
    init (): T;

    (* add the named writer to the output list. *)
    tee (name: Text.T; wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted};
    
    (* remove a named writer from the output list. *)
    untee (name: Text.T): Wr.T RAISES {Wr.Failure, Thread.Alerted};
  END;

  (* An initialized TeeWr.T returned by NEW(T).init() is an 
     output stream with which copies all it's output to all of it's
     output writers. If there are no writers currently on the output
     list, the TeeWr.T behaves like a NullWr.T  *)

END TeeWr. 
