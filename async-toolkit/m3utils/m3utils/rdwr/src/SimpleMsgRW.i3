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
 * Created On      : Wed Apr 19 10:16:48 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Sun Feb  4 14:00:51 1996
 * Update Count    : 4
 * 
 * SCCS Status     : @(#)SimpleMsgRW.i3	1.1	02/04/96
 * 
 * HISTORY
 *
 * Originally was the ConnMsgRW module in the TCP package. 
 * A small number of modifications allowed it to be adapted to sit on
 * generic Rd.T and Wr.T objects, instead of ConnFD.T objects used by
 * TCP.  The burning question is, why wasn't this done before???
 *
 * The original header in ConnMsgRW.i3 is here: *)
(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Dec  2 16:40:57 PST 1992 by wobber *)

INTERFACE SimpleMsgRW;

IMPORT Rd, Wr, MsgRd, MsgWr;

PROCEDURE NewRd(rd: Rd.T) : MsgRd.T;
   (* produces a message reader from a generic Rd.T *)

PROCEDURE NewWr(wr: Wr.T) : MsgWr.T;
   (* produces a message writer from a generic Wr.T *)

END SimpleMsgRW.

