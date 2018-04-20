MODULE StructAssign EXPORTS Main;
IMPORT mby_top_map AS Map;
IMPORT mby_top_map_addr AS MapAddr;
IMPORT IO;
IMPORT CompAddr;
IMPORT Fmt;
IMPORT Debug;
IMPORT RTName;

VAR
  map := NEW(MapAddr.H).init(CompAddr.T { 0, 0 });
  
BEGIN
  IO.Put(Fmt.Int(CompAddr.initCount) & " fields have been address initialized.\n");
  <*ASSERT map # NIL*>
  <*ASSERT map.update.Sched.RxqStorageData[1234].TailCsumLen # NIL*>
  
  map.update.Sched.RxqStorageData[1234].TailCsumLen.u(12);
  (* map.update.Sched is not an object *)
  <*ASSERT map.read.Sched.RxqStorageData[1234].TailCsumLen = 12 *>

  VAR
    sched := map.read.Sched; (* huge data structure on the stack *)
  BEGIN
    Debug.Out("bytesize(hlp) = " & Fmt.Int(BYTESIZE(map.read)));
    Debug.Out("bytesize(sched) = " & Fmt.Int(BYTESIZE(sched)));
    sched.RxqStorageData[1234].TailCsumLen := 13;
    Debug.Out("type="&RTName.GetByTC(TYPECODE(map.update.Sched.u)));
    map.update.Sched.u.u(sched);
    (* if instead of calling it u.u, we called it x.u then the object
       update...TailCsumLen could have two methods: one named u,
       and one named x, which returns itself, allowing a consistent
       calling convention of .....x.u.

       where x is something other than u.  For example "updater" or "uobj".
    *)
  END
END StructAssign.
