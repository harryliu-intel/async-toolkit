//
// File automatically generated:
//
// Tool: SOCFuseMap.pl
// Version: 2.26
// User: kmoses1
// Timestamp: Mon Jan 13 11:53:33 2014

`ifndef SOCFUSEGEN_API_VH
`define SOCFUSEGEN_API_VH

//
// ControlLUT Lookup
//
function automatic bit [26:0] SOCFUSEGEN_AC_LOOKUP (bit [14:0] rowaddr);
begin
    case (rowaddr)
// RowAddr : SOCFUSEGEN_AC_LOOKUP = { JTagWR, JTagRD, CSRWR, CSRRD, RD4Err, LLDD, LockoutIDBitPosition, LockoutIDRowAddress };
    15'h0002 : SOCFUSEGEN_AC_LOOKUP = { 1'h0, 1'h0, 1'h0, 1'h0, 1'h0, 1'h0, 5'h00, 16'h0001 };
    15'h0100 : SOCFUSEGEN_AC_LOOKUP = { 1'h0, 1'h0, 1'h0, 1'h0, 1'h0, 1'h0, 5'h01, 16'h0001 };
    15'h0101 : SOCFUSEGEN_AC_LOOKUP = { 1'h0, 1'h0, 1'h0, 1'h0, 1'h0, 1'h0, 5'h02, 16'h0001 };
    default  : SOCFUSEGEN_AC_LOOKUP = { 1'h0, 1'h0, 1'h0, 1'h0, 1'h0, 1'h0, 5'h00, 16'h0000 };
    endcase
end
endfunction


//
// DistrubutionLUT Lookup
//
function automatic bit [89:0] SOCFUSEGEN_DIST_LOOKUP (bit [7:0] SBPortID, bit [2:0] RequestType, bit [9:0] Count);
begin
    case ({ SBPortID, RequestType, Count})
// { IOSFSBPortID, RequestType, Count } : SOCFUSEGEN_DIST_LOOKUP = { PortIndex, RcvrAddr, BAR, RamAddr, DataSize, PullOnceIndex, BlkOnDbg, CmplFmt, LockoutIDBitPosition, LockoutIDRowAddress };
    { 8'haa, 3'b000, 10'h000 } : SOCFUSEGEN_DIST_LOOKUP = { 8'h00, 16'h0000, 3'b001, 16'h0008, 7'h01, 17'h00000, 1'h0, 1'h1, 5'h00, 16'h0001 };
    { 8'haa, 3'b000, 10'h001 } : SOCFUSEGEN_DIST_LOOKUP = { 8'h00, 16'h0001, 3'b001, 16'h0400, 7'h01, 17'h00000, 1'h0, 1'h1, 5'h01, 16'h0001 };
    { 8'haa, 3'b001, 10'h000 } : SOCFUSEGEN_DIST_LOOKUP = { 8'h00, 16'h0002, 3'b001, 16'h0404, 7'h01, 17'h00000, 1'h0, 1'h1, 5'h02, 16'h0001 };
    default                    : SOCFUSEGEN_DIST_LOOKUP = { 8'h00, /* 16'h0000 */ 16'h0000, 3'h0, 16'h0000, 7'h00, 17'h00000, 1'h0, 1'h0, 5'h00, 16'h0000 };
    endcase
end
endfunction


//
// Logical to Physical array index translation function.
// Input is the logical module/array index and the function returns the physical module/array index.
// Returns { errorbit, physical_arrayindex }
//
function automatic bit [10:0] SOCFUSEGEN_COMPACT_ARRAYINDEX (bit [9:0] mod_array);
begin
    bit [9:0] offset;
    bit out_of_range;

    offset = mod_array;
    out_of_range = 1'b0;
    if ({7'h00,3'h0} <= mod_array && mod_array <= {7'h00,3'h0}) begin
        // 0.0 to 0.0 , holes = 0 arrays
        offset = 10'h000;
    end
    else if ({7'h01,3'h0} <= mod_array && mod_array <= {7'h01,3'h0}) begin
        // 1.0 to 1.0 , holes = 7 arrays
        offset = 10'h007;
    end
    else begin
        out_of_range = 1'b1;
    end

    SOCFUSEGEN_COMPACT_ARRAYINDEX = { out_of_range, mod_array - offset };
end
endfunction

// Logical to Physical address translation function
// Input is the logical address (including special row indexing) and returns the physical address.
// Returns { errorbit, physical_address }
function automatic bit [16:0] SOCFUSEGEN_COMPACT_ADDRESS (bit [15:0] addr);
begin
    bit [10:0] cindex;
    cindex = SOCFUSEGEN_COMPACT_ARRAYINDEX(addr[14:5]);
    if (cindex[10])
    SOCFUSEGEN_COMPACT_ADDRESS = { 1'h1, 16'h0 };
    else
    SOCFUSEGEN_COMPACT_ADDRESS = { 1'h0, addr[15:15], cindex[9:0], addr[4:0] };
end
endfunction


//
// SharedID Lookup
//
function automatic bit [7:0] SOCFUSEGEN_SHAREDID_LOOKUP (bit [7:0] SBPortID);
begin
    case (SBPortID)
// { IOSFSBPortID } : SOCFUSEGEN_SHAREDID_LOOKUP = { SharedID };
    default   : SOCFUSEGEN_SHAREDID_LOOKUP = { 8'h00 };
    endcase
end
endfunction


//
// Virtual Fusing Override Disable
//
function automatic bit SOCFUSEGEN_VF_OVERRIDE_DISABLE (bit [7:0] portid, bit [15:0] addr, bit [6:0] size);
begin
    bit [15:0] addr_end;
    addr_end = addr + size - 1;

    case (portid)
    default : SOCFUSEGEN_VF_OVERRIDE_DISABLE = 0;
    endcase
end
endfunction


//
// Fuse Controller Configuration structure
//

typedef struct packed {
        bit [31:0] __dummy__filler;

} FUSECTRLCFG;


`endif
