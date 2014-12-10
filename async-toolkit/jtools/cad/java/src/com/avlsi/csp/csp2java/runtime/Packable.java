package com.avlsi.csp.csp2java.runtime;

public interface Packable {
    int pack(CspInteger packed, int start);
    int unpack(CspInteger packed, int start);
}
