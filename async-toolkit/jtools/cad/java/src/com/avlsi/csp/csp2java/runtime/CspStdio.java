/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id: //mrl/sw/intel/cad/java/src/com/avlsi/csp/csp2java/runtime/CspRuntimeAbstractDevice.java#10 $
 * $DateTime: 2017/09/13 22:18:34 $
 * $Author: amlines $
 */
package com.avlsi.csp.csp2java.runtime;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.Closeable;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.BitSet;

public class CspStdio {
    private BitSet openFiles;
    private Closeable[] streams;
    public CspStdio(final int maxFiles) {
        openFiles = new BitSet(maxFiles);
        streams = new Closeable[maxFiles];
    }

    public int fopen(String path, String mode) {
        int idx = openFiles.nextClearBit(0);
        if (idx != -1) {
            Closeable file = null;
            try {
                if (mode.equals("r")) {
                    file = new BufferedInputStream(new FileInputStream(path));
                } else if (mode.equals("w")) {
                    file = new BufferedOutputStream(new FileOutputStream(path));
                } else {
                    idx = -1;
                }
            } catch (IOException e) {
                idx = -1;
            }
            if (idx != -1) {
                openFiles.set(idx);
                streams[idx] = file;
            }
        }
        return idx + 1;
    }

    private boolean checkOpen(int stream) {
        return 0 <= stream && stream <= streams.length && openFiles.get(stream);
    }

    public int fclose(int stream) {
        int ret = -1;
        int idx = stream - 1;
        if (checkOpen(idx)) {
            openFiles.clear(idx);
            try {
                streams[idx].close();
                ret = 0;
            } catch (IOException e) {
            } finally {
                streams[idx] = null;
            }
        }
        return ret;
    }

    public int fread(byte[] ptr, int size, int nmemb, int stream) {
        int ret = 0;
        int idx = stream - 1;
        if (checkOpen(idx) && streams[idx] instanceof InputStream) {
            InputStream is = (InputStream) streams[idx];
            try {
                for (int i = 0; i < nmemb; ++i) {
                    int read = is.read(ptr, i * size, size);
                    if (read != size) {
                        break;
                    }
                    ret++;
                }
            } catch (IOException e) {}
        }
        return ret;
    }

    public int fwrite(byte[] ptr, int size, int nmemb, int stream) {
        int ret = 0;
        int idx = stream - 1;
        if (checkOpen(idx) && streams[idx] instanceof OutputStream) {
            OutputStream os = (OutputStream) streams[idx];
            try {
                for (int i = 0; i < nmemb; ++i) {
                    os.write(ptr, i * size, size);
                    ret++;
                }
            } catch (IOException e) {}
            if (ret > 0) {
                try {
                    os.flush();
                } catch (IOException e) {}
            }
        }
        return ret;
    }

    public void clear() {
        for (int i = 0; i < streams.length; ++i) {
            if (streams[i] != null) {
                try {
                    streams[i].close();
                } catch (IOException e) {
                }
                streams[i] = null;
            }
        }
        openFiles.clear();
    }
}
