package com.fulcrummicro.hw.verification.util.concurrent;

import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicInteger;

public class BinarySemaphore {

    private final AtomicInteger permit;

    private final Semaphore sem;

    public BinarySemaphore(boolean available) {
        this.permit = new AtomicInteger(available ? 1 : 0);
        this.sem = new Semaphore(available ? 1 : 0, true);
    }

    public void acquire() throws InterruptedException {
        this.sem.acquire();
        this.ClearPermit();
    }

    public void acquireUninterruptibly() {
        this.sem.acquireUninterruptibly();
        this.ClearPermit();
    }

    public void release() {
        if (this.permit.compareAndSet(0, 1)) {
            this.sem.release();
        }
    }

    public boolean tryAcquire() {
        boolean acquiredPermit;

        acquiredPermit = this.sem.tryAcquire();
        if (acquiredPermit) {
            this.ClearPermit();
        }
        return acquiredPermit;
    }

    private void ClearPermit() {
        if (!this.permit.compareAndSet(1, 0)) {
            throw new InternalError();
        }
    }

}
