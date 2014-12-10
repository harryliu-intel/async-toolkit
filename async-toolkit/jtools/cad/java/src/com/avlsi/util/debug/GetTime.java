package com.avlsi.util.debug;

public class GetTime
{
    static {
        System.loadLibrary("GetTime");
    }
    /**
     * This class should not be instantiated.
     **/
    private GetTime() { }

    public static native long userCPUTime();
    public static native long systemCPUTime();
    public static native long childrenUserCPUTime();
    public static native long childrenSystemCPUTime();

    public static void main(String[] argv) {
        System.out.println("User CPU time:   " + userCPUTime());
        System.out.println("System CPU time: " + systemCPUTime());
        for(long i=0; i<10000000; i++) {
            /* do nothing */
        }
        System.out.println("User CPU time:   " + userCPUTime());
        System.out.println("System CPU time: " + systemCPUTime());
    }
}
