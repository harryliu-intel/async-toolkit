package com.avlsi.util.debug;

/**
 * Functions that deal with versioning information contained in Fulcrum
 * packages.
 **/
public class VersionInfo {
    /**
     * This class should not be instantiated.
     **/
    private VersionInfo() { }

    public static String getVersionString(final Class theClass) {
        final String className = theClass.getName();
        final Package packageInfo = theClass.getPackage();
        final String versionStr =
            packageInfo == null ? null : packageInfo.getImplementationVersion();

        final String classShortName =
            className.substring(className.lastIndexOf('.') + 1);

        if ( versionStr != null ) {
            return classShortName + " Build: " + versionStr;
        } else {
            return "Unknown " + classShortName + "  Build";
        }
    }
}
