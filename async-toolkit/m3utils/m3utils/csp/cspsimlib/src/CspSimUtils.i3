INTERFACE CspSimUtils;
IMPORT IP;

PROCEDURE ScanIp(str : TEXT) : IP.Address4;

PROCEDURE FmtIp(READONLY addr : IP.Address4) : TEXT;

END CspSimUtils.
