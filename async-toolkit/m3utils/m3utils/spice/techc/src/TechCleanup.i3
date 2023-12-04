INTERFACE TechCleanup;
IMPORT Pathname;

PROCEDURE DeleteMatching(dir     : Pathname.T;
                         regEx   : TEXT);

PROCEDURE DeleteRecursively(workdir, subdir : Pathname.T);

PROCEDURE CompressFilesWithExtension(dir : Pathname.T; ext : TEXT);

PROCEDURE CompressFile(fn : Pathname.T);

END TechCleanup.
