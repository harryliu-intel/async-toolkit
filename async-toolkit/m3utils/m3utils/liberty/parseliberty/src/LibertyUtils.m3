MODULE LibertyUtils;
IMPORT ParseParams;
IMPORT Stdio;

PROCEDURE DoParseParams() : ParseParams.T =
  BEGIN
    RETURN NEW(ParseParams.T).init(Stdio.stderr)
  END DoParseParams;

BEGIN END LibertyUtils.
