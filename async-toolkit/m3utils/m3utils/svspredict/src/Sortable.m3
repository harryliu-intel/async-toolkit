MODULE Sortable;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Compare(a, b : T) : CompRes = BEGIN RETURN a.compare(b) END Compare;

BEGIN END Sortable.
