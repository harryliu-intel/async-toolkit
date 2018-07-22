INTERFACE MbyDoRealignKeys;
FROM MbyParserToMapperMeta IMPORT PaKeys;
FROM MbyParserTypes IMPORT PaKey;
FROM MbyMapperSizes IMPORT NRealignKeys;

TYPE RaKeys = ARRAY [0..NRealignKeys-1] OF PaKey;

PROCEDURE  RealignKeys(READONLY isIpV4, isIpV6    : ARRAY [0..1] OF BOOLEAN;
                       READONLY pk (*paKeys*)     : PaKeys;
                       VAR rk  (*realignedKeys*)  : RaKeys;
                       VAR ihlOk                  : BOOLEAN;
                       VAR ihlFits                : BOOLEAN);

END MbyDoRealignKeys.
