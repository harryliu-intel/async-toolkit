#include <string.h>
#include <stdlib.h>
#include "mby_model.h"
#include "mby_srv_errno.h"

fm_status ParseUint64(char *string, fm_uint64 *result)
{
    char *endptr = NULL;

    if (string == NULL)
    {
        *result = 0;
        return -1;
    }

    *result = 0;

    if (string[1] == 'x')
    {
        *result = strtoull(string + 2, &endptr, 16);
    }
    else
    {
        *result = strtoull(string, &endptr, 10);
    }

    return (string == endptr) ? FM_ERR_INVALID_ARGUMENT : FM_OK;

}

fm_status ParseInt(char *string, fm_int *result)
{
    char *endptr = NULL;

    if (string == NULL)
    {
        *result = 0;
        return -1;
    }

    *result = 0;

    if (string[1] == 'x')
    {
        *result = strtoul(string + 2, &endptr, 16);
    }
    else
    {
        *result = strtoul(string, &endptr, 10);
    }

    return (string == endptr) ? FM_ERR_INVALID_ARGUMENT : FM_OK;

}
void HexDump(fm_byte *buf, fm_int nbytes)
{


} /* end HexDump */
