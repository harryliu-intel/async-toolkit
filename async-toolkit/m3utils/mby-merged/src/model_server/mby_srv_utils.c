#include <string.h>
#include <stdlib.h>
#include "mby_model.h"
#include "mby_srv_log.h"
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
    fm_int linebytes;
    fm_int  j;
    fm_int  cnt;

    cnt = 0;
    do
    {
        linebytes = (nbytes > 16) ? 16 : nbytes;

        FM_LOG_PRINT("%02x:", cnt);

        for (j = 0 ; j < linebytes ; j++)
        {
            FM_LOG_PRINT(" %02x", buf[cnt + j]);
        }

        FM_LOG_PRINT("    ");

        for (j = 0 ; j < linebytes ; j++)
        {
            if ( (buf[cnt + j] < 0x20) || (buf[cnt + j] > 0x7e) )
            {
                FM_LOG_PRINT(".");
            }
            else
            {
                FM_LOG_PRINT("%c", buf[cnt + j]);
            }
        }

        FM_LOG_PRINT("\n");

        cnt += linebytes;
        nbytes -= linebytes;

    }
    while (nbytes > 0);

} /* end HexDump */
