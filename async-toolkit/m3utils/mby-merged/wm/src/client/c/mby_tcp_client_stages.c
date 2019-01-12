#include "mby_tcp_client_stages.h"

#include "mby_tcp_client_library.h"

/* Execute the Parser stage function
 *
 * Invokes the Parser stage in the server that is currently running.
 * The register space is passed from the model server.  Use wm_reg_write to
 * set the register values before calling this function.
 *
 * @param[in]   in pointer to Parser input structure
 * @param[out]  out pointer to Parser output structure
 *
 * @retval      WM_OK if successful
 */

SIMPLE_STAGE_IMPL(Parser)

SIMPLE_STAGE_IMPL(Mapper)

SIMPLE_STAGE_IMPL(Classifier)

int
wm_Modifier(Modifier_in_t   const * const in,
            Modifier_out_t        * const out,
            varchar_t       const * const rx_data,
            varchar_t             * const tx_data)
{
    return wm_do_stage("Modifier",
                       in, sizeof(*in),
                       rx_data,
                       out, sizeof(*out),
                       tx_data);
}
