#ifndef HLP_MODEL_TOOLS_H
#define HLP_MODEL_TOOLS_H

#include <fm_sdk_hlp_int.h>
#include <platforms/common/model/hlp/hlp_model_types.h>
#include <platforms/common/model/hlp/debug/hlp_model_debug.h>


/*****************************************************************************
 * Debug log
 *****************************************************************************/

/* Set to 1 to use printf() for MACsec log */
#define MSEC_PRINTF_LOG 0

#if MSEC_PRINTF_LOG

#define COLOR_NORMAL "\x1b[0m"
#define COLOR_RED    "\x1b[0;31m"
#define COLOR_GREEN  "\x1b[0;32m"

#define MSEC_LOG(...) printf(COLOR_GREEN "[MSEC] " COLOR_NORMAL __VA_ARGS__)
#define MSEC_ERR(...) printf(COLOR_GREEN "[MSEC]" COLOR_RED "[Error] " COLOR_NORMAL __VA_ARGS__)

void hexDump(fm_byte *buf, fm_int nbytes);

#else

#define MSEC_LOG(...) FM_LOG_DEBUG(FM_LOG_CAT_MODEL_MACSEC, __VA_ARGS__)
#define MSEC_ERR(...) FM_LOG_ERROR(FM_LOG_CAT_MODEL_MACSEC, __VA_ARGS__)

#define hexDump(...) do {} while (0)

#endif



/*****************************************************************************
 * MACsec frame fields and bits
 *****************************************************************************/

#define MACSEC_ETHTYPE 0x88E5

#define DA_SA_LEN 12
#define SECTAG_MAX_LEN 16
#define FCS_LEN 4
#define ICV_LEN 16

// SecTag offsets
#define ETHTYPE_OFF 0
#define TCI_OFF 2
#define SL_OFF 3
#define PN_OFF 4
#define SCI_OFF 8

#define TCI_V_BIT_OFF 7
#define TCI_ES_BIT_OFF 6
#define TCI_SC_BIT_OFF 5
#define TCI_SCB_BIT_OFF 4
#define TCI_E_BIT_OFF 3
#define TCI_C_BIT_OFF 2
#define TCI_AN_BITS_MASK 0x3

// TCI bits getters
#define TCI_NTH_BIT(secTag, n) (secTag[TCI_OFF] & (1 << n))
#define TCI_V_BIT(secTag) TCI_NTH_BIT(secTag, TCI_V_BIT_OFF)
#define TCI_ES_BIT(secTag) TCI_NTH_BIT(secTag, TCI_ES_BIT_OFF)
#define TCI_SC_BIT(secTag) TCI_NTH_BIT(secTag, TCI_SC_BIT_OFF)
#define TCI_SCB_BIT(secTag) TCI_NTH_BIT(secTag, TCI_SCB_BIT_OFF)
#define TCI_E_BIT(secTag) TCI_NTH_BIT(secTag, TCI_E_BIT_OFF)
#define TCI_C_BIT(secTag) TCI_NTH_BIT(secTag, TCI_C_BIT_OFF)
#define TCI_AN_BITS(secTag) (secTag[TCI_OFF] & TCI_AN_BITS_MASK)

// TCI bits setters (set to 1)
#define TCI_SET_NTH_BIT(secTag, n) (secTag[TCI_OFF] |= (1 << n))
#define TCI_SET_V_BIT(secTag) TCI_SET_NTH_BIT(secTag, TCI_V_BIT_OFF)
#define TCI_SET_ES_BIT(secTag) TCI_SET_NTH_BIT(secTag, TCI_ES_BIT_OFF)
#define TCI_SET_SC_BIT(secTag) TCI_SET_NTH_BIT(secTag, TCI_SC_BIT_OFF)
#define TCI_SET_SCB_BIT(secTag) TCI_SET_NTH_BIT(secTag, TCI_SCB_BIT_OFF)
#define TCI_SET_E_BIT(secTag) TCI_SET_NTH_BIT(secTag, TCI_E_BIT_OFF)
#define TCI_SET_C_BIT(secTag) TCI_SET_NTH_BIT(secTag, TCI_C_BIT_OFF)
#define TCI_SET_AN_BITS(secTag, val) (secTag[TCI_OFF] |= val & TCI_AN_BITS_MASK)

#define SECTAG_ETHTYPE(secTag) \
		((fm_uint16)secTag[ETHTYPE_OFF] << 8 | (fm_uint16)secTag[ETHTYPE_OFF+1])

// Packet number
#define SECTAG_PN(secTag) \
		((fm_uint32)secTag[PN_OFF]   << 24 | (fm_uint32)secTag[PN_OFF+1] << 16 | \
		 (fm_uint32)secTag[PN_OFF+2] << 8  | (fm_uint32)secTag[PN_OFF+3])

#define SECTAG_SET_PN(secTag, PN) \
	do { \
		secTag[PN_OFF] = (PN >> 24) & 0xFF; \
		secTag[PN_OFF+1] = (PN >> 16) & 0xFF; \
		secTag[PN_OFF+2] = (PN >> 8) & 0xFF; \
		secTag[PN_OFF+3] = PN & 0xFF; \
	} while (0);

// Secure channel identifier
#define SECTAG_SCI(secTag) \
		((fm_uint64)secTag[SCI_OFF]  << 56 | (fm_uint64)secTag[SCI_OFF+1] << 48 | \
		(fm_uint64)secTag[SCI_OFF+2] << 40 | (fm_uint64)secTag[SCI_OFF+3] << 32 | \
		(fm_uint64)secTag[SCI_OFF+4] << 24 | (fm_uint64)secTag[SCI_OFF+5] << 16 | \
		(fm_uint64)secTag[SCI_OFF+6] <<  8 | (fm_uint64)secTag[SCI_OFF+7])

#define SECTAG_SET_SCI(secTag, SCI) \
	do { \
		secTag[SCI_OFF] = (SCI >> 56) & 0xFF; \
		secTag[SCI_OFF+1] = (SCI >> 48) & 0xFF; \
		secTag[SCI_OFF+2] = (SCI >> 40) & 0xFF; \
		secTag[SCI_OFF+3] = (SCI >> 32) & 0xFF; \
		secTag[SCI_OFF+4] = (SCI >> 24) & 0xFF; \
		secTag[SCI_OFF+5] = (SCI >> 16) & 0xFF; \
		secTag[SCI_OFF+6] = (SCI >> 8) & 0xFF; \
		secTag[SCI_OFF+7] = SCI & 0xFF; \
	} while (0);

// Values of VALIDATEFRAMES:  0x3-N/A, 0x2-Check, 0x1-Strict, 0x0-Disabled
#define VALIDATE_DISABLED 0x0
#define VALIDATE_STRICT 0x1
#define VALIDATE_CHECK 0x2

// Ciphers supported: GCM-AES-128, GCM-AES-256
#define MAX_KEY_LEN (256/8)
#define IV_LEN 12

// Relay tagging
#define MAX_RLYTAG_LEN 8

#define RLY_MODE_NO 0
#define RLY_MODE_SINGLE 1
#define RLY_MODE_DUAL 2
#define RLY_MODE_REPLACE 3
#define RLY_MODE_GENERIC 4


/*****************************************************************************
 * Register access
 *****************************************************************************/

typedef struct {
	fm_byte blockId;
	fm_byte lane;
	fm_byte SC;
	fm_byte AN;
	fm_bool enableRx;
	fm_bool inUse; // Read only
	fm_uint64 lowestPN;
	fm_uint64 nextPN;
	fm_byte key[MAX_KEY_LEN];
	fm_uint32 keyLen; // #bits
} hlp_msec_rx_sa;

void dumpMacsecRxSA(hlp_msec_rx_sa *SA);

void readMacsecRxSA(hlp_model *model, fm_byte blockId, fm_byte lane,
		fm_byte SC, fm_byte AN, hlp_msec_rx_sa *SA);

void writeMacsecRxSA(hlp_model *model, hlp_msec_rx_sa *SA);

typedef struct {
	fm_byte blockId;
	fm_byte lane;
	fm_byte SC;
	fm_byte AN;
	fm_bool enableTx;
	fm_bool inUse; // Read only
	fm_uint64 nextPN;
	fm_bool confid;
	fm_byte key[MAX_KEY_LEN];
	fm_uint32 keyLen; // #bits
} hlp_msec_tx_sa;

void dumpMacsecTxSA(hlp_msec_tx_sa *SA);

void readMacsecTxSA(hlp_model *model, fm_byte blockId, fm_byte lane,
		fm_byte SC, fm_byte AN, hlp_msec_tx_sa *SA);

void writeMacsecTxSA(hlp_model *model, hlp_msec_tx_sa *SA);

/*****************************************************************************
 * Counter utils
 *****************************************************************************/

// Increment a generic counter
#define INCR_CNTR(model, addr, increment, saturate)                            \
    IncrementCounter((model),                                                  \
                     FM_MODEL_GET_REG_PTR((model), (addr)),                    \
                     (increment),                                              \
                     (saturate));

// Increment a counter associated to a particular Secure Channel (Rx/Tx)
#define INCR_SC_CNTR(model, SC, blockId, lane, txrx, counter, val) \
	INCR_## txrx ##_SC_CNTR(model, SC, blockId, lane, counter, val)

// Increment a counter associated to a particular Rx Secure Channel
#define INCR_RX_SC_CNTR(model, SC, blockId, lane, counter, val) \
	INCR_CNTR(model, SC == 0 ? \
              HLP_MSEC_RX_LANE_SC0_STAT_## counter (blockId, lane, 0) : \
              HLP_MSEC_RX_LANE_SC1_STAT_## counter (blockId, lane, 0), val, FALSE);

// Increment a counter associated to a particular Tx Secure Channel
#define INCR_TX_SC_CNTR(model, SC, blockId, lane, counter, val) \
	INCR_CNTR(model, HLP_MSEC_TX_LANE_SC0_STAT_## counter (blockId, lane, 0), val, FALSE);

// Increment a counter associated to a particular Secure Association (Rx/Tx)
#define INCR_SA_CNTR(model, SC, AN, blockId, lane, txrx, counter, val) \
	INCR_## txrx ##_SA_CNTR(model, SC, AN, blockId, lane, counter, val)

#define INCR_RX_SA_CNTR(model, SC, AN, blockId, lane, counter, val) \
	switch (AN) { \
		case 0: \
				INCR_CNTR(model, SC == 0 ? \
						HLP_MSEC_RX_LANE_SC0_SA0_STAT_ ## counter (blockId, lane, 0) : \
						HLP_MSEC_RX_LANE_SC1_SA0_STAT_ ## counter (blockId, lane, 0), \
						val, FALSE); \
		break; \
		case 1: \
				INCR_CNTR(model, SC == 0 ? \
						HLP_MSEC_RX_LANE_SC0_SA1_STAT_ ## counter (blockId, lane, 0) : \
						HLP_MSEC_RX_LANE_SC1_SA1_STAT_ ## counter (blockId, lane, 0), \
						val, FALSE); \
		break; \
		case 2: \
				INCR_CNTR(model, SC == 0 ? \
						HLP_MSEC_RX_LANE_SC0_SA2_STAT_ ## counter (blockId, lane, 0) : \
						HLP_MSEC_RX_LANE_SC1_SA2_STAT_ ## counter (blockId, lane, 0), \
						val, FALSE); \
		break; \
		case 3: \
				INCR_CNTR(model, SC == 0 ? \
						HLP_MSEC_RX_LANE_SC0_SA3_STAT_ ## counter (blockId, lane, 0) : \
						HLP_MSEC_RX_LANE_SC1_SA3_STAT_ ## counter (blockId, lane, 0), \
						val, FALSE); \
		break; \
	}

#define INCR_TX_SA_CNTR(model, SC, AN, blockId, lane, counter, val) \
	switch (AN) { \
		case 0: \
				INCR_CNTR(model, \
						HLP_MSEC_TX_LANE_SC0_SA0_STAT_ ## counter (blockId, lane, 0), \
						val, FALSE); \
		break; \
		case 1: \
				INCR_CNTR(model, \
						HLP_MSEC_TX_LANE_SC0_SA1_STAT_ ## counter (blockId, lane, 0), \
						val, FALSE); \
		break; \
		case 2: \
				INCR_CNTR(model, \
						HLP_MSEC_TX_LANE_SC0_SA2_STAT_ ## counter (blockId, lane, 0), \
						val, FALSE); \
		break; \
		case 3: \
				INCR_CNTR(model, \
						HLP_MSEC_TX_LANE_SC0_SA3_STAT_ ## counter (blockId, lane, 0), \
						val, FALSE); \
		break; \
	}

void IncrementCounter(hlp_model *model, fm_uint32 *addr,
                      fm_uint32  increment, fm_bool saturate);
#endif // HLP_MODEL_TOOLS_H
