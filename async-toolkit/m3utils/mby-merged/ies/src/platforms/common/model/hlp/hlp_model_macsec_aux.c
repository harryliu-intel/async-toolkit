#include <platforms/common/model/hlp/hlp_model_macsec_aux.h>

static void reverse(fm_byte *key, fm_uint32 len) {
	fm_byte *begin, *end;

	if (key == NULL || len < 2)
		return;

	begin = key;
	end = key + len - 1;
	while (begin < end) {
		fm_byte tmp;
		tmp = *begin;
		*begin = *end;
		*end = tmp;
		++begin;
		--end;
	}
}

void dumpMacsecRxSA(hlp_msec_rx_sa *SA) {

    MSEC_LOG("Rx SA Dump (blockId=%d - lane=%d - SC=%d - AN=%d): enableRx=%d - inUse=%d - lowestPn=%llu - nextPn=%llu\n",
			SA->blockId, SA->lane, SA->SC, SA->AN, SA->enableRx, SA->inUse,
			SA->lowestPN, SA->nextPN);
	// TODO maybe dump the key as well
}

void readMacsecRxSA(hlp_model *model, fm_byte blockId, fm_byte lane,
              fm_byte SC, fm_byte AN, hlp_msec_rx_sa *SA) {

    if (blockId > 7 || lane > 1 || SC > 1 || AN > 3) {
		MSEC_LOG("Error: read Rx SA - Invalid blockId=%d || lane=%d || SC=%d || AN=%d\n", blockId, lane, SC, AN);
		goto ABORT;
    }

	SA->blockId = blockId;
	SA->lane = lane;
	SA->SC = SC;
	SA->AN = AN;

    switch(SC) {
#define CASE(sc, sa) case sa: \
		SA->enableRx = FM_ARRAY_GET_BIT( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_EN_RCV(blockId, lane, 0)), \
				HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_EN_RCV, ENABLERECEIVE); \
		SA->inUse = SA->enableRx; \
		FM_ARRAY_SET_BIT( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_INUSE(blockId, lane, 0)), \
				HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_INUSE, INUSE, SA->inUse); \
		SA->lowestPN = FM_ARRAY_GET_FIELD64( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_MIN_PN(blockId, lane, 0)), \
				HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_MIN_PN, LOWESTPN); \
		SA->nextPN = FM_ARRAY_GET_FIELD64( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_NEXT_PN(blockId, lane, 0)), \
				HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_NEXT_PN, NEXTPN); \
		FM_MEMCPY_S(SA->key, MAX_KEY_LEN, \
				FM_MODEL_GET_REG_PTR(model, HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_KEY(blockId, lane, 0)), \
				MAX_KEY_LEN); \
		SA->keyLen = 128 + 128 * FM_ARRAY_GET_BIT( \
				FM_MODEL_GET_REG_PTR(model, HLP_MSEC_GLOBAL_KEY_LEN(SA->blockId, 0)), \
				HLP_MSEC_GLOBAL_KEY_LEN, KEYLENGTH);\
		reverse(SA->key, SA->keyLen / 8); \
		return;

        case 0:
        switch (AN) {
            CASE(0, 0);
            CASE(0, 1);
            CASE(0, 2);
            CASE(0, 3);
        }
        break;
        case 1:
        switch (AN) {
            CASE(1, 0);
            CASE(1, 1);
            CASE(1, 2);
            CASE(1, 3);
        }
        break;
    }
#undef CASE

ABORT:
	MSEC_LOG("Something went terribly wrong in function %s, WM will shutdown\n", __func__);
	exit(1);
}

void writeMacsecRxSA(hlp_model *model, hlp_msec_rx_sa *SA) {

    if (SA->blockId > 7 || SA->lane > 1 || SA->SC > 1 || SA->AN > 3) {
		MSEC_LOG("Error: write Rx SA - Invalid blockId=%d || lane=%d || SC=%d || AN=%d\n",
				SA->blockId, SA->lane, SA->SC, SA->AN);
		goto ABORT;
    }

    switch(SA->SC) {
#define CASE(sc, sa) case sa: \
		FM_ARRAY_SET_BIT( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_EN_RCV(SA->blockId, SA->lane, 0)), \
				HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_EN_RCV, ENABLERECEIVE, SA->enableRx); \
		SA->inUse = SA->enableRx; \
		FM_ARRAY_SET_BIT( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_INUSE(SA->blockId, SA->lane, 0)), \
				HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_INUSE, INUSE, SA->inUse); \
		FM_ARRAY_SET_FIELD64( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_MIN_PN(SA->blockId, SA->lane, 0)), \
				HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_MIN_PN, LOWESTPN, SA->lowestPN); \
		FM_ARRAY_SET_FIELD64( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_NEXT_PN(SA->blockId, SA->lane, 0)), \
				HLP_MSEC_RX_LANE_SC ## sc ## _SA ## sa ##_NEXT_PN, NEXTPN, SA->nextPN); \
		return;

        case 0:
        switch (SA->AN) {
            CASE(0, 0);
            CASE(0, 1);
            CASE(0, 2);
            CASE(0, 3);
        }
        break;
        case 1:
        switch (SA->AN) {
            CASE(1, 0);
            CASE(1, 1);
            CASE(1, 2);
            CASE(1, 3);
        }
        break;
    }
#undef CASE

ABORT:
	MSEC_LOG("Something went terribly wrong in function %s, WM will shutdown\n", __func__);
	exit(1);
}


void dumpMacsecTxSA(hlp_msec_tx_sa *SA) {

    MSEC_LOG("Tx SA Dump (blockId=%d - lane=%d - SC=%d - AN=%d): enableTx=%d - inUse=%d - confid=%d - nextPn=%llu\n",
			SA->blockId, SA->lane, SA->SC, SA->AN, SA->enableTx, SA->inUse,
			SA->confid, SA->nextPN);

}

void readMacsecTxSA(hlp_model *model, fm_byte blockId, fm_byte lane,
              fm_byte SC, fm_byte AN, hlp_msec_tx_sa *SA) {

    if (blockId > 7 || lane > 1 || SC != 0 || AN > 3) {
		MSEC_LOG("Error: read Tx SA - Invalid blockId=%d || lane=%d || SC=%d || AN=%d\n", blockId, lane, SC, AN);
		goto ABORT;
    }

	SA->blockId = blockId;
	SA->lane = lane;
	SA->SC = SC;
	SA->AN = AN;

	switch (AN) {
#define CASE(sc, sa) case sa: \
		SA->enableTx = FM_ARRAY_GET_BIT( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_EN_TRN(blockId, lane, 0)), \
				HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_EN_TRN, SA_EN_TRN); \
		SA->inUse = SA->enableTx; \
		FM_ARRAY_SET_BIT( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_INUSE(blockId, lane, 0)), \
				HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_INUSE, SA_INUSE, SA->inUse); \
		SA->confid = FM_ARRAY_GET_BIT( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_CONFID(blockId, lane, 0)), \
				HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_CONFID, SA_CONFID); \
		SA->nextPN = FM_ARRAY_GET_FIELD64( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_NEXT_PN(blockId, lane, 0)), \
				HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_NEXT_PN, NEXT_PN); \
		FM_MEMCPY_S(SA->key, MAX_KEY_LEN, \
				FM_MODEL_GET_REG_PTR(model, HLP_MSEC_TX_LANE_SC0_SA ## sa ##_KEY(blockId, lane, 0)), \
				MAX_KEY_LEN); \
		SA->keyLen = 128 + 128 * FM_ARRAY_GET_BIT( \
				FM_MODEL_GET_REG_PTR(model, HLP_MSEC_GLOBAL_KEY_LEN(SA->blockId, 0)), \
				HLP_MSEC_GLOBAL_KEY_LEN, KEYLENGTH);\
		reverse(SA->key, SA->keyLen / 8);\
		return;

		CASE(0, 0);
		CASE(0, 1);
		CASE(0, 2);
		CASE(0, 3);
	}
#undef CASE

ABORT:
	MSEC_LOG("Something went terribly wrong in function %s, WM will shutdown\n", __func__);
	exit(1);
}

void writeMacsecTxSA(hlp_model *model, hlp_msec_tx_sa *SA) {

    if (SA->blockId > 7 || SA->lane > 1 || SA->SC != 0 || SA->AN > 3) {
		MSEC_LOG("Error: write Tx SA - Invalid blockId=%d || lane=%d || SC=%d || AN=%d\n",
				SA->blockId, SA->lane, SA->SC, SA->AN);
		goto ABORT;
    }

	switch (SA->AN) {
#define CASE(sc, sa) case sa: \
		FM_ARRAY_SET_BIT( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_EN_TRN(SA->blockId, SA->lane, 0)), \
				HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_EN_TRN, SA_EN_TRN, SA->inUse); \
		SA->inUse = SA->enableTx; \
		FM_ARRAY_SET_BIT( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_INUSE(SA->blockId, SA->lane, 0)), \
				HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_INUSE, SA_INUSE, SA->inUse); \
		FM_ARRAY_SET_BIT( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_CONFID(SA->blockId, SA->lane, 0)), \
				HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_CONFID, SA_CONFID, SA->confid); \
		FM_ARRAY_SET_FIELD64( \
				FM_MODEL_GET_REG_PTR(model, \
					HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_NEXT_PN(SA->blockId, SA->lane, 0)), \
				HLP_MSEC_TX_LANE_SC ## sc ## _SA ## sa ##_NEXT_PN, NEXT_PN, SA->nextPN); \
		return;

		CASE(0, 0);
		CASE(0, 1);
		CASE(0, 2);
		CASE(0, 3);
	}
#undef CASE

ABORT:
	MSEC_LOG("Something went terribly wrong in function %s, WM will shutdown\n", __func__);
	exit(1);
}

void IncrementCounter(hlp_model *model, fm_uint32 *addr,
                             fm_uint32  increment, fm_bool    saturate)
{
    fm_uint32 delta;

    FM_NOT_USED(model);

    delta = 0xFFFFFFFF - *addr;
    if (saturate) {
        *addr += MIN(increment, delta);
    }
    else {
        if (increment > delta) {
            /* Ensure that 0xFFFFFFFF + 1 wraps around to zero. */
            *addr = increment - (1 + delta);
        }
        else {
            *addr += increment;
        }
    }

}   /* end IncrementCounter */


#if MSEC_PRINTF_LOG
void hexDump(fm_byte *buf, fm_int nbytes) {
    fm_int  j;
    fm_int  cnt;

    cnt = 0;
    do {
		fm_int linebytes;
        linebytes = (nbytes > 16) ? 16 : nbytes;

        printf("%02x:", cnt);

        for (j = 0 ; j < linebytes ; j++)
            printf(" %02x", buf[cnt + j]);

        printf("    ");

        for (j = 0 ; j < linebytes ; j++) {
            if ( (buf[cnt + j] < 0x20) || (buf[cnt + j] > 0x7e) )
                printf(".");
            else
                printf("%c", buf[cnt + j]);
        }

        printf("\n");
        cnt += linebytes;
        nbytes -= linebytes;
    }
    while (nbytes > 0);
} /* end HexDump */
#endif
