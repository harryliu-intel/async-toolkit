

#define INTERRUPT_READ_DELAY    1

fm_status SendInterruptEvent(fm_socket *socket, fm_int64 intr);

void handleIntr(fm_int socketNum, fm_socket **  sockets)
{
    fm_uint64 val64;
    fm_int i;

    ReadCSR64(0, HLP_GLOBAL_INTERRUPT(0), &val64);

    if ((val64 & GLOBAL_INTR_MASK) &&
        !FM_GET_BIT64(val64, HLP_GLOBAL_INTERRUPT, COMPLETION_PENDING) &&
        !FM_GET_BIT64(val64, HLP_GLOBAL_INTERRUPT, INTERRUPT_PENDING))
    {
        printf("Interrupt 0x%llx is pending\n", val64);
        for ( i = 1 ; i < socketNum ; i++ )
        {
            SendInterruptEvent(sockets[i], val64);
        }
        /* There is no response back from Admin Interrupt Queue,
         * hence clear COMPLENTION_PENDING bit. */
        FM_SET_BIT64(val64, HLP_GLOBAL_INTERRUPT, COMPLETION_PENDING, 0);
        FM_SET_BIT64(val64, HLP_GLOBAL_INTERRUPT, INTERRUPT_PENDING, 1);
        WriteCSRForce64(0, HLP_GLOBAL_INTERRUPT(0), val64);
    }
}



/*****************************************************************************/
/** SendInterruptEvent
 * \ingroup intModel
 *
 * \desc            Send IOSF interrupt event message.
 *
 * \param[in]       socket points to the socket where the message is originated.
 *
 * \param[in]       intr is the global interrupt value.
 *
 * \return          FM_OK if successful.
 *
 *****************************************************************************/
fm_status SendInterruptEvent(fm_socket *socket, fm_int64 intr)
{
    fm_modelMessage  emsg;
    fm_uint          msgLength;


    FM_CLEAR(emsg);

    emsg.type = htons(FM_MODEL_MSG_IOSF);
    msgLength = 20 + FM_MODEL_MSG_HEADER_SIZE;
    emsg.msgLength = htonl(msgLength);

    emsg.data[DEST_OFF] = strap_interrupt_destid;
    emsg.data[SOURCE_OFF] = strap_iosfsb_endpt_id;
    emsg.data[OPCODE_OFF] = IOSF_REG_WR_PRIV_CTRL;

    /* EH=1, AL=1, bar=0, tag=0 */
    emsg.data[3] = (0x3 << 6);
    *(fm_uint16*)(emsg.data + SAI_OFF) = strap_endpt_sai;
    /* fbe=0xf, sbe=0xf */
    emsg.data[8] = 0xFF;
    *(fm_uint64*)&emsg.data[12] = intr;

    if (debug >=3)
    {
        FM_LOG_PRINT("INTR: 0x%llx\n", intr);
        HexDump((fm_byte*)&emsg, msgLength);
    }

    SendMessage(socket, &emsg, msgLength);

    FM_LOG_EXIT(FM_LOG_CAT_PLATFORM, FM_OK);
}
