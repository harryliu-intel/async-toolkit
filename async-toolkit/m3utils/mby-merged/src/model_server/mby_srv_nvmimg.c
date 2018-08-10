
static int processChunk(unsigned int modNum, unsigned int chnkNum,
                        uint16_t *chunk, unsigned int chunkLen) {
    unsigned int type;
    unsigned int addr;
    unsigned int size;
    unsigned int cnt;
    unsigned int n = 0;
    uint64_t val;
    int numEntries = 0;

    while (n < chunkLen) {
        type = chunk[n] & 0xF;
        switch (type) {
        case 5:
            size = 6;
            break;
        default:
            printf("MOD[%d] CHNK[%d]: Invalid type %d at chunk word %d\n",
                   modNum, chnkNum, type, n);
            return -1;
        }
        addr = chunk[n + 1];
        addr <<= 12;
        addr |= (chunk[n] >> 4);

        val = 0;
        for (cnt = 3; cnt < 4; cnt--) {
            val <<= 16;
            val |= chunk[n + cnt + 2];
        }
        numEntries++;
        if (outFp)
        {
                fprintf(outFp, "0x%07x 0x%lx\n", addr, val);
        }
        WriteCSR64(0, addr, val);
        n += size;
    }
    return numEntries;
}

static int processModule(unsigned int modNum, uint16_t *module,
                         unsigned int len)
{
    unsigned int n = 0;
    uint16_t cl;
    int cnt = 0;
    int num;
    int numEntries = 0;

    while (n < len) {
        cl = module[n];
        if (debug >= 3)
            printf("MOD[%d] CHNK[%d] LEN: %d\n", modNum, cnt, cl);
        if (cl > 4096) {
            printf("MOD[%d]: Invalid chunk len %d(0x%x) at byte offset %d(0x%x)\n",
                   modNum, cl, cl, n * 2, n * 2);
            return -1;
        }
        if (cl == 0) {
            printf("MOD[%d]: Invalid chunk len %d\n", cnt, cl);
            return -1;
        }
        num = processChunk(modNum, cnt, module + n + 1, cl);
        if (num <= 0)
            return -1;
        numEntries += num;

        cnt++;
        n += (cl + 1);
    }
    return numEntries;
}

static int loadImg(char *filename, fm_uint32 *alBitmask)
{
    unsigned int addr;
    time_t start;
    time_t end;
    uint64_t val;
	FILE *fp;
	unsigned int len;
	int err;
    unsigned int cnt = 0;
    unsigned int num, numEntries = 0;
    uint32_t header[NVM_HDR_SIZE/4];
    uint32_t offset, modLen;
    uint16_t module[NVM_MOD_SIZE];

	if (!filename)
		return -1;

    if (strlen(filename) < 1) return 0;

    start = time(NULL);
	printf("Loading file: %s\n", filename);
	fp = fopen(filename, "r");

	if (!fp) {
		printf("Unable to open '%s' - '%d'\n", filename, errno);
		return -1;
	}

	if ((len = fread(header, NVM_HDR_SIZE, 1, fp)) != NVM_HDR_SIZE) {

        /* read the default alBitmask from Header PFA if not overwriten*/
        for (cnt = 0; cnt < 8; cnt++) {
            if (alBitmask[cnt] == UNDEF_VAL)
                alBitmask[cnt] = header[0x820/4 + cnt];
        }

        if (debug >= 2) {
            printf("IMG_ID : %08x\n", header[0]);
            printf("IMG_VER: %04x\n", header[1] & 0xFFFF);
            printf("REG_VER: %04x\n", header[1] >> 16);
            printf("CHKSM  : %04x\n", header[2]);
            printf("IMG_LEN: %d\n", header[3]);
            printf("MAX_LEN: %d\n", header[4]);
            printf("\n");
            printf("AL_BITMASK:\n");
            for (cnt = 0; cnt < 8; cnt++) {
                printf(" %08x", alBitmask[cnt]);
            }
            printf("\n");

        }
        if (outFp)
        {
            fprintf(outFp, "# Generated from %s: version %d checksum %04X\n",
                    filename, header[1] & 0xFFFF, header[2]);
        }
        if (header[0] != 0x4563AABB) {
            /* If text, load text image */
            if (isprint(header[0] & 0xFF)) {
                fclose(fp);
                printf("Loading text file: %s\n", filename);
                return loadTextCfg(filename);
            }
            printf("Expect header of 0x4563AABB but got 0x%08x\n", header[0]);
            return -1;
        }
        printf("Signed binary image version %02x.%02x with checksum %04X\n",
                        (header[1] >> 8) & 0xFF, header[1] & 0xFF, header[2]);
        for (cnt = 0; cnt < 256; cnt++) {
            offset = header[0x20/4 + cnt];
            if (debug >= 3)
                printf("AL_MODULE_BASE[%d]: %08x\n", cnt, offset);
            if (offset < 0xFFFFFF) {
                if (!(alBitmask[cnt/32] & (1ULL << (cnt%32)))) {
                    if (debug >= 2)
                        printf("Skipping module %d due to bit mask disabled\n",
                               cnt);
                    continue;
                }
                if (outFp)
                {
                        fprintf(outFp, "# PFA #%d\n", cnt);
                }
                if (fseek(fp, offset, SEEK_SET) != 0) {
                    printf("Unable to find module %d at offset %u\n",
                           cnt, offset);
                    return -1;
                }
                if ((len = fread(&modLen, 4, 1, fp)) != 1) {
                    printf("Unable to read module length. Got %d\n", len);
                    return -1;
                }
                if (debug >= 3)
                    printf("MODULE[%d] LEN:: %d\n", cnt, modLen);
                if (modLen > NVM_MOD_SIZE) {
                    printf("Invalid module length %d greater than module size %d\n",
                           modLen, NVM_MOD_SIZE);
                }
                if (modLen >
                    (header[0x20/4 + cnt + 1] - header[0x20/4 + cnt])) {
                    printf("Invalid module length %d greater then next module offset %d\n",
                           modLen,
                           header[0x20/4 + cnt + 1] - header[0x20/4 + cnt]);
                }
                if ((len = fread(module, 2, modLen, fp)) != modLen) {
                    printf("Unable to read module of size %d . Got %d\n",
                           modLen, len);
                    return -1;
                }
                if (debug >= 3) {
                    printf("MODULE[%d]:\n", cnt);
                    HexDump((unsigned char*)module, modLen * 2);
                }
                num = processModule(cnt, module, modLen);
                if (num <= 0) {
                    printf("Unable to process module %d\n", cnt);
                    return -1;
                }
                numEntries += num;
            }
        }
        /* Calculate FWD_PORT_CFG_1(cpkPort) */
        addr = HLP_FWD_PORT_CFG_1(20,0);
        val = 0x3FFFFFF;
        if (outFp)
        {
                fprintf(outFp, "0x%07x 0x%lx\n", addr, val);
        }
        WriteCSR64(0, addr, val);

	} else {
        printf("Failed to read image header. Got %d bytes\n", len);
        return -1;
    }

	fclose(fp);

    end = time(NULL);
    printf("%s: %d entries loaded in %lld seconds\n", filename,
           numEntries, (long long)(end - start));

	return 0;
}

