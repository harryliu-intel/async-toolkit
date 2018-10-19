

int loadTextCfg(char *filename)
{
	size_t line_size = 0;
	char *line = NULL;
    unsigned int addr;
    time_t start;
    time_t end;
    uint64_t val;
	FILE *fp;
	int len;
	int err;
    int cnt = 0;
    int lineNum = 0;

	if (!filename)
		return -1;

    if (strlen(filename) < 1) return 0;

    start = time(NULL);
	fp = fopen(filename, "r");

	if (!fp) {
		printf("Unable to open '%s' - '%d'\n", filename, errno);
		return -1;
	}
    printf("Loading text file: %s\n", filename);
	while ((len = getline(&line, &line_size, fp)) != -1) {
        lineNum++;
		if (len <= 2 || line[0] == '#')
			continue;

		/* Remove CR */
		if ((line[len - 1] == '\n') || (line[len - 1] == '\r')) {
			len--;
			line[len] = '\0';
		}
		if ((line[len - 1] == '\n') || (line[len - 1] == '\r')) {
			len--;
			line[len] = '\0';
		}

        if (line[0] == '#')
            continue;

        if (2 != sscanf(line, "0x%x 0x%lx\n", &addr, &val)) {
            printf("ERROR: Fail to parse line#%d: %s\n", lineNum, line);
            continue;
        }

        if (debug >= 3) printf("Addr %x %lx\n", addr, val);
        cnt++;

        if (WriteCSR64(0, addr, val))
        {
            printf("ERROR: Fail to write 0x%lx to 0x%x\n", val, addr);
        }

	}

	if (line)
		free(line);

	fclose(fp);

    end = time(NULL);
    printf("%s: %d entries loaded in %lld seconds\n", filename,
           cnt, (long long)(end - start));

	return 0;
}

int loadParserCfg(char *filename)
{
    fm_status       status = FM_OK;
    fm_uint32       addr;
    fm_uint32       value;

    printf("Setting parser registers default values\n");
    for (addr = HLP_PARSER_BASE; addr < HLP_PARSER_BASE + HLP_PARSER_SIZE; addr += 4)
    {
        value = hlpModelGetRegisterDefault(addr);

        status = WriteCSR(0, addr, value);
        if (status == FM_ERR_NOT_FOUND)
        {
            /* Ignore out-of-bounds errors. */
            status = FM_OK;
        }
        if (status)
            return status;

    }

    status = loadTextCfg(filename);
    return status;
}
