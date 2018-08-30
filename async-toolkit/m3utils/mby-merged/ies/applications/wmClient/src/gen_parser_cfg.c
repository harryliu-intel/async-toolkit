#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <errno.h>

#define HIG	1

int debug = 0;

int ParseUint(char *string, unsigned int *result)
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

    return (string == endptr) ? -1 : 0;

}

static char *api_reformat(char *name)
{
    int len;
    int i, n;
    int idx = 0;

    len = strlen(name);
    n = 0;
    for (i = 0; i < len;)
    {
        if (name[i] == '[')
	{
		name[n] = '(';
		i++;
		idx = 1;
	}
	else if (name[i] == ']' && name[i+1] == '[')
	{
		name[n] = ',';
		i += 2;
		idx = 1;
	}
	else if (name[i] == ']')
	{
		name[n] = ',';
		i++;
		idx = 1;
	}
	else
	{
	    name[n] = name[i];
	    i++;
	}
	n++;
    }
    if (!idx)
    {
	    /* Maybe overflow name string */
	    name[n] = '(';
	    n++;
    }
    name[n] = '\0';
    return name;
}

static void print_usage(char *cmd)
{
	printf("Usage: %s <filename> [options]\n", cmd);
	printf("    -h                       - This help.\n");
	printf("    -g                       - Output format for HIG.\n");
	printf("    -v                       - Verbose output.\n");

	exit(0);
}

int main(int argc, char *argv[])
{
	int verbose = 0;
	char *filename = argv[1];
	int i;
	char str_err_buf[32];
	size_t line_size = 0;
	char *line = NULL;
	int line_num = 0;
	FILE *fp;
	int len;
	int err;
	char *p, *r, *d;
	int format = 0;


	for (i = 2; i < argc; i++) {
		if (!strcmp(argv[i], "-v") && (i < argc) ) {
			verbose = 1;
		} else if (!strcmp(argv[i], "-g") && (i < argc) ) {
			format = HIG;
		} else {
			print_usage(argv[0]);
			exit(0);
		}
	}

	if (!filename) {
		print_usage(argv[0]);
		return -1;
	}

	if (format == HIG) {
		printf("#Extracting from file: %s\n", filename);
	} else {
		printf("\t/* Extracting from file: %s */\n", filename);
	}
	fp = fopen(filename, "r");

	if (!fp) {
		printf("Unable to open '%s' - '%d'\n", filename, errno);
		return -1;
	}

	while ((len = getline(&line, &line_size, fp)) != -1) {
		line_num++;
		if (len <= 2)
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

		if (verbose)
			printf("line#%d [%s].\n", line_num, line);
		if ((p = strstr(line, "data=")) || (p = strstr(line, "BSM_SCRATCH"))) {
			if (p[0] == 'd') {
				d = p + 5;
				r = strstr(d, "PARSER");
				if (!r) {
					r = strstr(d, "BSM_SCRATCH");
				}
				p = strstr(p, ";");
				if (!p) {
					p = strstr(d, " ");
				}
			} else {
				r = p;
				d = strstr(p, "0x");
				p = strstr(p, ":");
			}
			if (!p || !r) {
				printf("Failed to find delimiter\n");
				printf("d=[%s]\n", d);
				printf("r=[%s]\n", r);
				return -1;
			}
			*p = '\0';
			if (format == HIG) {
				printf("%s %s\n", r, d);
			} else {
				printf("\twrite64(HLP_%s0), %s);\n",
				       api_reformat(r), d);
			}
		} else if ((p = strstr(line, "STAGE"))) {
			if (format == HIG) {
				printf("# %s\n", p);
			} else {
				printf("\t/* %s */\n", p);
			}
		}
	}

	if (line)
		free(line);

	fclose(fp);

	exit(0);
}
