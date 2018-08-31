addr2macro
==========

The addr2macro tool allows translation of _decimal_ addresses into macro calls.
The tool expects the database as a file; then, addresses are read (in decimal) from the standard input
and the results are printed on the standard output. These can be redirected (for example, to FIFOs);
run the program without arguments for more information.

## Creating the database
The generate_reg_db.py script takes the register header file from stdin and writes the database to
stdout. The utility recognizes following line formats:
#define HLP_MSEC_BASE                                           (0x0400000)
#define HLP_MSEC_SCRATCH(index, word)                           ((0x0040000) * ((index) - 0) + ((word)*4)+ (0x0000000) + (HLP_MSEC_BASE))
At this moment, the only legal subtraction is subtracting zero.

## Database format
The database is a CSV file: each line corresponds to a single register.
It looks as follows:
register_name,base_address[,dimension_name,dimension_stride]

The last section is repeated for each macro parameter. All numbers are in decimal.
The order of dimensions should be the order of their appearance on macro's formal
argument list.
For the example in previous section, the entry would look as follows:
HLP_MSEC_SCRATCH,4194304,index,262144,word,4

## Tool usage
Generate the database using the generate_reg_db.py script - this needs to be done only once. Redirect
the output to a file.
Then, pass the generated file's path as addr2macro's first argument. The tool will accept _decimal_
addresses on the input and print the best macro "call" it can guess on the output.
