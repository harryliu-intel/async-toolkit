#include "regs.h"

#include <fstream>
#include <iostream>
#include <string>

static std::tuple<std::string, std::string> split_input(const std::string& in)
{
	auto pos_of_eq = in.find('=');
	auto addr = in.substr(0, pos_of_eq);
	std::string rest; if(pos_of_eq != std::string::npos) rest = in.substr(pos_of_eq);
	return std::make_tuple(addr, rest);
}

int main(int argc, char* argv[])
{
	if (argc != 2 && argc != 4)
	{
		std::cerr << "Usage:\n\t" << argv[0] << " [REGISTER_DATABASE]\n"
				"\tor\n"
				"\t" << argv[0] << " [REGISTER_DATABASE] [INPUT_FILE] [OUTPUT_FILE]\n";
		return -1;
	}
	std::string db_fn(argv[1]);

	std::ifstream db_is(db_fn);

	if (db_is.fail())
		throw std::runtime_error("can't open register DB");

	all_regs<4> regs(db_is);

	bool redirect_mode = argc == 4;

	std::ifstream in_file;
	std::ofstream out_file;

	if (redirect_mode)
	{
		// disable buffering so it plays nicer with FIFOs
		in_file.rdbuf()->pubsetbuf(0, 0);
		out_file.rdbuf()->pubsetbuf(0, 0);
		in_file.open(argv[2]);
		out_file.open(argv[3]);
		if (in_file.fail() || out_file.fail())
			throw std::runtime_error("can't open input/output file");
	}

	std::istream& input = redirect_mode ? in_file : std::cin;
	std::ostream& output = redirect_mode ? out_file : std::cout;

	std::stringstream conv;

	for (;;)
	{
		char buffer[1024];
		input.getline(buffer, 1024);
		if (input.eof()) break;
		if (input.fail())
			throw std::runtime_error("line too long");

		auto in_split = split_input(std::string(buffer));
		conv << std::get<0>(in_split);

		size_t addr;
		conv >> addr;
		if (conv.fail())
			throw std::runtime_error("invalid input address");

		output << regs.macrofy(addr) << std::get<1>(in_split) << std::endl;

		conv.str(std::string());
		conv.clear();
	}

	return 0;
}

