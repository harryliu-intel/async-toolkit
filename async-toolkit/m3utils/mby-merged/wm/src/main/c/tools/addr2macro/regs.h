#pragma once

#include <cstdint>
#include <string>
#include <tuple>
#include <array>
#include <new>
#include <exception>
#include <sstream>
#include <vector>
#include <map>
#include <istream>
#include <fstream>
#include <iostream>
#include <algorithm>

class dimension {
public:
	dimension(const std::string& name, size_t stride, uint16_t pos) : name(name), stride(stride), pos(pos)
	{

	}

	dimension() = default;
	dimension(const dimension&) = default;
	dimension(dimension&&) = default;

	dimension& operator=(const dimension&) = default;

	std::tuple<size_t, size_t> un_offset(size_t offset) const
	{
		return std::make_tuple(offset / stride, offset % stride);
	}

	uint16_t get_position() const
	{
		return pos;
	}
private:
	std::string name;
	size_t stride;
	/* Position in the macro's argument list */
	uint16_t pos;

	friend bool operator< (const dimension& l, const dimension& r);
};

bool operator< (const dimension& l, const dimension& r)
{
	return l.stride > r.stride;
}

template <size_t N>
class reg {
public:
	reg(const std::string name, size_t base) : name(name), base(base), dimensions(), no_dimensions(0)
	{
	}

	template <class I>
	reg(const std::string name, size_t base, const I& dimensions) : name(name), base(base), dimensions(init_dim(dimensions)), no_dimensions(dimensions.size())
	{
	}

	explicit reg(const std::vector<std::string>& fields)
	{
		if (fields.size() < 2 || fields.size() % 2 == 1)
			throw std::runtime_error("invalid CSV entry");

		auto& name_field = fields[0];
		auto& base_field = fields[1];

		if (name_field.size() == 0 || base_field.size() == 0)
			throw std::runtime_error("name or base of register missing");

		name = name_field;

		std::stringstream conv;
		conv << base_field;
		conv >> base;

		if (conv.fail())
			throw std::runtime_error("failed reading base addr");

		uint16_t dim_count = 0;

		std::vector<dimension> dims;

		for (uint16_t i = 2;i < fields.size();i+=2)
		{
			auto dim_name = fields[i];
			auto dim_stride = fields[i+1];
			size_t dim_stride_num;

			if (dim_name.size() == 0 || dim_stride.size() == 0)
				throw std::runtime_error("name or stride of dimension is missing");

			std::stringstream conv;
			conv << dim_stride;
			conv >> dim_stride_num;

			if (conv.fail())
				throw std::runtime_error("failed reading stride");

 			dims.push_back(dimension(name, dim_stride_num, dim_count));
			++dim_count;
		}

		no_dimensions = dim_count;

		std::sort(dims.begin(), dims.end());

		for (uint16_t i = 0;i < dim_count;++i)
			dimensions[i] = std::move(dims[i]);
	}

	reg(const reg&) = default;

	void append_dimension(const dimension& dim)
	{
		if (no_dimensions >= N)
			throw std::runtime_error("attempting to append more dimensions than compiled for");
		dimensions[no_dimensions] = dim;
		++no_dimensions;
	}

	size_t get_base_address() const
	{
		return base;
	}

	std::string macrofy(size_t memory_address) const
	{
		if (memory_address < base) throw std::runtime_error("address in memory not in register's area");

		auto offset = memory_address - base;
		std::stringstream ret;
		std::vector<size_t> args(no_dimensions);

		ret << std::hex;

		ret << name << "(";

		for (size_t i = 0;i < no_dimensions;++i)
		{
			auto result = dimensions[i].un_offset(offset);

			offset = std::get<1>(result);

			args[dimensions[i].get_position()] = std::get<0>(result);
		}

		for (size_t i = 0;i < no_dimensions;++i)
		{
			ret << "0x" << args[i];
			if (i != no_dimensions-1)
				ret << ", ";
		}

		ret << ")";

		if (offset > 0)
			ret << " + " << "0x" << offset;

		return ret.str();
	}

private:
	std::string name;
	size_t base;

	/* Should be in descending order of strides */
	std::array<dimension, N> dimensions;
	size_t no_dimensions;
};


static std::vector<std::string> split2vector(const std::string& in, char delimiter)
{
	std::vector<std::string> ret;
	size_t pos = 0;
	while (pos < in.size())
	{
		auto next = in.find(delimiter, pos);
		ret.push_back(in.substr(pos, next-pos));
		if (next == std::string::npos) break;
		pos = next+1;
	}
	return ret;
}

template <size_t N>
class all_regs
{
public:
	using reg_type = reg<N>;

	explicit all_regs(std::istream& is)
	{
		load_from_stream(is);
	}

	std::string macrofy(size_t memory_address) const
	{
		auto i = regs.upper_bound(memory_address);
		--i;
		if (i == regs.end())
		{
			// no register was found
			std::stringstream ret;
			ret << memory_address;
			return ret.str();
		}
		return std::get<1>(*i).macrofy(memory_address);
	}

private:
	void append_reg(const reg_type& one_reg)
	{
		regs.emplace(std::make_pair(one_reg.get_base_address(), one_reg));
	}

	void load_from_stream(std::istream& is)
	{
		for (;;)
		{
			char buffer[1204];
			is.getline(buffer, 1024);

			if (is.eof()) break;

			std::string line(buffer);

			auto fields = split2vector(line, ',');

			append_reg(reg_type(fields));
		}
	}

	std::map<size_t, reg_type> regs;
};
