#include <cstdio>
#include <cstdlib>
#include <cstring>

/************************************************

This is for post processing RAM or VRAM dump
taken from emulator or reconstructed from picture,
of running M68k Opcode Sizes Test ROM.

The reason is that some of opcodes are skipped,
and some of them are reported wrong.

Several options provided, just check them out.

************************************************/

void usage()
{
	printf(
"Usage:\n"
"  m68k_opcode_sizes [command] RAM_file output\n"
"\n"
"  command:\n"
"    fix - fix wrong lengths and outputs result\n"
"    sizes - prints all lengths\n"
"    valid - dumps all valid opcodes\n"
"            including illegal opcode\n"
"    all   - dumps all opcodes including bad ones\n"
"\n"
"    valid_print - same as \"valid\" but prints offsets\n"
"    all_print - same as \"all\" but prints offsets\n"
"\n"
"  WARNING: all outputs will be overwriten!\n"
"\n"
"  Author: r57shell@uralweb.ru\n"
"  Last update: 01.09.2017\n"
);
	exit(0);
}

// override size of opcode
// returns -1 if no override, and words count else
int over(int x)
{
	if ((x & 0xF1F8) == 0x51C8) // dbcc
		return 2;
	if ((x & 0xFE00) == 0x6000 // bra, bsr
	 || (x & 0xF100) == 0x6000) // bcc
	{
		if (x & 0xFF)
			return 1;
		else
			return 2;
	}
	if ((x & 0xFF80) == 0x4E80) // jsr, jmp
	{
		if ((x & 0x38) == 0x10) // (an)
			return 1;
		if ((x & 0x38) == 0x28) // (d16,An)
			return 2;
		if ((x & 0x38) == 0x30) // (d8,An,Xn)
			return 2;
		if ((x & 0x3F) == 0x38  // (xxx).w
		 || (x & 0x3F) == 0x3A  // (d16,pc)
		 || (x & 0x3F) == 0x3B) // (d8,pc)
			return 2;
		if ((x & 0x3F) == 0x39) // (xxx).l
			return 3;
	}
	if ((x & 0xE1C0) == 0x2040) // movea
	{
		if ((x & 0x3F) < 5*8) // mode
			return 1;
		if ((x & 0x3F) == 0x39) // (xxx).l
			return 3;
		if ((x & 0x3F) < 0x3C)
			return 2;
		if ((x & 0x3F) == 0x3C) // #imm
		{
			if (x & 0x1000)
				return 2;
			else
				return 3;
		}
	}
	if (x == 0x4E72) // stop
		return 2;
	if (x == 0x4FF9) // lea (xxx).l,a7
		return 3;
	if (x == 0xDFFC) // adda (xxx).l,a7
		return 3;
	return -1;
}

// constructs opcode almost same as in test ROM
// the difference is in some opcodes
// with byte access mode and immediate operand
// and those immediate has high byte equal to zero here
// but in test ROM they all have value #$F000
void make(FILE *f, int op, int len, bool print)
{
	if (!len)
		return;
	unsigned char buff[0x100];
	if (print)
		printf("%X: %X\n", ftell(f), op);
	buff[0] = op>>8;
	buff[1] = op;
	for (int i=1; i<len; ++i)
	{
		buff[i*2] = 0xF0;
		buff[i*2+1] = 0;
	}
	if (((op & 0xFF) == 0x3C && (op >> 8) < 0x10) // btst, bchg, bset, bclr
	 || (op & 0xFF00) == 0x0800)
		buff[2] = 0; // overwrite high byte of immediate value
	if (fwrite(buff, 1, 2*len, f) != 2*len)
	{
		fclose(f);
		printf("Unable to write file\nPerhaps disk is full?\n");
		exit(4);
	}
}

unsigned char RAM[0x8000];

// reads RAM from file and applies override of opcode sizes
void fix(const char *in)
{
	FILE *f = fopen(in, "rb");
	if (!f)
	{
		printf("Can't open file %s\n", in);
		exit(1);
	}
	if (fread(RAM, 1, 0x8000, f) != 0x8000)
	{
		fclose(f);
		printf("RAM file has size less than %d\n", 0x8000);
		exit(2);
	}
	fclose(f);

	for (int op=0; op<0x10000; ++op)
	{
		int v = over(op);
		if (v >= 0)
		{
			if (op&1)
				RAM[op>>1] = (RAM[op>>1]&(~0xF))|v;
			else
				RAM[op>>1] = (RAM[op>>1]&(~0xF0))|(v<<4);
		}
	}
}

int get_len(int opcode)
{
	return (opcode & 1) ?(RAM[opcode>>1] & 0xF) : (RAM[opcode>>1] >> 4);
}

void make_fix(const char *in, const char *out)
{
	fix(in);
	FILE *f = fopen(out, "wb");
	if (!f)
	{
		printf("Can't open file %s\n", out);
		exit(3);
	}
	if (fwrite(RAM, 1, 0x8000, f) != 0x8000)
	{
		fclose(f);
		printf("Unable to write file %s\nPerhaps disk is full?\n", out);
		exit(4);
	}
	fclose(f);
}

void make_sizes(const char *in, const char *out)
{
	fix(in);
	FILE *f = fopen(out, "w+");
	if (!f)
	{
		printf("Can't open file %s\n", out);
		exit(3);
	}
	for (int op=0; op<0x10000; ++op)
	{
		fprintf(f, "%04X: %d\n", op, get_len(op));
	}
	fclose(f);
}

void make_valid(const char *in, const char *out, bool print)
{
	fix(in);
	FILE *f = fopen(out, "wb");
	if (!f)
	{
		printf("Can't open file %s\n", out);
		exit(3);
	}
	for (int op=0; op<0x10000; ++op)
	{
		int len = get_len(op);
		if (op == 0x4AFC) // illegal
			len = 1;
		make(f, op, len, print);
	}
	fclose(f);
}

void make_all(const char *in, const char *out, bool print)
{
	fix(in);
	FILE *f = fopen(out, "wb");
	if (!f)
	{
		printf("Can't open file %s\n", out);
		exit(3);
	}
	for (int op=0; op<0x10000; ++op)
	{
		int len = get_len(op);
		if (len < 1)
			len = 1;
		make(f, op, len, print);
	}
	fclose(f);
}

int main(int argc, char**args)
{
	if (argc != 4)
		usage();
	if (!strcmp(args[1], "fix"))
		return make_fix(args[2], args[3]);
	if (!strcmp(args[1], "sizes"))
		return make_sizes(args[2], args[3]);
	if (!strcmp(args[1], "valid"))
		return make_valid(args[2], args[3], false);
	if (!strcmp(args[1], "valid_print"))
		return make_valid(args[2], args[3], true);
	if (!strcmp(args[1], "all"))
		return make_all(args[2], args[3], false);
	if (!strcmp(args[1], "all_print"))
		return make_all(args[2], args[3], true);
	printf("Unrecognized command: %s\n", args[1]);
	usage();
	return 0;
}
