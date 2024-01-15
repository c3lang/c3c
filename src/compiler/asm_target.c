// Copyright (c) 2022-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
#include "compiler_internal.h"
#include "compiler/asm/x86.h"
#include "compiler/asm/aarch64.h"

#define ASM_PTR_HASH(name__) (uint32_t)(((uintptr_t)name__  * 31) ^ ((uintptr_t)name__ >> 15))

const Clobbers NO_CLOBBER = { .mask[0] = 0 };

INLINE AsmInstruction *insert_instruction_named(const char *name)
{
	TokenType token_type = TOKEN_IDENT;
	unsigned len = (unsigned)strlen(name);
	const char *interned = symtab_add(name, len, fnv1a(name, len), &token_type);
	uint32_t hash = ASM_PTR_HASH(interned);
	uint32_t slot = hash & ASM_INSTRUCTION_MASK;
	while (1)
	{
		AsmInstruction *instr = &asm_target.instructions[slot];
		if (!instr->name)
		{
			instr->name = interned;
			return instr;
		}
		slot = (slot + 1) & ASM_INSTRUCTION_MASK;
	}

}

INLINE AsmArgBits parse_bits(const char **desc)
{
	if (memcmp("80", *desc, 2) == 0)
	{
		*desc += 2;
		return ARG_BITS_80;
	}
	if (memcmp("8", *desc, 1) == 0)
	{
		*desc += 1;
		return ARG_BITS_8;
	}
	if (memcmp("16", *desc, 2) == 0)
	{
		*desc += 2;
		return ARG_BITS_16;
	}
	if (memcmp("32", *desc, 2) == 0)
	{
		*desc += 2;
		return ARG_BITS_32;
	}
	if (memcmp("64", *desc, 2) == 0)
	{
		*desc += 2;
		return ARG_BITS_64;
	}
	if (memcmp("128", *desc, 3) == 0)
	{
		*desc += 3;
		return ARG_BITS_128;
	}
	if (memcmp("256", *desc, 3) == 0)
	{
		*desc += 3;
		return ARG_BITS_256;
	}
	if (memcmp("512", *desc, 3) == 0)
	{
		*desc += 3;
		return ARG_BITS_512;
	}
	error_exit("Invalid bits: %s.", *desc);
}

INLINE AsmArgType decode_arg_type(const char **desc)
{
	AsmArgType arg_type = { .is_readwrite = false };
	if (**desc == 'w')
	{
		arg_type.is_write = true;
		assert((*desc)[1] == ':');
		*desc += 2;
	}
	else if (**desc == 'r' && (*desc)[1] == 'w')
	{
		arg_type.is_readwrite = true;
		arg_type.is_write = true;
		assert((*desc)[2] == ':');
		*desc += 3;
	}
	char c;
	while ((c = ((*desc)++)[0]) != 0 && c != ',')
	{
		switch (c)
		{
			case 'r':
				arg_type.ireg_bits |= parse_bits(desc);
				goto NEXT;
			case 'm':
				if (memcmp("mem", &(*desc)[-1], 3) == 0)
				{
					arg_type.is_address = true;
					*desc += 2;
					goto NEXT;
				}
				error_exit("Unexpected string %s", &(*desc)[-1]);
			case 'v':
				arg_type.vec_bits |= parse_bits(desc);
				goto NEXT;
			case 'i':
				if (memcmp("mm", *desc, 2) == 0)
				{
					*desc += 2;
					c = **desc;
					if (c == 'i')
					{
						(*desc)++;
						arg_type.imm_arg_ibits |= parse_bits(desc);
						goto NEXT;
					}
					if (c == 'u')
					{
						desc++;
						arg_type.imm_arg_ubits |= parse_bits(desc);
						goto NEXT;
					}
					AsmArgBits bits = parse_bits(desc);
					arg_type.imm_arg_ibits |= bits;
					arg_type.imm_arg_ubits |= bits;
					goto NEXT;
				}
			default:
				error_exit("Unexpected string '%s'.", &(*desc)[-1]);
		}
NEXT:
		switch (**desc)
		{
			case '/':
				(*desc)++;
				continue;
			case ',':
				(*desc)++;
				while (**desc == ' ') (*desc)++;
				FALLTHROUGH;
			case 0:
				return arg_type;
			default:
				error_exit("Expected '/' or end: '%s'.", desc);
		}
	}
	return arg_type;
}
static inline void reg_instr_clob(const char *name, Clobbers mask, const char *args)
{
	AsmInstruction *instr = insert_instruction_named(name);
	instr->mask = mask;
	unsigned param_count = 0;
	while (args && args[0] != 0)
	{
		assert(param_count <= MAX_ASM_INSTRUCTION_PARAMS);
		instr->param[param_count++] = decode_arg_type(&args);
	}
	instr->param_count = param_count;
}

static inline void reg_instr(const char *name, const char *args)
{
	AsmInstruction *instr = insert_instruction_named(name);
	instr->mask = NO_CLOBBER;
	int param_count = 0;
	while (args && args[0] != 0)
	{
		assert(param_count <= MAX_ASM_INSTRUCTION_PARAMS);
		instr->param[param_count++] = decode_arg_type(&args);
	}
	instr->param_count = param_count;
}

INLINE void reg_register(const char *name, AsmRegisterType reg_type, AsmArgBits bits, unsigned clobber_id)
{
	TokenType token_type = TOKEN_CT_IDENT;
	unsigned len = (unsigned)strlen(name);
	const char *interned = symtab_add(name, len, fnv1a(name, len), &token_type);
	uint32_t hash = ASM_PTR_HASH(interned);
	uint32_t slot = hash & ASM_REGISTER_MASK;
	while (1)
	{
		AsmRegister *reg = &asm_target.registers[slot];
		if (!reg->name)
		{
			*reg = (AsmRegister) { .name = interned, .type = reg_type, .bits = bits, .clobber_index = clobber_id };
			asm_target.register_count++;
			return;
		}
		slot = (slot + 1) & ASM_REGISTER_MASK;
	}
}

INLINE void reg_register_list(const char **names, unsigned count, AsmRegisterType param, unsigned bitsize, unsigned first_clobber)
{
	for (unsigned i = 0; i < count; i++) reg_register(names[i], param, bitsize, i + first_clobber);
}

AsmInstruction *asm_instr_by_name(const char *name)
{
	uint32_t hash = ASM_PTR_HASH(name);
	uint32_t slot = hash & ASM_INSTRUCTION_MASK;
	while (1)
	{
		AsmInstruction *inst = &asm_target.instructions[slot];
		if (inst->name == name) return inst;
		if (inst->name == NULL) return NULL;
		slot = (slot + 1) & ASM_INSTRUCTION_MASK;
	}
}

AsmRegister *asm_reg_by_name(const char *name)
{
	uint32_t hash = ASM_PTR_HASH(name);
	uint32_t slot = hash & ASM_REGISTER_MASK;
	while (1)
	{
		AsmRegister *reg = &asm_target.registers[slot];
		const char *reg_name = reg->name;
		if (reg_name == name) return reg;
		if (!reg_name) return NULL;
		slot = (slot + 1) & ASM_REGISTER_MASK;
	}
}


static void init_asm_aarch64(void)
{
//	Clobbers cc_flag_mask = clobbers_make(AARCH64_CC, -1);
	asm_target.clobber_name_list = Aarch64ClobberNames;
	asm_target.extra_clobbers = NULL;
	reg_instr("ldr", "w:r32/r64, mem"); // Could be separated
	reg_instr("ldrb", "w:r32, mem");
	reg_instr("ldrsb", "w:r32/r64, mem");
	reg_instr("ldrh", "w:r32, mem");
	reg_instr("ldrsh", "w:r32/r64, mem");
	reg_instr("ldrsw", "w:r64, mem");
	reg_instr("ldp", "w:r32/r64, w:r32/r64, mem");
	reg_instr("ldpsw", "w:r32/r64, w:r32/r64, mem");
	reg_instr("str", "r32/r64, w:mem");
	reg_instr("strb", "r32/r64, w:mem");
	reg_instr("strh", "r32/r64, w:mem");
	reg_instr("stp", "r32/r64, r32/r64, w:mem");
	reg_instr("mov", "w:r32/r64, mem");
	reg_register_list(aarch64_quad_regs, 32, ASM_REG_INT, ARG_BITS_64, AARCH64_R0);
	reg_register_list(aarch64_long_regs, 32, ASM_REG_INT, ARG_BITS_32, AARCH64_R0);
	reg_register_list(aarch64_f128_regs, 32, ASM_REG_FLOAT, ARG_BITS_128, AARCH64_Q0);
	reg_register_list(aarch64_double_regs, 32, ASM_REG_FLOAT, ARG_BITS_64, AARCH64_Q0);
	reg_register_list(aarch64_float_regs, 32, ASM_REG_FLOAT, ARG_BITS_32, AARCH64_Q0);
	reg_register_list(aarch64_f16_regs, 32, ASM_REG_FLOAT, ARG_BITS_16, AARCH64_Q0);
	reg_register_list(aarch64_f8_regs, 32, ASM_REG_FLOAT, ARG_BITS_8, AARCH64_Q0);
	reg_register_list(aarch64_v8b_regs, 32, ASM_REG_IVEC, ARG_BITS_64, AARCH64_FIRST_RV_CLOBBER);
	reg_register_list(aarch64_v16b_regs, 32, ASM_REG_IVEC, ARG_BITS_128, AARCH64_FIRST_RV_CLOBBER);
	reg_register_list(aarch64_v4h_regs, 32, ASM_REG_IVEC, ARG_BITS_64, AARCH64_FIRST_RV_CLOBBER);
	reg_register_list(aarch64_v8h_regs, 32, ASM_REG_IVEC, ARG_BITS_128, AARCH64_FIRST_RV_CLOBBER);
	reg_register_list(aarch64_v2s_regs, 32, ASM_REG_IVEC, ARG_BITS_64, AARCH64_FIRST_RV_CLOBBER);
	reg_register_list(aarch64_v4s_regs, 32, ASM_REG_IVEC, ARG_BITS_128, AARCH64_FIRST_RV_CLOBBER);
	reg_register_list(aarch64_v1d_regs, 32, ASM_REG_IVEC, ARG_BITS_64, AARCH64_FIRST_RV_CLOBBER);
	reg_register_list(aarch64_v2d_regs, 32, ASM_REG_IVEC, ARG_BITS_128, AARCH64_FIRST_RV_CLOBBER);
	reg_register("$sp", ASM_REG_INT, ARG_BITS_64, AARCH64_R31);
}

static void init_asm_wasm(void)
{
	error_exit("WASM asm not complete.");
}

static void init_asm_arm(void)
{
	error_exit("ARM asm not complete.");
}

static void init_asm_riscv(void)
{
	error_exit("RISCV asm not complete.");
}

static void init_asm_ppc(void)
{
	error_exit("PPC asm not complete.");
}

static void init_asm_x86(void)
{
	Clobbers rax_mask = clobbers_make(X86_RAX, -1);
	Clobbers cc_flag_mask = clobbers_make(X86_CC, -1);
	Clobbers rax_cc_mask = clobbers_make_from(cc_flag_mask, X86_RAX, -1);
	Clobbers rax_rdx_cc_mask = clobbers_make_from(cc_flag_mask, X86_RAX, X86_RDX, -1);
	bool is_x64 = platform_target.arch == ARCH_TYPE_X86_64;
	if (!is_x64)
	{
		reg_instr_clob("aaa", rax_mask, 0);
		reg_instr_clob("into", cc_flag_mask, NULL);
		reg_instr("pushl", "r32/mem/imm32");
		reg_instr("popl", "w:r32/mem/imm32");
	}
	if (is_x64)
	{
		reg_instr_clob("syscall", clobbers_make_from(cc_flag_mask, X86_RAX, X86_R11, X86_RCX, -1), NULL);
		reg_instr("pushq", "r64/mem");
		reg_instr("popq", "w:r64/mem");
	}
	reg_instr_clob("adcb", cc_flag_mask, "rw:r8/mem, r8/mem/imm8");
	reg_instr_clob("adcw", cc_flag_mask, "rw:r16/mem, r16/mem/imm16/immi8");
	reg_instr_clob("adcl", cc_flag_mask, "rw:r32/mem, r32/mem/imm32/immi8");
	reg_instr_clob("adcq", cc_flag_mask, "rw:r64/mem, r64/mem/immi32/immi8");

	reg_instr_clob("adcxl", cc_flag_mask, "r32, rw:r32/mem");
	reg_instr_clob("adcxq", cc_flag_mask, "r64, rw:r64/mem");

	reg_instr_clob("addb", cc_flag_mask, "rw:r8/mem, r8/mem/imm8");
	reg_instr_clob("addw", cc_flag_mask, "rw:r16/mem, r16/mem/imm16/immi8");
	reg_instr_clob("addl", cc_flag_mask, "rw:r32/mem, r32/mem/imm32/immi8");
	reg_instr_clob("addq", cc_flag_mask, "rw:r64/mem, r64/mem/immi32/immi8");

	reg_instr("addpd", "rw:v128, v128/mem");
	reg_instr("addps", "rw:v128, v128/mem");
	reg_instr("addsd", "rw:v128, v128/mem");
	reg_instr("addss", "rw:v128, v128/mem");
	reg_instr("vaddpd", "w:v128/v256/v512, v128/v256/v512, v128/v256/v512/mem");
	reg_instr("vaddps", "w:v128/v256/v512, v128/v256/v512, v128/v256/v512/mem");
	reg_instr("vaddsd", "w:v128, v128, v128/mem");
	reg_instr("vaddss", "w:v128, v128, v128/mem");

	reg_instr_clob("cbtw", rax_mask, NULL);
	reg_instr_clob("cwtl", rax_mask, NULL);
	reg_instr_clob("cltq", rax_mask, NULL);
	reg_instr_clob("clc", rax_mask, NULL);
	reg_instr_clob("cld", rax_mask, NULL);
	reg_instr("clflush", "mem");
	reg_instr("movb", "w:r8/mem, r8/mem/imm8");
	reg_instr("movsbw", "w:r16/mem, r8/mem");
	reg_instr("movzbw", "w:r16/mem, r8/mem");
	reg_instr("movsbl", "w:r32/mem, r8/mem");
	reg_instr("movzbl", "w:r32/mem, r8/mem");
	reg_instr("movsbq", "w:r64/mem, r8/mem");
	reg_instr("movzbq", "w:r64/mem, r8/mem");
	reg_instr("movw", "w:r16/mem, r16/mem/imm16"); // Add seg
	reg_instr("movswl", "w:r32/mem, r16/mem");
	reg_instr("movzwl", "w:r32/mem, r16/mem");
	reg_instr("movswq", "w:r64/mem, r16/mem");
	reg_instr("movzwq", "w:r64/mem, r16/mem");
	reg_instr("movl", "w:r32/mem, r32/mem/imm32");
	reg_instr("movslq", "w:r64/mem, r32/mem");
	reg_instr("movzlq", "w:r64/mem, r32/mem");
	reg_instr("movq", "w:r64/mem, r64/mem/immi32/imm64"); // Seg
	//reg_instr("mov", "wAARG_SEG | AARG_W, AARG_R16 | AARG_R64, 0); // Seg
	/*
	reg_instr("movabsb", AARG_R8 | AARG_IMM64, AARG_R8 | AARG_IMM64, 0); // Missing segment
	reg_instr("movawsw", AARG_R16 | AARG_IMM64, AARG_R16 | AARG_IMM64, 0); // Missing segment
	reg_instr("movalsl", AARG_R32 | AARG_IMM64, AARG_R32 | AARG_IMM64, 0); // Missing segment
	reg_instr("movaqsq", AARG_R64 | AARG_IMM64, AARG_R64 | AARG_IMM64, 0); // Missing segment*/
	reg_instr("nop", NULL);
	reg_instr("nopw", NULL);
	reg_instr("nopl", NULL);
	reg_instr_clob("orb", cc_flag_mask, "rw:r8/mem, r8/mem/imm8");
	reg_instr_clob("orw", cc_flag_mask, "rw:r16/mem, r16/mem/imm16");
	reg_instr_clob("orl", cc_flag_mask, "rw:r32/mem, r32/mem/imm32");
	reg_instr_clob("orq", cc_flag_mask, "rw:r64/mem, r64/mem/immi32/imm64");
	reg_instr_clob("negb", cc_flag_mask, "rw:r8/mem");
	reg_instr_clob("negw", cc_flag_mask, "rw:r16/mem");
	reg_instr_clob("negl", cc_flag_mask, "rw:r32/mem");
	reg_instr_clob("negq", cc_flag_mask, "rw:r64/mem");
	reg_instr("notb", "rw:r8/mem");
	reg_instr("notw", "rw:r16/mem");
	reg_instr("notl", "rw:r32/mem");
	reg_instr("notq", "rw:r64/mem");
	reg_instr_clob("xorb", cc_flag_mask, "rw:r8/mem, r8/mem/imm8");
	reg_instr_clob("xorw", cc_flag_mask, "rw:r16/mem, r16/mem/imm16");
	reg_instr_clob("xorl", cc_flag_mask, "rw:r32/mem, r32/mem/imm32");
	reg_instr_clob("xorq", cc_flag_mask, "rw:r64/mem, r64/mem/immi32/imm64");
	reg_instr_clob("mulb", rax_cc_mask, "r8/mem");
	reg_instr_clob("mulw", rax_rdx_cc_mask, "r16/mem");
	reg_instr_clob("mull", rax_rdx_cc_mask, "r32/mem");
	reg_instr_clob("mulq", rax_rdx_cc_mask, "rw:r64/mem");
	reg_instr_clob("subb", rax_cc_mask, "rw:r8/mem, r8/mem/imm8");
	reg_instr_clob("subw", rax_cc_mask, "rw:r16/mem, r16/mem/imm16");
	reg_instr_clob("subl", rax_cc_mask, "rw:r32/mem, r32/mem/imm32");
	reg_instr_clob("subq", rax_cc_mask, "rw:r64/mem, r64/mem/immi32/imm64");
	reg_instr_clob("cpuid",  clobbers_make_from(cc_flag_mask, X86_RAX, X86_RBX, X86_RCX, X86_RDX, -1), NULL);
	reg_instr("hlt", NULL);
	reg_instr("in", "w:r8/r16/r32, r16/imm8"); // Actually ensure reg_al_ax and dx
	reg_instr_clob("incb", cc_flag_mask, "rw:r8/mem");
	reg_instr_clob("incw", cc_flag_mask, "rw:r16/mem");
	reg_instr_clob("incl", cc_flag_mask, "rw:r32/mem");
	reg_instr_clob("incq", cc_flag_mask, "rw:r64/mem");
	reg_instr("insb", NULL);
	reg_instr("insw", NULL);
	reg_instr("insl", NULL);
	reg_instr_clob("int", cc_flag_mask, "imm8");
	reg_instr_clob("int3", cc_flag_mask, NULL);
	reg_instr_clob("int1", cc_flag_mask, NULL);
	reg_instr("invd", NULL);
	reg_instr("invpcid", "r32/r64, mem");
	reg_instr("invlpg", "w:mem");
	reg_instr("invlpga", "r32, r64"); // c, a check this one!
	reg_instr("iret", NULL);
	reg_instr("iretl", NULL);
	reg_instr("iretw", NULL);
	reg_instr("iretq", NULL);
	reg_instr("rdtsc", NULL);
	reg_instr("rdtscp", NULL);
	reg_instr("ret", NULL);
	reg_instr("push", "imm8");
	reg_instr("pushw", "r16/mem/imm16");
	reg_instr("popw", "w:r16/mem");

	asm_target.clobber_name_list = X86ClobberNames;
	asm_target.extra_clobbers = "~{flags},~{dirflag},~{fspr}";
	if (platform_target.arch == ARCH_TYPE_X86)
	{
		reg_register_list(x86_long_regs, 8, ASM_REG_INT, ARG_BITS_32, X86_RAX);
		reg_register_list(x86_word_regs, 8, ASM_REG_INT, ARG_BITS_16, X86_RAX);
		reg_register_list(x86_low_byte_regs, 8, ASM_REG_INT, ARG_BITS_8, X86_RAX);
		reg_register_list(x86_float_regs, 8, ASM_REG_FLOAT, ARG_BITS_80, X86_ST0);
		reg_register_list(x86_xmm_regs, 8, ASM_REF_FVEC, ARG_BITS_128, X86_MM0);
	}
	else
	{
		reg_register_list(x64_quad_regs, 15, ASM_REG_INT, ARG_BITS_64, X86_RAX);
		reg_register_list(x86_long_regs, 15, ASM_REG_INT, ARG_BITS_32, X86_RAX);
		reg_register_list(x86_word_regs, 15, ASM_REG_INT, ARG_BITS_16, X86_RAX);
		reg_register_list(x86_low_byte_regs, 15, ASM_REG_INT, ARG_BITS_8, X86_RAX);
		reg_register_list(x86_high_byte_regs, 4, ASM_REG_INT, ARG_BITS_8, X86_RAX);
		reg_register_list(x86_xmm_regs, 16, ASM_REF_FVEC, ARG_BITS_128, X86_XMM0);
		reg_register_list(x86_ymm_regs, 16, ASM_REF_FVEC, ARG_BITS_256, X86_XMM0);
		reg_register_list(x86_zmm_regs, 16, ASM_REF_FVEC, ARG_BITS_512, X86_XMM0);
	}
}
void init_asm(void)
{
	if (asm_target.initialized) return;
	asm_target.initialized = true;
	switch (platform_target.arch)
	{
		case ARCH_UNSUPPORTED:
		case ARCH_TYPE_X86_64:
		case ARCH_TYPE_X86:
			init_asm_x86();
			return;
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_BE:
			init_asm_aarch64();
			return;
		case ARCH_TYPE_ARM:
		case ARCH_TYPE_ARMB:
		case ARCH_TYPE_THUMB:
		case ARCH_TYPE_THUMBEB:
			init_asm_arm();
			return;
		case ARCH_TYPE_WASM64:
		case ARCH_TYPE_WASM32:
			init_asm_wasm();
			return;
		case ARCH_TYPE_UNKNOWN:
			error_exit("Unknown arch does not support asm.");
			UNREACHABLE
		case ARCH_TYPE_PPC:
		case ARCH_TYPE_PPC64:
		case ARCH_TYPE_PPC64LE:
			init_asm_ppc();
			return;
		case ARCH_TYPE_RISCV32:
		case ARCH_TYPE_RISCV64:
			init_asm_riscv();
			return;
	}
	UNREACHABLE
}
