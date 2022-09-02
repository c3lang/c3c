// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
#include "compiler_internal.h"
#include "x86.h"

#define ASM_PTR_HASH(name__) (uint32_t)(((uintptr_t)name__ >> 9) ^ ((uintptr_t)name__ >> 1))

const Clobbers NO_CLOBBER = {};

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

static inline void reg_instr_clob(const char *name, Clobbers mask, ...)
{
	AsmInstruction *instr = insert_instruction_named(name);
	instr->mask = mask;
	va_list list;
	va_start(list, mask);
	unsigned param_count = 0;
	while (1)
	{
		AsmArgGroup group = va_arg(list, AsmArgGroup);
		if (!group) break;
		assert(param_count <= MAX_ASM_INSTRUCTION_PARAMS);
		instr->param[param_count++] = group;
	}
	va_end(list);
	instr->param_count = param_count;
}

static inline void reg_instr(const char *name, ...)
{
	AsmInstruction *instr = insert_instruction_named(name);
	instr->mask = NO_CLOBBER;
	va_list list;
	va_start(list, name);
	int param_count = 0;
	while (1)
	{
		AsmArgGroup group = va_arg(list, AsmArgGroup);
		if (!group) break;
		assert(param_count <= MAX_ASM_INSTRUCTION_PARAMS);
		instr->param[param_count++] = group;
	}
	va_end(list);
	instr->param_count = param_count;
}

INLINE void reg_register(const char *name, AsmArgGroup param, unsigned clobber_id)
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
			*reg = (AsmRegister) { .name = interned, .type = param, .clobber_index = clobber_id };
			return;
		}
		slot = (slot + 1) & ASM_REGISTER_MASK;
	}
}

INLINE void reg_register_list(const char **names, unsigned count, AsmArgGroup param, unsigned first_clobber)
{
	for (unsigned i = 0; i < count; i++) reg_register(names[i], param, i + first_clobber);
}

AsmInstruction *asm_instr_by_name(const char *name)
{
	uint32_t hash = ASM_PTR_HASH(name);
	uint32_t slot = hash & ASM_INSTRUCTION_MASK;
	while (1)
	{
		AsmInstruction *inst = &asm_target.instructions[slot];
		if (inst->name == name) return inst;
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
	error_exit("Aarch64 asm not complete.");
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
	bool is_x64 = platform_target.arch == ARCH_TYPE_X86_64;
	if (!is_x64)
	{
		reg_instr_clob("aaa", rax_mask, 0);
		reg_instr("int", AARG_IMM8, 0);
	}
	if (is_x64)
	{
		reg_instr_clob("syscall", clobbers_make_from(cc_flag_mask, X86_R11, X86_RCX, -1), 0);
	}
	reg_instr_clob("adcb", cc_flag_mask, AARG_RM8 | AARG_W, AARG_RM8 | AARG_IMM8, 0);
	reg_instr_clob("adcw", cc_flag_mask, AARG_RM16 | AARG_W, AARG_RM16 | AARG_IMM16 | AARG_IMMI8, 0);
	reg_instr_clob("adcl", cc_flag_mask, AARG_RM32 | AARG_W, AARG_RM32 | AARG_IMM32 | AARG_IMMI8, 0);
	reg_instr_clob("adcq", cc_flag_mask, AARG_RM64 | AARG_W, AARG_RM64 | AARG_IMMI32 | AARG_IMMI8, 0);
	reg_instr_clob("addb", cc_flag_mask, AARG_RM8 | AARG_W, AARG_RM8 | AARG_IMM8, 0);
	reg_instr_clob("addw", cc_flag_mask, AARG_RM16 | AARG_W, AARG_RM16 | AARG_IMM16 | AARG_IMMI8, 0);
	reg_instr_clob("addl", cc_flag_mask, AARG_RM32 | AARG_W, AARG_RM32 | AARG_IMM32 | AARG_IMMI8, 0);
	reg_instr_clob("addq", cc_flag_mask, AARG_RM64 | AARG_W, AARG_RM64 | AARG_IMMI32 | AARG_IMMI8, 0);

	reg_instr_clob("cbtw", rax_mask, 0);
	reg_instr_clob("cwtl", rax_mask, 0);
	reg_instr_clob("cltq", rax_mask, 0);
	reg_instr_clob("clc", rax_mask, 0);
	reg_instr_clob("cld", rax_mask, 0);
	reg_instr("clflush", AARG_M8, 0);
	reg_instr("movb", AARG_RM8 | AARG_W, AARG_RM8 | AARG_IMM8, 0);
	reg_instr("movsbw", AARG_R16 | AARG_W, AARG_RM8, 0);
	reg_instr("movzbw", AARG_R16 | AARG_W, AARG_RM8, 0);
	reg_instr("movsbl", AARG_RM32 | AARG_W, AARG_RM8, 0);
	reg_instr("movzbl", AARG_RM32 | AARG_W, AARG_RM8, 0);
	reg_instr("movsbq", AARG_RM64 | AARG_W, AARG_RM8, 0);
	reg_instr("movzbq", AARG_RM64 | AARG_W, AARG_RM8, 0);
	reg_instr("movw", AARG_RM16 | AARG_W, AARG_R16 | AARG_IMM16 | AARG_SEG, 0);
	reg_instr("movswl", AARG_RM32 | AARG_W, AARG_RM16, 0);
	reg_instr("movzwl", AARG_RM32 | AARG_W, AARG_RM16, 0);
	reg_instr("movswq", AARG_RM64 | AARG_W, AARG_RM16, 0);
	reg_instr("movzwq", AARG_RM64 | AARG_W, AARG_RM16, 0);
	reg_instr("movl", AARG_RM32 | AARG_W, AARG_RM32 | AARG_IMM32, 0);
	reg_instr("movslq", AARG_RM64 | AARG_W, AARG_RM32, 0);
	reg_instr("movzlq", AARG_RM64 | AARG_W, AARG_RM32, 0);
	reg_instr("movq", AARG_RM64 | AARG_W, AARG_RM64 | AARG_IMMI32 | AARG_IMM64 | AARG_SEG, 0);
	reg_instr("mov", AARG_SEG | AARG_W, AARG_R16 | AARG_R64, 0);
	reg_instr("movabsb", AARG_R8 | AARG_IMM64, AARG_R8 | AARG_IMM64, 0); // Missing segment
	reg_instr("movawsw", AARG_R16 | AARG_IMM64, AARG_R16 | AARG_IMM64, 0); // Missing segment
	reg_instr("movalsl", AARG_R32 | AARG_IMM64, AARG_R32 | AARG_IMM64, 0); // Missing segment
	reg_instr("movaqsq", AARG_R64 | AARG_IMM64, AARG_R64 | AARG_IMM64, 0); // Missing segment
	reg_instr("nop", 0);
	reg_instr("nopw", 0);
	reg_instr("nopl", 0);
	reg_instr_clob("orb", cc_flag_mask, AARG_RM8 | AARG_RW, AARG_RM8 | AARG_IMM8, 0);
	reg_instr_clob("orw", cc_flag_mask, AARG_RM16 | AARG_RW, AARG_RM16 | AARG_IMM16, 0);
	reg_instr_clob("orl", cc_flag_mask, AARG_RM32 | AARG_RW, AARG_RM32 | AARG_IMM32, 0);
	reg_instr_clob("orq", cc_flag_mask, AARG_RM64 | AARG_RW, AARG_RM64 | AARG_IMMI32 | AARG_IMM64, 0);
	reg_instr_clob("negb", cc_flag_mask, AARG_RM8 | AARG_RW, 0);
	reg_instr_clob("negw", cc_flag_mask, AARG_RM16 | AARG_RW, 0);
	reg_instr_clob("negl", cc_flag_mask, AARG_RM32 | AARG_RW, 0);
	reg_instr_clob("negq", cc_flag_mask, AARG_RM64 | AARG_RW, 0);
	reg_instr("notb", AARG_RM8 | AARG_RW, 0);
	reg_instr("notw", AARG_RM16 | AARG_RW, 0);
	reg_instr("notl", AARG_RM32 | AARG_RW, 0);
	reg_instr("notq", AARG_RM64 | AARG_RW, 0);
	reg_instr_clob("xorb", cc_flag_mask, AARG_RM8 | AARG_RW, AARG_RM8 | AARG_IMM8, 0);
	reg_instr_clob("xorw", cc_flag_mask, AARG_RM16 | AARG_RW, AARG_RM16 | AARG_IMMI8 | AARG_IMM16, 0);
	reg_instr_clob("xorl", cc_flag_mask, AARG_RM32 | AARG_RW, AARG_RM32 | AARG_IMMI8 | AARG_IMM32, 0);
	reg_instr_clob("xorq", cc_flag_mask, AARG_RM64 | AARG_RW, AARG_RM64 | AARG_IMMI8 | AARG_IMMI32 | AARG_IMM64, 0);

	asm_target.clobber_name_list = X86ClobberNames;
	asm_target.extra_clobbers = "~{flags},~{dirflag},~{fspr}";
	if (platform_target.arch == ARCH_TYPE_X86)
	{
		reg_register_list(x86_long_regs, 8, AARG_R32, X86_FIRST_RI_CLOBBER);
		reg_register_list(x86_word_regs, 8, AARG_R16, X86_FIRST_RI_CLOBBER);
		reg_register_list(x86_low_byte_regs, 8, AARG_R8, X86_FIRST_RI_CLOBBER);
		reg_register_list(x86_xmm_regs, 8, AARG_XMM, X86_MM0);
	}
	else
	{
		reg_register_list(x64_quad_regs, 15, AARG_R64, X86_FIRST_RI_CLOBBER);
		reg_register_list(x86_long_regs, 15, AARG_R32, X86_FIRST_RI_CLOBBER);
		reg_register_list(x86_word_regs, 15, AARG_R16, X86_FIRST_RI_CLOBBER);
		reg_register_list(x86_low_byte_regs, 15, AARG_R8, X86_FIRST_RI_CLOBBER);
		reg_register_list(x86_high_byte_regs, 4, AARG_R8, X86_FIRST_RI_CLOBBER);
		reg_register_list(x86_xmm_regs, 16, AARG_XMM, X86_XMM0);
		reg_register_list(x86_ymm_regs, 16, AARG_XMM, X86_YMM0);
	}
}
void init_asm(void)
{
	if (asm_target.initialized) return;
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
			break;
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
