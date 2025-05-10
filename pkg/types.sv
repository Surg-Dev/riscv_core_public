package ooo_config;
    /* This package is a good place to put tunable top-level parameters or typedefs that are widely used, but don't need to be trickled down everywhere.
     * Constants that are part of the spec do _not_ belong here.
     */

    // Reminder that $clog2 operates on the VALUE and $bits operates on the TYPE

    // Register Naming
    localparam int PHYS_REGS = 40;
    localparam int ROB_DEPTH = PHYS_REGS - 32;
    localparam int LQ_DEPTH = 8;
    localparam int SQ_DEPTH = 8;
    localparam int PHYS_BITS = $clog2(PHYS_REGS);
    localparam int ROB_BITS  = $clog2(ROB_DEPTH);
    localparam int LQ_BITS  = $clog2(LQ_DEPTH);
    localparam int SQ_BITS  = $clog2(SQ_DEPTH);
    localparam int LSQ_BITS = (LQ_BITS > SQ_BITS) ? LQ_BITS : SQ_BITS;
    typedef logic [PHYS_BITS-1:0] preg;
    typedef logic [4:0] rreg;
    typedef logic [ROB_BITS:0]  rob_ent;
    typedef logic [LSQ_BITS:0]  lsq_ent;
    typedef logic [ROB_BITS-1:0] rob_cast;
    typedef logic [LSQ_BITS-1:0] lsq_cast;

    localparam int BRU_DEPTH = 4;
    localparam int BRU_BITS  = $clog2(BRU_DEPTH);
    typedef logic [BRU_DEPTH-1:0] br_mask;

    // Regfile Ports
    localparam int REGF_READ  = 5; // 5 for CP3
    localparam int REGF_WRITE = 5; // 5 for CP3

    localparam REGF_RBITS = $clog2(REGF_READ);
    localparam int REGF_WBITS = $clog2(REGF_WRITE);

    // Functional Units
    localparam int FUNC_UNITS = 5; // 5 for CP3
    localparam int FUNC_BITS  = $clog2(FUNC_UNITS);

    typedef enum logic [2:0] {
        ALU = 3'b000,
        MUL = 3'b001,
        DIV = 3'b010,
        BRU = 3'b011,
        MEM = 3'b100
     } func_units_t;

    localparam int cacheline_size = 4;
    localparam int cache_ways = 4;
    localparam int cache_sets = 16;

    // RVFI Base Struct
    typedef struct packed {
        // Arch Regs
        rreg rs1;
        rreg rs2;
        rreg rd;
        // Phys Regs (Used for Regfile Index)
        preg ps1;
        preg ps2;
        preg pd;
        // PC + Inst
        logic [31:0] pc_rdata;
        logic [31:0] pc_wdata;
        logic [31:0] inst;
    } rvfi_t;

    // RVFI mem op struct
    typedef struct packed {
        logic   [31:0]  mem_addr;
        logic   [3:0]   mem_rmask;
        logic   [3:0]   mem_wmask;
        logic   [31:0]  mem_rdata;
        logic   [31:0]  mem_wdata;
    } rvfi_mem_t;

endpackage

package cache_state;

    typedef enum logic [1:0] {
        CACHE_STATE_NONE = 2'b00,
        READ = 2'b01,
        WRITE = 2'b10,
        WRITE_STALL = 2'b11
    } cache_state_t;

    typedef struct packed {
        logic [31:0] addr;
        logic [3:0]  rmask;
        logic [3:0]  wmask;
        logic [31:0] wdata;
    } cache_stage_t;

    typedef enum logic [1:0] {
        RO_IDLE = 2'b00,
        RO_READ = 2'b01,
        RO_WRITE_STALL = 2'b10
    } ro_cache_state_t;

    typedef struct packed {
        logic [31:0] addr;
        logic [3:0]  rmask;
    } ro_cache_stage_t;

endpackage

package branch_types;
import ooo_config::*;
    // I'm literally just lazy, feel free to move to a different namespace

    typedef struct packed {
        // Is this a valid branch resolution
        logic        valid;
        // Is this instruction mispredicted?
        logic        mispred;
        // Target Address of Jump
        logic [31:0] target;

        // ROB Index to reset to
        rob_ent rob_idx;

        // LSQ Index to reset to
        lsq_ent lsq_idx;

        // Branch Mask of Resolved Branch
        br_mask branch_mask;
        // Branch Index of Resolved Branch
        logic [BRU_BITS:0] bru_idx;
    } ebr_t;

endpackage


package rv32i_types;
import ooo_config::*;
export ooo_config::func_units_t;

    // Q: Why are functions marked 'automatic'?
    // A: Systemverilog functions are static by default, and unlike e.g. functions in C (which have a function stack), they are not reentrant.
    // Q: What is the struct { enum union } design pattern?
    // A: This is a tagged union, which has been in the spec for 2 decades but hasn't seen adoption among vendors.
    //    George is trying to write custom Spade-like translators for surfer to make these extra useful during debugging.
    // Q: Why so many function calls?
    // A: Although functions in rtl can't be marked as such, all of these functions are "pure" (no side-effects) and are quite easy for the compiler/synthesizer to optimize.
    // Q: Shouldn't you assume that any value can be X at any time?
    // A: For an actual chip, yes, but this mp only gets tested with sim.
    //    Even though this technically could introduce a discrepancy between synth and sim output, there's no way to test synth.
    //    Not only does it make invalid state obvious (and easy to trace in Verdi), but it also allows for more efficient synth.
    // Q: ...why is your code so boilerplate-y?
    // A: SystemVerilog has issues with enum literal collision in the same scope (dumb) so you can't use 'none' everywhere, and there are no anonymous structs or automatic padding/bitfields for unions... you must explicitly create a struct and add padding.
    //    Alonzo Church is rolling over in his grave

    typedef enum logic [6:0] {
        op_b_lui       = 7'b0110111, // load upper immediate (U type)
        op_b_auipc     = 7'b0010111, // add upper immediate PC (U type)
        op_b_jal       = 7'b1101111, // jump and link (J type)
        op_b_jalr      = 7'b1100111, // jump and link register (I type)
        op_b_br        = 7'b1100011, // branch (B type)
        op_b_load      = 7'b0000011, // load (I type)
        op_b_store     = 7'b0100011, // store (S type)
        op_b_imm       = 7'b0010011, // arith ops with register/immediate operands (I type)
        op_b_reg       = 7'b0110011  // arith ops with register operands (R type)
    } rv32i_opcode;

    typedef enum logic [2:0] {
        arith_f3_add   = 3'b000, // check logic 30 for sub if op_reg op
        arith_f3_sll   = 3'b001,
        arith_f3_slt   = 3'b010,
        arith_f3_sltu  = 3'b011,
        arith_f3_xor   = 3'b100,
        arith_f3_sr    = 3'b101, // check logic 30 for logical/arithmetic
        arith_f3_or    = 3'b110,
        arith_f3_and   = 3'b111
    } arith_f3_t;

    // Keep for covergroups.
    typedef enum logic [2:0] {
        load_f3_lb     = 3'b000,
        load_f3_lh     = 3'b001,
        load_f3_lw     = 3'b010,
        load_f3_lbu    = 3'b100,
        load_f3_lhu    = 3'b101
    } load_f3_t;

    typedef enum logic [2:0] {
        store_f3_sb    = 3'b000,
        store_f3_sh    = 3'b001,
        store_f3_sw    = 3'b010
    } store_f3_t;

    typedef enum logic [2:0] {
        ls_f3_b         = 3'b000,
        ls_f3_h         = 3'b001,
        ls_f3_w         = 3'b010,
        ls_f3_bu        = 3'b100,   // Only used for load instr
        ls_f3_hu        = 3'b101    // Only used for load instr
    } load_store_f3_t;

    typedef enum logic [2:0] {
        branch_f3_beq  = 3'b000,
        branch_f3_bne  = 3'b001,
        branch_f3_blt  = 3'b100,
        branch_f3_bge  = 3'b101,
        branch_f3_bltu = 3'b110,
        branch_f3_bgeu = 3'b111
    } branch_f3_t;

    typedef enum logic [2:0] {
        mul_f3     = 3'b000,
        mul_f3_h   = 3'b001,
        mul_f3_hsu = 3'b010,
        mul_f3_hu  = 3'b011
    } mul_f3_t;
    typedef enum logic [2:0] {
        div_f3   = 3'b100,
        div_f3_u = 3'b101,
        rem_f3   = 3'b110,
        rem_f3_u = 3'b111
    } div_f3_t;

    typedef enum logic [1:0] {
        FUNCT7_NONE, BASE, VARIANT, RV32M // decode happens in inst_funct7
    } funct7_t;

    typedef enum logic [6:0] {
        base     = 7'b0000000,
        variant  = 7'b0100000,
        rv32m    = 7'b0000001
    } funct7_n_t;

    typedef enum logic [3:0] {
        alu_op_add     = 4'b0000,
        alu_op_sll     = 4'b0001,
        alu_op_sra     = 4'b0010,
        alu_op_sub     = 4'b0011,
        alu_op_xor     = 4'b0100,
        alu_op_srl     = 4'b0101,
        alu_op_or      = 4'b0110,
        alu_op_and     = 4'b0111,
        alu_op_slt     = 4'b1000,
        alu_op_sltu    = 4'b1001
    } alu_ops;

    typedef struct packed {
        func_units_t tag;
        union packed {
            struct packed { alu_ops val; } alu;
            struct packed {
                logic isJalr; // the "tag" for this is imm_optional. Don't ask why.
                branch_f3_t bf3;
            } bru;
            struct packed { logic is_str; logic [2:0] val; } mem; // users should match all variants in both store and load types
            struct packed { logic __pad; mul_f3_t val; } mul;
            struct packed { logic __pad; div_f3_t val; } div;
        } op;
    } exe_t;

    typedef union packed {
        logic [31:0] word;

        struct packed {
            logic [11:0] _i_imm;
            logic [4:0]  rs1;
            logic [2:0]  funct3;
            logic [4:0]  rd;
            rv32i_opcode opcode;
        } i_type;

        struct packed {
            logic [6:0]  funct7;
            logic [4:0]  rs2;
            logic [4:0]  rs1;
            logic [2:0]  funct3;
            logic [4:0]  rd;
            rv32i_opcode opcode;
        } r_type;

        struct packed {
            logic [11:5] _s_imm_top;
            logic [4:0]  rs2;
            logic [4:0]  rs1;
            logic [2:0]  funct3;
            logic [4:0]  _s_imm_bot;
            rv32i_opcode opcode;
        } s_type;

        struct packed {
            logic [11:5] _b_imm_top;
            logic [4:0]  rs2;
            logic [4:0]  rs1;
            logic [2:0]  funct3;
            logic [4:0]  _b_imm_bot;
            rv32i_opcode opcode;
        } b_type;

        struct packed {
            logic [31:12] imm;
            logic [4:0]   rd;
            rv32i_opcode  opcode;
        } j_type;

        struct packed {
            logic [31:12] imm;
            logic [4:0]   rd;
            rv32i_opcode  opcode;
        } u_type;
    } instr_t;

    typedef enum logic[2:0] {
        inst_r_type,
        inst_i_type,
        inst_s_type,
        inst_b_type,
        inst_u_type,
        inst_j_type
    } inst_type_t;


    // helper functions for decoding and checking instructions

    /* RETURNS:
     * type: what instruction type an opcode maps to.
     */
    function automatic inst_type_t inst_type(instr_t inst);
        inst_type_t t;
        unique case (inst.r_type.opcode)
            op_b_lui: t = inst_u_type;
            op_b_auipc: t = inst_u_type;
            op_b_jal: t = inst_j_type;
            op_b_jalr: t = inst_i_type;
            op_b_load: t = inst_i_type;
            op_b_imm: t = inst_i_type;
            op_b_br: t = inst_b_type;
            op_b_store: t = inst_s_type;
            op_b_reg: t = inst_r_type;
            default: t = inst_type_t'('x);
        endcase
        return t;
    endfunction

    /* RETURNS:
     * immediate: the (shifted) immediate value, or X if the instruction has no immediate operand.
     */
    function automatic [31:0] inst_imm(input instr_t inst);
        logic [31:0] imm;
        unique case (inst_type(inst))
            inst_r_type: imm = 'x; // R type (no immediate)
            inst_i_type: imm = {{21{inst.word[31]}}, inst.word[30:20]};
            inst_s_type: imm = {{21{inst.word[31]}}, inst.word[30:25], inst.word[11:7]};
            inst_b_type: imm = {{20{inst.word[31]}}, inst.word[7], inst.word[30:25], inst.word[11:8], 1'b0};
            inst_u_type: imm = {inst.word[31:12], 12'h000};
            inst_j_type: imm = {{12{inst.word[31]}}, inst.word[19:12], inst.word[20], inst.word[30:21], 1'b0};
            default: imm = 'x;
        endcase
        return imm;
    endfunction

    /* RETURNS:
     * immediate_valid: Whether or not the immidiate exists for the given instruction type.
     */
    function automatic inst_imm_optional(input instr_t inst);
        logic imm_optional;
        unique case (inst_type(inst))
            inst_r_type: imm_optional = 1'b0; // R type (no immediate)
            inst_i_type: imm_optional = 1'b1;
            inst_s_type: imm_optional = 1'b0; // whyge
            inst_b_type: imm_optional = 1'b0;
            inst_u_type: imm_optional = 1'b1;
            inst_j_type: imm_optional = 1'b1;
            default: imm_optional = '0;
        endcase
        return imm_optional;
    endfunction

    /* RETURNS:
     * rs1: logical address of the 1st source register.
     */
    function automatic [4:0] inst_rs1(instr_t inst);
        logic [4:0] rs1;
        unique case (inst_type(inst))
            inst_r_type: rs1 = inst.r_type.rs1;
            inst_i_type: rs1 = inst.i_type.rs1;
            inst_s_type: rs1 = inst.s_type.rs1;
            inst_b_type: rs1 = inst.b_type.rs1;
            inst_u_type: rs1 = '0;
            inst_j_type: rs1 = '0;
            default: rs1 = '0;
        endcase
        return rs1;
    endfunction

    /* RETURNS:
     * rs2: logical address of the 2nd source register.
     */
    function automatic [4:0] inst_rs2(instr_t inst);
        logic [4:0] rs2;
        unique case (inst_type(inst))
            inst_r_type: rs2 = inst.r_type.rs2;
            inst_i_type: rs2 = '0;
            inst_s_type: rs2 = inst.s_type.rs2;
            inst_b_type: rs2 = inst.b_type.rs2;
            inst_u_type: rs2 = '0;
            inst_j_type: rs2 = '0;
            default: rs2 = '0;
        endcase
        return rs2;
    endfunction

    /* RETURNS:
     * rd: logical address of the destination register.
     */
    function automatic [4:0] inst_rd(instr_t inst);
        logic [4:0] rd;
        unique case (inst_type(inst))
            inst_r_type: rd = inst.r_type.rd;
            inst_i_type: rd = inst.i_type.rd;
            inst_s_type: rd = '0;
            inst_b_type: rd = '0;
            inst_u_type: rd = inst.u_type.rd;
            inst_j_type: rd = inst.j_type.rd;
            default: rd = '0;
        endcase
        return rd;
    endfunction

    /* RETURNS:
     * funct3: the literal 3 bits in the rv32 instruction.
     *   More aggressive checking is probably not worth it given most '*_f3_t' types use all 8 states.
     */
    function automatic [2:0] inst_funct3(instr_t inst);
        logic [2:0] funct3;
        unique case (inst_type(inst))
            inst_r_type: funct3 = inst.r_type.funct3;
            inst_i_type: funct3 = inst.i_type.funct3;
            inst_s_type: funct3 = inst.s_type.funct3;
            inst_b_type: funct3 = inst.b_type.funct3;
            inst_u_type: funct3 = 'x;
            inst_j_type: funct3 = 'x;
            default: funct3 = 'x;
        endcase
        return funct3;
    endfunction

    /* RETURNS:
     * funct7: an enum representing additional information about which variant an instruction is (like add vs sub)
     *   This is a packed enum and is _not_ (necessarily) 7 bits long.
     *   You probably don't need to be calling this outside this file.
     */
    function automatic funct7_t inst_funct7(instr_t inst);
        funct7_t funct7 = FUNCT7_NONE;
        unique case (inst_type(inst))
            inst_r_type: begin
                unique case (inst.r_type.funct7)
                    7'b0000000: funct7 = BASE;
                    7'b0100000: funct7 = VARIANT;
                    7'b0000001: funct7 = RV32M;
                    default: ;
                endcase
            end
            inst_i_type: begin
                unique case (inst.r_type.funct7) // that's not a typo... maybe I should rename this.
                    7'b0000000: funct7 = BASE;
                    7'b0100000: funct7 = VARIANT;
                    default: ;
                endcase
            end
            default: ;
        endcase
        return funct7;
    endfunction

    /* RETURNS:
     * ret: tagged union representing microcode operation
     *   ret.tag: which of the execution units this microinstruction is bound for (alu, branch unit, memory, IP mul/div), or none if invalid
     *   ret.op: the exact operation (add, sla, mulh, lb, etc) that the execution unit will execute.
     */
    function automatic exe_t inst_op(instr_t inst);
        exe_t ret;

        unique case (inst.r_type.opcode)
            op_b_lui: ret = '{tag:ALU, op:alu_op_add};
            op_b_auipc: ret = '{tag:ALU, op:alu_op_add};

            op_b_imm: begin
                ret.tag = ALU;
                unique case (inst_funct3(inst))
                    arith_f3_slt: ret.op = alu_op_slt;
                    arith_f3_sltu: ret.op = alu_op_sltu;
                    arith_f3_sr: begin
                        if (inst_funct7(inst) == VARIANT) ret.op = alu_op_sra;
                        else if (inst_funct7(inst) == BASE) ret.op = alu_op_srl;
                    end
                    default: ret.op = alu_ops'({1'b0, inst_funct3(inst)});
                endcase
            end
            op_b_reg: begin
                ret.tag = ALU;
                case (inst_funct7(inst))
                    BASE: begin
                        unique case (inst_funct3(inst))
                            arith_f3_slt: ret.op = alu_op_slt;
                            arith_f3_sltu: ret.op = alu_op_sltu;
                            arith_f3_sr: ret.op = alu_op_srl;
                            arith_f3_add: ret.op = alu_op_add; // different from op_b_imm
                            default: ret.op = alu_ops'({1'b0, inst_funct3(inst)});
                        endcase
                    end
                    VARIANT: begin
                        unique case (inst_funct3(inst))
                            arith_f3_sr: ret.op = alu_op_sra;
                            arith_f3_add: ret.op = alu_op_sub;
                            default: ret.op = alu_ops'('x);
                        endcase
                    end
                    RV32M: begin
                        case (inst_funct3(inst))
                            mul_f3, mul_f3_h, mul_f3_hsu, mul_f3_hu: ret.tag = MUL;
                            div_f3, div_f3_u, rem_f3, rem_f3_u: ret.tag = DIV;
                            default: ret.tag = func_units_t'('x);
                        endcase
                        ret.op = {1'b0, inst_funct3(inst)};
                    end
                    default:     ret.op = 'x;
                endcase
            end
            // PASSING LINT TRUST
            op_b_load: ret = '{tag:MEM, op:{1'b0, inst_funct3(inst)}};
            op_b_store: ret = '{tag:MEM, op:{1'b1, inst_funct3(inst)}};

            op_b_jal: ret = '{tag:BRU, op:{1'b0, inst_funct3(inst)}};
            op_b_jalr: ret = '{tag:BRU, op:{1'b1, inst_funct3(inst)}};
            op_b_br: ret = '{tag:BRU, op:{1'b0, inst_funct3(inst)}};
            default: ret = '{tag:ALU, op:alu_op_add};
        endcase
        return ret;
    endfunction

endpackage

package resv_station;
import ooo_config::*;
import rv32i_types::*;

    // TODO ps2 and imm are only coresident for branches
    // need to look into a refactor later
    typedef struct packed {
        logic        valid;

        preg         ps1;
        preg         ps2;
        logic        ps1_valid;
        logic        ps2_valid;

        preg         pd;
        rreg         rd;
        rob_ent      rob_idx;

        logic [31:0] imm;
        logic        imm_optional;

        lsq_ent      lsq_idx;

        logic [BRU_BITS:0] bru_idx;

        exe_t        micro_op;
        br_mask      branch_mask;
    } resv_entry_t;

endpackage

package ld_str_queue; 
import ooo_config::*;
import rv32i_types::*;

    typedef struct packed {
        logic               valid;
        logic               addr_ready;         // set by mem when addr is computed
        logic               sq_ack_done;        // Set by Store queue when pointed store instruction finishes
        // logic               issued;             // Set by lsq when mem request for entry is finished

        logic [31:0]        address;
        logic [2:0]         ld_type;
        rob_ent             rob_index;
        rreg                rd;
        preg                pd;
        logic [SQ_BITS-1:0] sq_ptr;
        br_mask             branch_mask;
    } lq_entry_t;

    typedef struct packed {
        logic valid;
        logic [31:0] address;
        logic [2:0]  ld_type;
        rob_ent      rob_index;
        rreg         rd;
        preg         pd;
    } lq_update_t;

    typedef struct packed {
        logic           ready_to_request;
        logic [31:0]    address;
        logic [2:0]     str_type;
        rob_ent         rob_index;
        logic [31:0]    str_val;
    } sq_entry_t;
endpackage
