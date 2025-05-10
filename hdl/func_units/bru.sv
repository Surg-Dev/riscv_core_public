module bru
import ooo_config::*;
import rv32i_types::*;
import branch_types::*;
(
    input clk,
    input rst,

    resv_func.bru  resv,
    regf_func.bru  regf,
    cdb.bru        cdb,
    // TODO[ebr]: flush here instead of ROB
    bru_rob.bru    rob
);

    typedef enum logic [1:0] { BRANCH, JAL, JALR } branch_t;
    typedef struct packed {
        logic valid;

        branch_t variant;
        branch_f3_t   bruop;
        logic [31:0] a;
        logic [31:0] b;
        logic [31:0] imm;
        logic [31:0]  pc;

        // bookkeeping
        logic [ROB_BITS:0] rob_index;
        logic [LSQ_BITS:0] lsq_index;
        logic [BRU_BITS:0] bru_index;
        rreg rd;
        preg pd;
        // logic         predTaken;
        br_mask branch_mask;
    } bru_instr_t;

    bru_instr_t bru_instr;

    logic flush;
    always_comb begin
        flush = 1'b0;
        if (cdb.br_resolve.valid & cdb.br_resolve.mispred) begin
            if (resv.bru_issue) begin
                if (resv.bru_resv_entry.branch_mask[cdb.br_resolve.bru_idx[BRU_BITS-1:0]]) begin
                    flush = 1'b1;
                end
            end
        end
    end

    assign rob.idx = resv.bru_resv_entry.rob_idx; // for getting PC
    always_ff @(posedge clk) begin
        if (rst) begin
            bru_instr <= '0;
        end else if (resv.bru_issue) begin
            bru_instr.valid <= (flush) ? 1'b0 : 1'b1;

            if (!resv.bru_resv_entry.imm_optional)
                bru_instr.variant <= BRANCH;
            else if (resv.bru_resv_entry.imm_optional && resv.bru_resv_entry.micro_op.op.bru.isJalr)
                bru_instr.variant <= JALR;
            else if (resv.bru_resv_entry.imm_optional)
                bru_instr.variant <= JAL;
            else
                bru_instr.variant <= branch_t'('x); // fail loudly

            bru_instr.bruop <= resv.bru_resv_entry.micro_op.op.bru.bf3;
            bru_instr.a <= regf.bru_ps1_v;
            bru_instr.b <= regf.bru_ps2_v;
            bru_instr.imm <= resv.bru_resv_entry.imm;
            bru_instr.pc <= rob.pc;

            bru_instr.rob_index <= resv.bru_resv_entry.rob_idx;
            bru_instr.rd <= resv.bru_resv_entry.rd;
            bru_instr.pd <= resv.bru_resv_entry.pd;
            
            bru_instr.branch_mask <= resv.bru_resv_entry.branch_mask;
            bru_instr.lsq_index <= resv.bru_resv_entry.lsq_idx;
            bru_instr.bru_index <= resv.bru_resv_entry.bru_idx;
        end else begin
            bru_instr.valid <= 1'b0;
        end
    end

    logic [31:0] rd_v;
    logic        br_en;
    always_comb begin
        cdb.br_resolve = '{default:'x, valid:'0, mispred:'0};
        rd_v = 'x;

        if (bru_instr.valid) begin
            unique case (bru_instr.variant)
                BRANCH: begin
                    unique case (bru_instr.bruop)
                        branch_f3_beq : br_en = (unsigned'(bru_instr.a) == unsigned'(bru_instr.b));
                        branch_f3_bne : br_en = (unsigned'(bru_instr.a) != unsigned'(bru_instr.b));
                        branch_f3_blt : br_en = (signed'(bru_instr.a) <  signed'(bru_instr.b));
                        branch_f3_bge : br_en = (signed'(bru_instr.a) >=  signed'(bru_instr.b));
                        branch_f3_bltu: br_en = (unsigned'(bru_instr.a) <  unsigned'(bru_instr.b));
                        branch_f3_bgeu: br_en = (unsigned'(bru_instr.a) >=  unsigned'(bru_instr.b));
                        default       : br_en = 'x;
                    endcase
                    // TODO: [BATAGE] Feed prediction info w/ branch
                    cdb.br_resolve  = '{valid: '1,
                                        mispred: br_en, 
                                        target: bru_instr.pc + bru_instr.imm, 
                                        rob_idx: bru_instr.rob_index,
                                        lsq_idx: bru_instr.lsq_index,
                                        branch_mask: bru_instr.branch_mask,
                                        bru_idx: bru_instr.bru_index};
                end
                JAL: begin
                    rd_v = bru_instr.pc + 'd4;

                    cdb.br_resolve  = '{valid: '1,
                                        mispred: 1'b1, 
                                        target: bru_instr.pc + bru_instr.imm, 
                                        rob_idx: bru_instr.rob_index,
                                        lsq_idx: bru_instr.lsq_index,
                                        branch_mask: bru_instr.branch_mask,
                                        bru_idx: bru_instr.bru_index};
                end
                JALR: begin
                    rd_v = bru_instr.pc + 'd4;

                    cdb.br_resolve  = '{valid: '1,
                                        mispred: 1'b1, 
                                        target: (bru_instr.a + bru_instr.imm) & 32'hfffffffe, 
                                        rob_idx: bru_instr.rob_index,
                                        lsq_idx: bru_instr.lsq_index,
                                        branch_mask: bru_instr.branch_mask,
                                        bru_idx: bru_instr.bru_index};
                end
                default: ;
            endcase
        end
    end

    assign cdb.bru_c.data = rd_v;
    assign cdb.bru_c.rd = bru_instr.rd;
    assign cdb.bru_c.pd = bru_instr.pd;
    assign cdb.bru_c.valid = bru_instr.valid;
    assign cdb.bru_c.rob_index = bru_instr.rob_index;

endmodule
