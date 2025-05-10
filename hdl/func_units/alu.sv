module alu
import ooo_config::*;
import rv32i_types::*;
(
    input clk,
    input rst,

    resv_func.alu resv,
    regf_func.alu regf,
    cdb.alu       cdb
);

    typedef struct packed {
        logic valid;
        logic [31:0] opA;
        logic [31:0] opB;
        logic [ROB_BITS-1:0] rob_index;
        rreg rd;
        preg pd;
        alu_ops aluop;
        br_mask branch_mask;
    } alu_instr_t;

    alu_instr_t alu_instr;

    logic flush;
    logic alt_flush;
    always_comb begin
        flush = 1'b0;
        alt_flush = 1'b0;
        // If CDB raises a valid mispred, and alu issues, and the branch mask matches, "flush"
        if (cdb.br_resolve.valid & cdb.br_resolve.mispred) begin
            if (resv.alu_issue) begin
                if (resv.alu_resv_entry.branch_mask[cdb.br_resolve.bru_idx[BRU_BITS-1:0]]) begin
                    flush = 1'b1;
                end
                if (alu_instr.branch_mask[cdb.br_resolve.bru_idx[BRU_BITS-1:0]]) begin
                    alt_flush = 1'b1;
                end
            end
        end
    end

    always_ff @(posedge clk) begin
        if (rst) begin
            alu_instr <= '0;
        end else begin
            // If the reservation station issues a command, load the values from the resv entry into the ALU latch.
            if (resv.alu_issue) begin
                alu_instr.valid <= (flush) ? 1'b0 : 1'b1;
                alu_instr.opA <= regf.alu_ps1_v;
                // If the reservation station imm is valid, take it over the regfile.
                alu_instr.opB <= (resv.alu_resv_entry.imm_optional) ? resv.alu_resv_entry.imm : regf.alu_ps2_v;

                alu_instr.rob_index <= resv.alu_resv_entry.rob_idx;
                alu_instr.rd <= resv.alu_resv_entry.rd;
                alu_instr.pd <= resv.alu_resv_entry.pd;
                alu_instr.aluop <= resv.alu_resv_entry.micro_op.op.alu.val;
                alu_instr.branch_mask <= resv.alu_resv_entry.branch_mask;
            end else begin
            // If issue goes down, then we need to devalidate (don't bother with the rest of the latch)
                alu_instr.valid <= 1'b0;
            end
        end
    end

    alu_backend unit(
        .aluop(alu_instr.aluop),
        .a(alu_instr.opA),
        .b(alu_instr.opB),
        .f(cdb.alu_c.data)
    );

    assign cdb.alu_c.rd = alu_instr.rd;
    assign cdb.alu_c.pd = alu_instr.pd;
    assign cdb.alu_c.valid = alu_instr.valid;
    assign cdb.alu_c.rob_index = alu_instr.rob_index;

endmodule

module alu_backend
import rv32i_types::*;
(
    input   logic   [3:0]   aluop,
    input   logic   [31:0]  a, b,
    output  logic   [31:0]  f
);

    logic signed   [31:0] as;
    logic signed   [31:0] bs;
    logic unsigned [31:0] au;
    logic unsigned [31:0] bu;

    assign as =   signed'(a);
    assign bs =   signed'(b);
    assign au = unsigned'(a);
    assign bu = unsigned'(b);

    always_comb begin
        unique case (aluop)
            alu_op_add:  f = au +   bu;
            alu_op_sll:  f = au <<  bu[4:0];
            alu_op_sra:  f = unsigned'(as >>> bu[4:0]);
            alu_op_sub:  f = au -   bu;
            alu_op_xor:  f = au ^   bu;
            alu_op_srl:  f = au >>  bu[4:0];
            alu_op_or:   f = au |   bu;
            alu_op_and:  f = au &   bu;
            alu_op_slt:  f = as < bs ? 32'b1 : 32'b0;
            alu_op_sltu: f = au < bu ? 32'b1 : 32'b0;
            default: f = 'x;
        endcase
    end

endmodule : alu_backend
