module mul
import ooo_config::*;
import rv32i_types::*;
(
    input logic     clk,
    input logic     rst,

    resv_func.mul resv,
    regf_func.mul regf,
    cdb.mul       cdb
);
    localparam STAGES = 2;

    typedef struct packed {
        logic valid;
        logic [31:0] opA;
        logic [31:0] opB;
        logic [ROB_BITS-1:0] rob_index;
        rreg rd;
        preg pd;
        mul_f3_t mulop;
        br_mask branch_mask;
    } mul_instr_t;

    mul_instr_t stages [STAGES];
    logic signed_mul;
    logic signed [32:0] op_a;
    logic signed [32:0] op_b;
    logic signed [63:0] f;

    logic iss_flush;
    always_comb begin
        iss_flush = 1'b0;
        if (cdb.br_resolve.valid & cdb.br_resolve.mispred) begin
            if (resv.mul_issue) begin
                if (resv.mul_resv_entry.branch_mask[cdb.br_resolve.bru_idx[BRU_BITS-1:0]]) begin
                    iss_flush = 1'b1;
                end
            end
        end
    end

    br_mask iss_br_mask;

    always_comb begin
        iss_br_mask = resv.mul_resv_entry.branch_mask;
        if (cdb.br_resolve.valid) begin
            iss_br_mask[cdb.br_resolve.bru_idx[BRU_BITS-1:0]] = 1'b0;
        end
    end

    logic flush [STAGES];
    always_comb begin
        flush = '{default:'0};
        if (cdb.br_resolve.valid & cdb.br_resolve.mispred) begin
            for (int i=1; i < STAGES; i++) begin
                if (stages[i-1].branch_mask[cdb.br_resolve.bru_idx[BRU_BITS-1:0]]) begin
                    flush[i] = 1'b1;
                end
            end
        end
    end

    always_ff @(posedge clk) begin
        if (rst) begin
            for (int i=0; i < STAGES; i++) begin
                stages[i] <= '{default:'x, mulop: mul_f3_t'('x), valid:1'b0};
            end
        end else begin
            for (int i=1; i<STAGES; i++) begin
                stages[i] <= stages[i-1];
                if (cdb.br_resolve.valid) begin 
                    if (flush[i]) begin
                        stages[i].valid <= 1'b0;
                    end else begin
                        stages[i].branch_mask[cdb.br_resolve.bru_idx[BRU_BITS-1:0]] <= 1'b0;
                    end
                end
            end
            // If the reservation station issues a command, load the values from the resv entry into the MUL latch.
            if (resv.mul_issue) begin
                stages[0].valid <= (iss_flush) ? 1'b0 : 1'b1;
                stages[0].opA <= regf.mul_ps1_v;
                stages[0].opB <= regf.mul_ps2_v;
                stages[0].rob_index <= resv.mul_resv_entry.rob_idx;
                stages[0].rd <= resv.mul_resv_entry.rd;
                stages[0].pd <= resv.mul_resv_entry.pd;
                stages[0].mulop <= resv.mul_resv_entry.micro_op.op.mul.val;
                stages[0].branch_mask <= iss_br_mask;
            end else begin
            // If issue goes down, then we need to devalidate (don't bother with the rest of the latch)
                stages[0].valid <= 1'b0;
            end
        end
    
    end

    logic signed [63:0] out [STAGES-1];

    always_comb begin
        signed_mul = (stages[0].mulop != mul_f3_hu);
        case (stages[0].mulop)
            mul_f3_hu: begin
                op_a = signed'({1'b0, stages[0].opA});
                op_b = signed'({1'b0, stages[0].opB});
            end
            mul_f3_hsu: begin
                op_a = signed'({stages[0].opA[31], stages[0].opA});
                op_b = signed'({1'b0, stages[0].opB});
            end
            default: begin
                op_a = signed'({stages[0].opA[31], stages[0].opA});
                op_b = signed'({stages[0].opB[31], stages[0].opB});
            end
        endcase

        // op_a = (stages[0].mulop == mul_f3_hu) ? {1'b0, stages[0].opA} : {stages[0].opA[31], stages[0].opA};
        // op_b = ((stages[0].mulop == mul_f3_hu) || (stages[0].mulop == mul_f3_hsu)) ? 
        //             {1'b0, stages[0].opB} : {stages[0].opB[31], stages[0].opB};
        // f = (signed_mul) ? signed'(op_a * op_b) : unsigned'(op_a * op_b);
        f = 64'(op_a * op_b);
    end

    always_ff @(posedge clk) begin
        out[0] <= f;
        for (int i=1; i <STAGES-1; i++) begin
            out[i] <= out[i-1];
        end
    end

    // DW_mult_pipe #(
    //     .a_width(33), 
    //     .b_width(33), 
    //     .num_stages(STAGES), 
    //     .stall_mode(0), 
    //     .rst_mode(0), 
    //     .op_iso_mode(0)
    // ) mul_ip (
    //     .clk(clk),
    //     .rst_n(1'b1),
    //     .en(1'b1),
    //     .tc(signed_mul),
    //     .a(op_a),
    //     .b(op_b),
    //     .product(f) 
    // );
    logic [63:0] uf;
    assign uf = $unsigned(out[STAGES-2]);

    assign cdb.mul_c.data        = (stages[STAGES-1].mulop == mul_f3) ? uf[31:0] : uf[63:32];

    assign cdb.mul_c.rd          = stages[STAGES-1].rd;
    assign cdb.mul_c.pd          = stages[STAGES-1].pd;
    assign cdb.mul_c.valid       = stages[STAGES-1].valid;
    assign cdb.mul_c.rob_index   = stages[STAGES-1].rob_index;

endmodule
