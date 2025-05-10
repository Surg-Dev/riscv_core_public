module div
import ooo_config::*;
import rv32i_types::*;
(
    input clk,
    input rst,

    resv_func.div resv,
    regf_func.div regf,
    cdb.div       cdb
);

    localparam STAGES = 7;

    typedef struct packed {
        logic valid;
        logic [31:0] opA;
        logic [31:0] opB;
        logic [ROB_BITS-1:0] rob_index;
        rreg rd;
        preg pd;
        div_f3_t divop;
        br_mask branch_mask;
    } div_instr_t;

    // Divider Wires
    logic [32:0] op_a; 
    logic [32:0] op_b;
    logic [32:0] f;
    logic [32:0] rem;
    logic div0;

    div_instr_t stages [STAGES];

    logic iss_flush;
    always_comb begin
        iss_flush = 1'b0;
        if (cdb.br_resolve.valid & cdb.br_resolve.mispred) begin
            if (resv.div_issue) begin
                if (resv.div_resv_entry.branch_mask[cdb.br_resolve.bru_idx[BRU_BITS-1:0]]) begin
                    iss_flush = 1'b1;
                end
            end
        end
    end

    br_mask iss_br_mask;

    always_comb begin
        iss_br_mask = resv.div_resv_entry.branch_mask;
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
                stages[i] <= '{default:'x,divop: div_f3_t'('x), valid:1'b0}; 
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

            if (resv.div_issue) begin
                stages[0].valid <= (iss_flush) ? 1'b0 : 1'b1;
                stages[0].opA <= regf.div_ps1_v;
                stages[0].opB <= regf.div_ps2_v;
                stages[0].rob_index <= resv.div_resv_entry.rob_idx;
                stages[0].rd <= resv.div_resv_entry.rd;
                stages[0].pd <= resv.div_resv_entry.pd;
                stages[0].divop <= resv.div_resv_entry.micro_op.op.div.val;
                stages[0].branch_mask <= iss_br_mask;
            end else begin
                stages[0].valid <= 1'b0;
            end
        end
    end

    always_comb begin
        op_a = 'x;
        op_b = 'x;
        case (stages[0].divop)
            div_f3, rem_f3: begin 
                op_a = {stages[0].opA[31], stages[0].opA};
                op_b = {stages[0].opB[31], stages[0].opB};
            end
            div_f3_u, rem_f3_u: begin
                op_a = {1'b0, stages[0].opA};
                op_b = {1'b0, stages[0].opB};
            end
            default: begin end
        endcase
    end

    // a width, b width, (1=signed 0=unsigned), (1=rem, 0=mod), stages, stallable, rstmode
    DW_div_pipe #(
        .a_width(33), 
        .b_width(33), 
        .tc_mode(1),            // 0: Unsigned, 1: Signed
        .rem_mode(1),           // 0: Modulus,  1: Remainder
        .num_stages(STAGES), 
        .stall_mode(0),         // 0: No-Stall, 1: Stallable
        .rst_mode(0)            // 0: No Reset, 1: Async,     2: Sync
    ) div_module (      
        .clk(clk),
        .rst_n(1'b1), //Active-Low
        .en(1'b1),    //Active-High
        .a(op_a),
        .b(op_b),
        .quotient(f),
        .remainder(rem),
        .divide_by_0(div0)
    );

    // CDB Assignments on output stage.
    assign cdb.div_c.rd = stages[STAGES-1].rd;
    assign cdb.div_c.pd = stages[STAGES-1].pd;
    assign cdb.div_c.valid = stages[STAGES-1].valid;
    assign cdb.div_c.rob_index = stages[STAGES-1].rob_index;
    
    // TODO: Test that overflow + div zero is correct
    always_comb begin
        case (stages[STAGES-1].divop)
            div_f3: cdb.div_c.data = f[31:0];
            div_f3_u: cdb.div_c.data = f[31:0];
            rem_f3:   cdb.div_c.data = rem[31:0];
            rem_f3_u: cdb.div_c.data = rem[31:0];
            default: cdb.div_c.data = 'x;
        endcase
    end

                                    
    

    
endmodule
