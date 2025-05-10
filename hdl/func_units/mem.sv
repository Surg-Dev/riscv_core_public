module mem
import ooo_config::*;
import rv32i_types::*;
(
    input logic     clk,
    input logic     rst,

    resv_func.mem resv,
    regf_func.mem regf,
    cdb.mem       cdb,
    mem_lsq.mem   lsq
);

    typedef struct packed {
        logic valid;
        logic [31:0] opA;
        logic [31:0] opB;
        logic [31:0] str_val; // For store
        logic is_str;
        rob_ent rob_index;
        lsq_ent lsq_index;
        rreg rd; // For load
        preg pd; // For load
        load_store_f3_t ldstrop;
        br_mask branch_mask;
    } ld_str_instr_t;

    ld_str_instr_t ld_str_instr;

    logic flush;
    always_comb begin
        flush = 1'b0;
        if (cdb.br_resolve.valid & cdb.br_resolve.mispred) begin
            if (resv.mem_issue) begin
                if (resv.mem_resv_entry.branch_mask[cdb.br_resolve.bru_idx[BRU_BITS-1:0]]) begin
                    flush = 1'b1;
                end
            end
        end
    end

    always_ff @(posedge clk) begin
        if (rst) begin
            ld_str_instr <= '0;
        end else begin
            // If the reservation station issues a command, load the values from the resv entry into the MEM latch.
            if (resv.mem_issue) begin
                ld_str_instr.valid <= (flush) ? 1'b0 : 1'b1;
                
                ld_str_instr.opA <= regf.mem_ps1_v;
                ld_str_instr.opB <= resv.mem_resv_entry.imm;    // For Load/Store, We'll always have opB as immediate
                ld_str_instr.str_val <= regf.mem_ps2_v;         // For store, rs2 acts as value to store

                ld_str_instr.rob_index <= resv.mem_resv_entry.rob_idx;
                ld_str_instr.lsq_index <= resv.mem_resv_entry.lsq_idx;
                ld_str_instr.rd <= resv.mem_resv_entry.rd; // For load
                ld_str_instr.pd <= resv.mem_resv_entry.pd; // For load
                ld_str_instr.ldstrop <= load_store_f3_t'(resv.mem_resv_entry.micro_op.op.mem.val);
                ld_str_instr.is_str <= resv.mem_resv_entry.micro_op.op.mem.is_str;
                ld_str_instr.branch_mask <= resv.mem_resv_entry.branch_mask;
            end else begin
            // If issue goes down, then we need to devalidate (don't bother with the rest of the latch)
                ld_str_instr.valid <= 1'b0;
            end
        end
    
    end

    // Compute address
    logic [31:0] mem_addr;
    assign mem_addr = ld_str_instr.opA + ld_str_instr.opB;

    ////////////////////////////////////////////////////////////////////////////
    // Load Store Queue
    assign lsq.valid        = ld_str_instr.valid;
    assign lsq.addr         = mem_addr;
    assign lsq.ld_str_type  = ld_str_instr.ldstrop;
    assign lsq.lsq_idx      = ld_str_instr.lsq_index;
    assign lsq.rob_idx      = ld_str_instr.rob_index;     
    assign lsq.rd           = ld_str_instr.rd;
    assign lsq.pd           = ld_str_instr.pd;
    assign lsq.str_val      = ld_str_instr.str_val;
    assign lsq.is_str       = ld_str_instr.is_str;

    ////////////////////////////////////////////////////////////////////////////
    // DCache Request Handler
    assign cdb.mem_c.valid      = lsq.deq_done;
    assign cdb.mem_c.data       = lsq.deq_ld_data;   
    assign cdb.mem_c.rd         = lsq.deq_rd;          
    assign cdb.mem_c.pd         = lsq.deq_pd;          
    assign cdb.mem_c.rob_index  = lsq.deq_rob_idx;

endmodule
