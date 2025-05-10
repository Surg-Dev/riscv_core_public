module resv
import ooo_config::*;
import rv32i_types::*;
import resv_station::*;
(
    input  logic           clk,
    input  logic           rst,

    disp_resv.resv         disp,
    resv_func.resv         func,
    resv_regf.resv         regf,
    cdb.resv                cdb
);

    localparam int RESV_ENTRIES = 8;
    typedef logic [$clog2(RESV_ENTRIES)-1:0] int_rs;
    typedef logic [$clog2(FUNC_UNITS)-1:0] int_fn;


    resv_entry_t stations[RESV_ENTRIES];
    resv_entry_t entry_next; 

    // A phys reg becomes busy when in the dispatch
    // CDB will mark ready once it executes
    logic [PHYS_REGS-1:0] busy_list;

    struct {
        logic   en;
        int_rs  idx;
    } issue [FUNC_UNITS];


    logic full;
    int station_to_enqueue;

    logic flush;
    assign flush = cdb.br_resolve.valid & cdb.br_resolve.mispred;
    logic br_pred;
    assign br_pred = cdb.br_resolve.valid & !cdb.br_resolve.mispred;

    // Determine Full bit.
    always_comb begin
        full = '1;
        // OH IM FUCKING SYNOPSYS I CANT DEAL WITH THE "WITH" OPERATOR
        // HUR DEE DUR "stations.and() with (item.valid)" is too *ADVANCED* for me
        for (int i = 0; i < RESV_ENTRIES; i++) begin
            full &= stations[i].valid;
        end
    end
    assign disp.full = full;

    // Prio-Mux to select a slot to insert a new instruction.
    always_comb begin
        station_to_enqueue = '0;
        for (int i = 0; i < RESV_ENTRIES; i++) begin
            if (!stations[i].valid) begin
                station_to_enqueue = i;
                break;
            end
        end
    end

    always_comb begin
        entry_next = disp.entry;

        // Get whether the ops are busy
        entry_next.ps1_valid = !busy_list[entry_next.ps1];
        entry_next.ps2_valid = !busy_list[entry_next.ps2];

        // Forward CDB wakeup since disp.entry is latched.
        for (int j = 0; j < FUNC_UNITS; j++) begin
            if (cdb.valid[j]) begin
                if (entry_next.ps1 == cdb.pd[j]) begin
                    entry_next.ps1_valid = 1'b1;
                end
                if (entry_next.ps2 == cdb.pd[j]) begin
                    entry_next.ps2_valid = 1'b1;
                end
            end
        end

        // // TODO: Deterimine if needed...
        if (flush) begin
            if ((entry_next.branch_mask[cdb.br_resolve.bru_idx[BRU_BITS-1:0]])) begin
                entry_next.valid = 1'b0;
            end                    
        end

        if (br_pred) begin
            entry_next.branch_mask[cdb.br_resolve.bru_idx[BRU_BITS-1:0]] = 1'b0;
        end
    end

    // Stateful Logic for entry load/update/devalidate
    always_ff @(posedge clk) begin
        if (rst) begin
            // Reset Behavior: Invalidate all entries. 
            for (int i = 0; i < RESV_ENTRIES; i++) begin
                stations[i] <= '{default:'x, valid:'0, micro_op:exe_t'('x)};
            end

            // No regs are busy
            busy_list <= '0;
        end else begin

            if (flush) begin
                for (int i = 0; i < RESV_ENTRIES; i++) begin 
                    // If this instruction is dep on this branch, devalidate.
                    if ((stations[i].branch_mask[cdb.br_resolve.bru_idx[BRU_BITS-1:0]])) begin
                        stations[i].valid <= 1'b0;
                    end
                end
            end

            if (br_pred) begin
                for (int i=0; i < RESV_ENTRIES; i++) begin
                    stations[i].branch_mask[cdb.br_resolve.bru_idx[BRU_BITS-1:0]] <= 1'b0;
                end
            end

            // Look at the CDB for any physical regs that got updated.
            for (int i = 0; i < RESV_ENTRIES; i++) begin
                for (int j = 0; j < FUNC_UNITS; j++) begin
                    if (cdb.valid[j]) begin
                        if (stations[i].ps1 == cdb.pd[j]) begin
                            stations[i].ps1_valid <= 1'b1;
                        end
                        if (stations[i].ps2 == cdb.pd[j]) begin
                            stations[i].ps2_valid <= 1'b1;
                        end
                    end
                end
            end

            for (int i =0; i < FUNC_UNITS; i++) begin
                if (cdb.valid[i]) begin
                    busy_list[cdb.pd[i]] <= 1'b0;
                end
            end

            // Once dispatch issues an instruction, mark it as busy
            // TODO: I'd rather not make an interface from the free list or more info from dispatch
            //       but if we find that this is not in sync with flushing, we might have to.
            if (!full & entry_next.valid & !flush) begin
                stations[station_to_enqueue] <= entry_next;
                busy_list[entry_next.pd] <= (entry_next.pd != '0) ? 1'b1 : 1'b0;
            end


            // Devalidate the entry which we are issuing (if any).
            for (int i = 0; i < FUNC_UNITS; i++) begin
                if (issue[i].en) begin
                    stations[issue[i].idx].valid <= 1'b0;
                end
            end

        end
    end

    generate for (genvar FUNC = 0; FUNC < FUNC_UNITS; FUNC++) begin
        always_comb begin
            issue[FUNC] = '{en:'0, idx:'x};
            for (int unsigned i = 0; i < RESV_ENTRIES; i++) begin
                if (stations[i].valid) begin
                    if (stations[i].micro_op.tag == func_units_t'(FUNC)) begin
                        if (stations[i].ps1_valid && (stations[i].ps2_valid  || stations[i].imm_optional)) begin
                            issue[FUNC].en = 1'b1;
                            issue[FUNC].idx = $clog2(RESV_ENTRIES)'(i);
                            break;
                        end
                    end
                end
            end
        end
    end endgenerate

    // Assign outgoing functional units.
    always_comb begin
        for (int i = 0; i < FUNC_UNITS; i++) begin
            regf.ps1[i]        = stations[issue[i].idx].ps1;
            regf.ps2[i]        = stations[issue[i].idx].ps2;

            func.issue[i]      = issue[i].en ? '1 : '0;
            func.resv_entry[i] = stations[issue[i].idx];
        end
    end

endmodule
