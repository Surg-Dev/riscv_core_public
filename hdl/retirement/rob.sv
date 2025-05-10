module rob
import ooo_config::*;
(
    input logic clk,
    input logic rst,

    // Dispatch Interface
    disp_rob.rob disp_itf,

    // CDB Interface
    cdb.rob cdb_itf,
    bru_rob.rob  bru_itf,
    // LSQ interface
    rob_lsq.rob lsq_itf,

    // Writeback interface
    rob_rrat.rob  rrat
);

logic can_commit;

localparam PTR_BITS = ROB_BITS;
logic enqueue;
logic dequeue;

logic flush;
assign flush = cdb_itf.br_resolve.valid & cdb_itf.br_resolve.mispred;

assign enqueue = disp_itf.en & (!flush);

logic full;
logic empty;

logic [PTR_BITS:0] head;
logic [PTR_BITS:0] tail;




typedef struct packed {
    logic  ready_to_commit;
    // logic  mispredict; // TODO[ebr] remove
    logic is_resolved_br;
    preg   pdest;
    rreg   rdest;
    logic [31:0] pc;
    rvfi_t rvfi;
    rvfi_mem_t rvfi_mem;
} rob_entry_t;

rob_entry_t data [0:ROB_DEPTH-1];


//RVFI
logic [63:0] order; 
rvfi_t rvfi_commit;
rvfi_mem_t rvfi_mem_commit;

// logic    flush_rob; // next cycle flush for rob



logic [PTR_BITS:0] flush_tail;
assign flush_tail = cdb_itf.br_resolve.rob_idx + 1'b1;

always_ff @(posedge clk) begin
    if (rst) begin
        head <= '0;
        tail <= '0;

        for (int i = 0; i < ROB_DEPTH; i++) begin
            data[i].ready_to_commit <= 1'b0;
        end
    end else begin
        // If there is a branch resolve, we need to reset tail
        // Otherwise we accept new instructions from dispatch as normal
        if (enqueue & dequeue & (full | empty)) begin
            data[tail[PTR_BITS-1:0]] <= '{1'b0, 1'b0, disp_itf.pd, disp_itf.rd, disp_itf.pc, disp_itf.rvfi, '0};
            tail <= tail + 1'b1;
            if (full) begin
                head <= head + 1'b1;
            end
        end else begin
            // Write
            if (enqueue & !full) begin
                // Allocate a new entry
                data[tail[PTR_BITS-1:0]] <= '{1'b0, 1'b0, disp_itf.pd, disp_itf.rd,  disp_itf.pc, disp_itf.rvfi, '0};
                tail <= tail + 1'b1;
            end
            // Read
            if (dequeue & !empty) begin
                data[head[PTR_BITS-1:0]].ready_to_commit <= 1'b0;  // Invalidate after dequeue, could improve by checking can_commit only if not empty instead
                head <= head + 1'b1;
            end
        end
        

        // Update ROB entry if any instruction finished
        for (int i = 0; i < FUNC_UNITS; i++) begin
            if (cdb_itf.valid[i])  data[rob_cast'(cdb_itf.rob_index[i])].ready_to_commit <= 1'b1;
        end

        if (cdb_itf.valid[BRU]) begin
            data[rob_cast'(cdb_itf.rob_index[BRU])].is_resolved_br <= 1'b1;
        end

        if (flush) begin
            data[rob_cast'(cdb_itf.br_resolve.rob_idx)].rvfi.pc_wdata <= cdb_itf.br_resolve.target;
        end

        if (flush) begin
            tail <= flush_tail;
            // data[flush_tail[PTR_BITS-1:0]].ready_to_commit <= 1'b0;
        end

        // // TODO[ebr] remove special case
        // if (cdb_itf.mispredict.valid) begin
        //     data[cdb_itf.rob_index[BRU]].rvfi.pc_wdata <= cdb_itf.mispredict.target;
        //     data[cdb_itf.rob_index[BRU]].mispredict <= '1;
        // end

        if (lsq_itf.rvfi_ready) begin
            data[rob_cast'(lsq_itf.rvfi_rob_idx)].rvfi_mem <= lsq_itf.rvfi;
        end
    end
end

// return allocated index to dispatch (tail value in current cycle)
assign disp_itf.idx = tail;

// inform disp stage if queue is full
assign disp_itf.full = full;


// Logic
always_comb begin
    full  = (head[PTR_BITS] ^ tail[PTR_BITS]) & (head[PTR_BITS-1:0] == tail[PTR_BITS-1:0]);
    empty = (head == tail);
end

// We can commit 1 instruction if the rob entry at head is ready to commit
// and non empty... because we need to be careful due to partial resets.
assign can_commit = data[head[PTR_BITS-1:0]].ready_to_commit & !empty;
assign dequeue = can_commit;
assign disp_itf.br_commit = can_commit & data[head[PTR_BITS-1:0]].is_resolved_br;

// used in bru to calculate target
assign bru_itf.pc = data[rob_cast'(bru_itf.idx)].pc;

// signals to rrat

assign rrat.regf_we = can_commit;
assign rrat.pd      = data[head[PTR_BITS-1:0]].pdest;
assign rrat.rd      = data[head[PTR_BITS-1:0]].rdest;

// For LSQ str commit
assign lsq_itf.head = head[ROB_BITS-1:0];

// RVFI Logic

always_ff @(posedge clk) begin
    if (rst) begin 
        order <= '0;
    end else if (can_commit) begin
        order <= order + 'd1;
    end
end

assign rvfi_commit = data[head[PTR_BITS-1:0]].rvfi;

always_comb begin
    if (lsq_itf.rvfi_ready && rob_cast'(lsq_itf.rvfi_rob_idx) == head[PTR_BITS-1:0]) begin
        rvfi_mem_commit = lsq_itf.rvfi;
    end else begin
        rvfi_mem_commit = data[head[PTR_BITS-1:0]].rvfi_mem;
    end
end

// RVFI Signals
logic           monitor_valid;
logic   [63:0]  monitor_order;
logic   [31:0]  monitor_inst;
logic   [4:0]   monitor_rs1_addr;
logic   [4:0]   monitor_rs2_addr;
// logic   [31:0]  monitor_rs1_rdata; dut.core.regf.rf.data[dut.core.rob.rvfi_commit.ps1]
// logic   [31:0]  monitor_rs2_rdata; dut.core.regf.rf.data[dut.core.rob.rvfi_commit.ps2]
logic   [4:0]   monitor_rd_addr;
// logic   [31:0]  monitor_rd_wdata;  dut.core.regf.rf.data[dut.core.rob.rvfi_commit.pd]
logic   [31:0]  monitor_pc_rdata;
logic   [31:0]  monitor_pc_wdata;
logic   [31:0]  monitor_mem_addr;
logic   [3:0]   monitor_mem_rmask;
logic   [3:0]   monitor_mem_wmask;
logic   [31:0]  monitor_mem_rdata;
logic   [31:0]  monitor_mem_wdata;

assign monitor_valid     = can_commit;
assign monitor_order     = order;
assign monitor_inst      = rvfi_commit.inst;
assign monitor_rs1_addr  = rvfi_commit.rs1;
assign monitor_rs2_addr  = rvfi_commit.rs2;

assign monitor_rd_addr   = rvfi_commit.rd;

assign monitor_pc_rdata  = rvfi_commit.pc_rdata;
assign monitor_pc_wdata  = rvfi_commit.pc_wdata;

assign monitor_mem_addr  = rvfi_mem_commit.mem_addr ;
assign monitor_mem_rmask = rvfi_mem_commit.mem_rmask;
assign monitor_mem_wmask = rvfi_mem_commit.mem_wmask;
assign monitor_mem_rdata = rvfi_mem_commit.mem_rdata;
assign monitor_mem_wdata = rvfi_mem_commit.mem_wdata;

endmodule
