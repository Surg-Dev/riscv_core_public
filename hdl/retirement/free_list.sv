module free_list
import ooo_config::*;
(
    input clk,
    input rst,

    rrat_flist.flist rrat,
    flist_disp.flist disp_itf
);

    // Gaaah, I want to use the same names as queue.sv bite me.
    localparam TDEPTH = ROB_DEPTH;
    localparam WIDTH  = PHYS_BITS;
    localparam PTR    = ROB_BITS;

    logic [WIDTH-1 : 0] data [0:TDEPTH-1];
    logic [PTR:0] head;
    logic [PTR:0] tail;

    logic full, empty;
    logic enqueue, dequeue;

    logic [WIDTH-1:0] wdata, rdata;

    assign dequeue = disp_itf.dequeue;
    assign enqueue = rrat.enqueue;

    assign disp_itf.phys_reg   = rdata;
    assign wdata   = rrat.phys_reg;

    assign disp_itf.cur_head = head;

    // Copied from queue.sv - custom reset behavior
    always_ff @(posedge clk) begin
        // Reset
        if (rst) begin
            for (int unsigned i=0; i < TDEPTH; ++i) begin
                // First 32 regs are assigned, thus regs p32-p(PHYS_REGS) are added automatically.
                data[i] <= WIDTH'(i) + 6'b100000;
            end
            head <= '0;
            tail <= TDEPTH[PTR:0];
        end else begin
            if (enqueue & dequeue & (full | empty)) begin
                if (empty) begin
                    data[tail[PTR-1:0]] <= wdata;
                    tail <= tail + 1'b1;
                end else if (full) begin
                    data[tail[PTR-1:0]] <= wdata;
                    head <= head + 1'b1;
                    tail <= tail + 1'b1;
                end
            end else begin
                // Write
                if (enqueue & !full) begin
                    data[tail[PTR-1:0]] <= wdata;
                    tail <= tail + 1'b1;
                end
                // Read
                if (dequeue & !empty) begin
                    head <= head + 1'b1;
                end
            end
            // Soft reset head of flist
            if (disp_itf.flush) begin
                head <= disp_itf.ckpt_head;       
            end 
        end
    end

    // Logic
    always_comb begin
        full  = (head[PTR] ^ tail[PTR]) & (head[PTR-1:0] == tail[PTR-1:0]);
        empty = (head == tail);
        rdata = data[head[PTR-1:0]];

        disp_itf.empty = empty;
        disp_itf.full  = full;
    end



endmodule
