module fetch
import rv32i_types::*;
#(
    parameter RESET_ADDR = 32'h1eceb000
)(
    input   logic           clk,
    input   logic           rst,

    input   logic   [31:0]  ic_rdata,
    output  logic   [31:0]  ic_addr,
    output  logic   [3:0]   ic_rmask,
    input   logic           ic_resp,

    fetch_disp.fetch        disp_itf,
    cdb.fetch               cdb
);

logic flush;
assign flush = cdb.br_resolve.valid & cdb.br_resolve.mispred;
// Indicates that there was an outstanding request to cache.
logic outstanding_req;
logic flush_req;
always_ff @(posedge clk) begin
    if (rst) begin
        outstanding_req <= 1'b0;
        flush_req <= 1'b0;
    end else if (flush) begin
        outstanding_req <= 1'b0;
        flush_req <= 1'b1;
    end
    else begin
        if (flush_req) begin
            if (ic_resp) begin
                flush_req <= 1'b0;
                outstanding_req <= 1'b1;
            end
        end else begin 
            if((|ic_rmask))
                outstanding_req <= 1'b1;
            else if (outstanding_req && ic_resp)
                outstanding_req <= 1'b0;
        end
    end
end

logic [31:0]  pc;
logic [31:0]  pc_next;
logic [31:0]  pc_req;

assign pc_next = pc + 'd4;

logic enqueue;
logic full;

logic req_stall;
assign req_stall = (outstanding_req && !ic_resp) || flush_req;

always_ff @(posedge clk) begin
    if(rst) begin
        pc <= RESET_ADDR;
        pc_req <= RESET_ADDR;
    end else if (flush) begin
        pc_req <= cdb.br_resolve.target;
        pc <= cdb.br_resolve.target;
    end else if (!full && !req_stall) begin
        pc_req <= pc;
        pc <= pc_next;
    end else if (flush_req && ic_resp) begin
        pc <= pc_next;
    end
end

assign ic_addr = (full) ? pc_req : pc;
assign ic_rmask = 4'b1111;

assign enqueue = !full && ic_resp && !flush_req;

logic [95:0] q_data;

queue #(3, 96) fetch_buffer (
    .clk(clk),
    .rst(rst | flush),

    .enqueue(enqueue),
    .wdata({pc, pc_req, ic_rdata}),
    .full(full),

    .dequeue(disp_itf.dequeue),
    .rdata(q_data),
    .empty(disp_itf.empty)
);

//RVFI

assign disp_itf.rdata   = q_data[31:0];
assign disp_itf.pc      = q_data[63:32];
assign disp_itf.pc_next = q_data[95:64];

endmodule
