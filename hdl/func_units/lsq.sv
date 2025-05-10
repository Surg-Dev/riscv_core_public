module lsq_split
import rv32i_types::*;
import ooo_config::*;
import ld_str_queue::*;
(
    input logic clk,
    input logic rst,

    // Dispatch Interface
    disp_lsq.lsq disp,

    // ROB interface
    rob_lsq.lsq rob,

    // Mem unit interface
    mem_lsq.lsq mem,

    // CDB interface
    cdb.lsq cdb,
    
    // Dmem port
    output  logic   [31:0]  dc_addr,
    output  logic   [3:0]   dc_rmask,
    output  logic   [3:0]   dc_wmask,
    input   logic   [31:0]  dc_rdata,
    output  logic   [31:0]  dc_wdata,
    input   logic           dc_resp    
);

// TODOs:
/*
    Rewrite LQ as a resv-table
    implement EBR branch squashing on the lq
    build aribit mux against lq/stq
    hook up rvfi
*/
logic flush;
logic enqueue_en;

assign flush =  cdb.br_resolve.valid & cdb.br_resolve.mispred;
assign enqueue_en = disp.en & !flush;

// Load Queue Ports
logic lq_alloc, lq_dequeue;
logic discard_ld;
lq_entry_t issued_ld;
lq_update_t update_lq_ent;

logic sq_ack;
logic [SQ_BITS-1:0] sq_front_idx;

// Store Queue Ports
logic sq_full, sq_alloc, sq_empty;
logic sq_dequeue;
sq_entry_t sq_front, sq_update_ent;
logic [SQ_BITS:0] sq_alloc_idx;
logic [SQ_BITS-1:0] sq_last_alloc_idx;

assign lq_alloc      =  enqueue_en && !disp.is_str;
assign update_lq_ent = '{mem.valid && !mem.is_str, mem.addr, mem.ld_str_type, mem.rob_idx, mem.rd, mem.pd};


load_queue lq(
    .clk(clk),
    .rst(rst),
    
    
    .br_update(cdb.br_resolve.valid),
    .flush(flush),
    .bru_idx(cdb.br_resolve.bru_idx),
    .discard_ld(discard_ld),

    .enqueue(lq_alloc),
    .rob_idx_enq(disp.rob_entry),
    .branch_mask_enq(disp.branch_mask),

    .enq_sq_ptr(sq_last_alloc_idx),
    .enq_sq_ptr_valid(!sq_empty),

    .dequeue(lq_dequeue), // TODO: add dfp response.
    .iss_ent(issued_ld),

    .sq_ack(sq_ack),
    .sq_ack_idx(sq_front_idx),

    .update_lq_ent(update_lq_ent)
);



assign sq_alloc      = enqueue_en && disp.is_str;
assign sq_update_ent = '{mem.valid && mem.is_str, mem.addr, mem.ld_str_type, mem.rob_idx, mem.str_val};

assign disp.sq_full  = sq_full;
assign disp.idx      = sq_alloc_idx;


store_queue sq(
    .clk(clk),
    .rst(rst),

    .full(sq_full),
    .enqueue(sq_alloc),
    .enqueue_idx(sq_alloc_idx),
    
    .last_enqueue_idx(sq_last_alloc_idx),

    .front(sq_front),
    .front_idx(sq_front_idx),

    .empty(sq_empty),
    .dequeue(sq_dequeue),

    .update_idx(mem.lsq_idx),
    .update_ent(sq_update_ent),

    .flush(flush),
    .flush_tail(cdb.br_resolve.lsq_idx)
);



// DCACHE Interface
logic mem_ld_req_done, mem_st_req_done;
logic discard_dc;
// Determine Load
logic lq_can_request;
logic [31:0] lq_req_addr;
logic [2:0]  lq_req_type;

logic sq_can_request;
logic [31:0] sq_req_addr;
logic [31:0] sq_req_wdata;
logic [2:0]  sq_req_type;

// Load Request
// TODO: Discard load if on same cycle resp we get a flush!

// We can issue a load during a flush, if the issued load isnt being told to discard
assign lq_can_request = issued_ld.valid && !discard_ld; 
assign lq_req_addr    = issued_ld.address;
assign lq_req_type    = issued_ld.ld_type;
// Dequeue when done.
// assign lq_dequeue     = mem_ld_req_done;

// Store Request
assign sq_can_request = !sq_empty && sq_front.ready_to_request && (rob_cast'(rob.head) == rob_cast'(sq_front.rob_index));
assign sq_req_addr = sq_front.address;
assign sq_req_wdata =  sq_front.str_val;
assign sq_req_type  = sq_front.str_type;
assign sq_ack         = mem_st_req_done;

logic mem_req_resp;
assign mem_req_resp = dc_resp;

// ARBITER

typedef enum logic [1:0] {
    ARB_REQ_NONE = 0,
    ARB_REQ_LD = 1,
    ARB_REQ_ST = 2
} req_type_t;

req_type_t arbiter_decision;
assign arbiter_decision = sq_can_request ? ARB_REQ_ST : // Prioritize stores
                          lq_can_request ? ARB_REQ_LD :
                          ARB_REQ_NONE;
req_type_t active_req_type;

always_ff @(posedge clk) begin
    if(rst) begin
        active_req_type <= ARB_REQ_NONE;
    end
    else begin
        if(active_req_type != ARB_REQ_NONE && mem_req_resp) begin // TODO: Fix for same cycle resp.
            active_req_type <= ARB_REQ_NONE;
        end
        else begin   
            if (arbiter_decision == ARB_REQ_ST) begin    // Prioritize stores
                active_req_type <= ARB_REQ_ST;
            end
            else if(lq_can_request) begin
                active_req_type <= ARB_REQ_LD;
            end
        end
    end
end

always_ff @(posedge clk) begin
    if (rst) begin
        discard_dc <= 1'b0;
    end else begin
        if ((active_req_type == ARB_REQ_LD) && discard_ld) begin
            discard_dc <= 1'b1;
        end
        if (mem_req_resp) begin
            discard_dc <= 1'b0;
        end
    end
end

// req_type_t select_op;
// // We select the comb state if either A. We have a response going on right now
// assign select_op = (!mem_req_resp) ? active_req_type

logic [31:0]    mem_req_addr;
logic [31:0]    mem_req_wdata;
logic [2:0]     mem_req_type;

logic   [3:0]    mem_mask;
logic   [31:0]   mem_req_wdata_shifted;
logic [31:0]     mem_req_rdata;
logic            mem_req_done;

assign mem_req_addr     = (arbiter_decision == ARB_REQ_ST) ? sq_req_addr : lq_req_addr;
assign mem_req_wdata    = sq_req_wdata;
assign mem_req_type     = (arbiter_decision == ARB_REQ_ST) ? sq_req_type : lq_req_type;


always_comb begin
    mem_mask    = 4'b0;  
    unique case (mem_req_type)
        ls_f3_b, ls_f3_bu   : mem_mask = 4'b0001 << mem_req_addr[1:0]; 
        ls_f3_h, ls_f3_hu   : mem_mask = 4'b0011 << mem_req_addr[1:0];
        ls_f3_w             : mem_mask = 4'b1111;
        default             : mem_mask = 4'b0000; 
    endcase
end

always_comb begin
    mem_req_wdata_shifted = 32'b0;
    unique case (mem_req_type)
        ls_f3_b     : mem_req_wdata_shifted[8 *mem_req_addr[1:0] +: 8 ] = mem_req_wdata[7 :0];
        ls_f3_h     : mem_req_wdata_shifted[16*mem_req_addr[1]   +: 16] = mem_req_wdata[15:0];
        ls_f3_w     : mem_req_wdata_shifted = mem_req_wdata;
        default     : mem_req_wdata_shifted = 32'bx;
    endcase
end

always_comb begin
    unique case(mem_req_type)
        ls_f3_b  : mem_req_rdata = {{24{dc_rdata[7 +8 *mem_req_addr[1:0]]}}, dc_rdata[8 *mem_req_addr[1:0] +: 8 ]};
        ls_f3_bu : mem_req_rdata = {{24{1'b0}}                             , dc_rdata[8 *mem_req_addr[1:0] +: 8 ]};
        ls_f3_h  : mem_req_rdata = {{16{dc_rdata[15+16*mem_req_addr[1]  ]}}, dc_rdata[16*mem_req_addr[1]   +: 16]};
        ls_f3_hu : mem_req_rdata = {{16{1'b0}}                             , dc_rdata[16*mem_req_addr[1]   +: 16]};
        ls_f3_w  : mem_req_rdata = dc_rdata;
        default  : mem_req_rdata = 32'bx;            
    endcase
end


// TODO: idk if this actually works for next-cycle shenanigans
assign dc_addr      = {mem_req_addr[31:2], 2'b0};
assign dc_rmask     = ((active_req_type == ARB_REQ_NONE && arbiter_decision == ARB_REQ_LD) || (active_req_type == ARB_REQ_LD)) && !mem_req_resp ? mem_mask : 4'b0000;
assign dc_wmask     = ((active_req_type == ARB_REQ_NONE && arbiter_decision == ARB_REQ_ST) || (active_req_type == ARB_REQ_ST)) && !mem_req_resp ? mem_mask : 4'b0000;
assign dc_wdata     = mem_req_wdata_shifted;

assign mem_ld_req_done = (active_req_type == ARB_REQ_LD) && mem_req_resp && !discard_ld && !discard_dc; // TODO: check here for consistency...
assign mem_st_req_done = (active_req_type == ARB_REQ_ST) && mem_req_resp;
assign mem_req_done    = mem_ld_req_done || mem_st_req_done;

// assign ongoing_dc_request = (|dc_wmask) || (|dc_rmask);

// Send to CDB through mem unit
// TODO: should we send CDB resp for store when we request or when we recieve response?
assign mem.deq_done     = mem_req_done;   // Tell mem unit, we completed a request
assign mem.deq_rob_idx  = (active_req_type == ARB_REQ_ST) ? sq_front.rob_index : issued_ld.rob_index;
assign mem.deq_rd       = (active_req_type == ARB_REQ_ST) ? '0                 : issued_ld.rd;
assign mem.deq_pd       = (active_req_type == ARB_REQ_ST) ? '0                 : issued_ld.pd;
assign mem.deq_ld_data  = mem_req_rdata;

assign sq_dequeue =  mem_st_req_done;
assign lq_dequeue =  mem_ld_req_done;

// RVFI

logic [31:0] rvfi_addr ;
logic [3:0]  rvfi_rmask;
logic [3:0]  rvfi_wmask;
logic [31:0] rvfi_rdata;
logic [31:0] rvfi_wdata;
rob_ent      rvfi_rob_ent;
logic rvfi_ready;
always_ff @(posedge clk) begin
    if (rst) begin
        rvfi_rmask <= '0;
        rvfi_wmask <= '0;
        rvfi_ready <= '0;
    end else begin
        if (mem_req_done) begin
            rvfi_rdata    <= (active_req_type == ARB_REQ_LD) ? dc_rdata : '0;
            rvfi_addr     <= dc_addr;
            rvfi_rmask    <= (active_req_type == ARB_REQ_LD) ? mem_mask : 4'b0000; // FIXME: do we need this gating? we already have gated signal
            rvfi_wmask    <= (active_req_type == ARB_REQ_ST) ? mem_mask : 4'b0000;  
            rvfi_wdata    <= mem_req_wdata_shifted;
            rvfi_rob_ent  <= (active_req_type == ARB_REQ_ST) ? sq_front.rob_index: issued_ld.rob_index;
            rvfi_ready    <= 1'b1;
        end else begin
            rvfi_addr     <= '0;
            rvfi_rmask    <= '0;
            rvfi_wmask    <= '0;
            rvfi_wdata    <= '0;
            rvfi_rdata    <= '0;
            rvfi_rob_ent  <= '0;
            rvfi_ready    <= '0;
        end
    end
end

assign rob.rvfi = {rvfi_addr, rvfi_rmask, rvfi_wmask, rvfi_rdata, rvfi_wdata};
assign rob.rvfi_rob_idx = rvfi_rob_ent;
assign rob.rvfi_ready = rvfi_ready;
endmodule

module load_queue
import rv32i_types::*;
import ooo_config::*;
import ld_str_queue::*;
(
    input logic clk,
    input logic rst,

    // EBR Flushing
    input logic br_update,
    input logic flush,
    input logic [BRU_BITS:0] bru_idx,
    // If we flush the issued load, we need to discard that mem response.
    output logic discard_ld,

    // output logic full,

    // Dispatch Ports
    input  logic enqueue,
    input  rob_ent rob_idx_enq,
    input  br_mask branch_mask_enq,

    // Pull the youngest, older store than this load.
    input  logic [SQ_BITS-1:0] enq_sq_ptr,
    input  logic               enq_sq_ptr_valid,

    input  logic dequeue, //TODO: we kill the entry once we actually issue...
    output lq_entry_t iss_ent,

    // Update on store commits
    input logic sq_ack,
    input logic [SQ_BITS-1:0] sq_ack_idx,

    // input logic update_idx // TODO: We can bypass this by snooping the cdb's update rob, could be expensive.
    input  lq_update_t update_lq_ent
);

    lq_entry_t stations [LQ_DEPTH];
    int station_to_enqueue;
    int station_to_update;
    int station_to_issue;
    logic issue_en;

    // Prio Mux to enqueue a station
    always_comb begin
        station_to_enqueue = '0;
        for (int i = 0; i < LQ_DEPTH; i++) begin
            if (!stations[i].valid) begin
                station_to_enqueue = i;
                break;
            end
        end
    end

    // Select Mux to find the station to update.
    
    always_comb begin
        station_to_update = '0;
        for (int i = 0; i < LQ_DEPTH; i++) begin
            if (stations[i].valid && update_lq_ent.valid) begin 
                if (update_lq_ent.rob_index == stations[i].rob_index) begin
                    station_to_update = i;
                end
            end
        end
    end

    // Prio Mux to find a valid staiton to issue
    always_comb begin
        station_to_issue = '0;
        issue_en = 1'b0;
        if (!flush & (dequeue | !iss_ent.valid)) begin
            for (int i = 0; i < LQ_DEPTH; i++) begin
                if (stations[i].valid &&
                    stations[i].addr_ready &&
                    (stations[i].sq_ack_done || (sq_ack && (sq_ack_idx == stations[i].sq_ptr)))) begin
                        station_to_issue = i;
                        issue_en = 1'b1;
                        break;
                end
            end           
        end
    end

    always_ff @(posedge clk) begin
        if (rst) begin
            for (int i = 0; i < LQ_DEPTH; i++) begin
                stations[i] <= '{valid: 1'b0, default:'x};
            end
            iss_ent <= '{valid: 1'b0, default:'x};
        end else begin
            // Enqueue from dispatch unit. 
            if (enqueue) begin
                stations[station_to_enqueue] <= '{  valid: 1'b1,
                                                    addr_ready: 1'b0,
                                                    sq_ack_done: (!enq_sq_ptr_valid | (sq_ack && (sq_ack_idx == enq_sq_ptr))),
                                                    address: 'x,
                                                    ld_type: 'x,
                                                    rob_index: rob_idx_enq,
                                                    rd: 'x,
                                                    pd: 'x,
                                                    sq_ptr: enq_sq_ptr,
                                                    br_mask: branch_mask_enq
                                                };
            end
            
            // Update from mem unit
            if (update_lq_ent.valid && stations[station_to_update].valid) begin
                stations[station_to_update].addr_ready <= 1'b1;
                stations[station_to_update].address <= update_lq_ent.address;
                stations[station_to_update].ld_type <= update_lq_ent.ld_type;
                stations[station_to_update].rd      <= update_lq_ent.rd;
                stations[station_to_update].pd      <= update_lq_ent.pd;
            end

            // Take store updates.
            if (sq_ack) begin
                for (int i = 0; i < LQ_DEPTH; i++) begin
                    if(stations[i].valid) begin
                        if (stations[i].sq_ptr == sq_ack_idx) begin
                            stations[i].sq_ack_done <= 1'b1;
                        end
                    end
                end
            end

            // Handle Branch Update
            if (br_update) begin
                for (int i = 0; i < LQ_DEPTH; i++) begin
                    if (stations[i].branch_mask[bru_idx[BRU_BITS-1:0]]) begin
                        if (flush) begin
                            stations[i].valid <= 1'b0;
                        end else begin
                            stations[i].branch_mask[bru_idx[BRU_BITS-1:0]] <= 1'b0;
                        end
                    end
                end

                // update the issuer.
                if (iss_ent.branch_mask[bru_idx[BRU_BITS-1:0]]) begin
                    if (flush) begin
                        iss_ent.valid <= 1'b0;
                    end else begin
                        iss_ent.branch_mask[bru_idx[BRU_BITS-1:0]] <= 1'b0;
                    end
                end
            end

            // Issue out to the issue reg
            if (issue_en) begin
                iss_ent <= stations[station_to_issue];
                stations[station_to_issue].valid <= 1'b0;
            end

            // If we have a dequeue and no valid issue lined up, devalidate the issuing reg
            if (dequeue & !issue_en) begin
                iss_ent.valid <= 1'b0;
            end
        end
    end

    always_comb begin
        discard_ld = 1'b0;

        // If a mispredict happens, and the issued instruction is valid, raise a discard flag.
        if (flush && iss_ent.valid) begin 
            if (iss_ent.branch_mask[bru_idx[BRU_BITS-1:0]]) begin
                discard_ld = 1'b1;
            end
        end
    end

    // Dequeuing logic
    /*
        we select a station to issue, devalidate it in the stations array
        that station get latches into a reg
        once the load completes, we issue the dequeue signal
        if there is a new entry to issue, we latch that, otherwise devalidate the reg
        we don't issue new stations if the reg is valid w/o a dequeue.
    */
endmodule



module store_queue
import rv32i_types::*;
import ooo_config::*;
import ld_str_queue::*;
(
    input logic clk,
    input logic rst,

    // allocate an entry
    output logic                full,
    input  logic                enqueue,
    output logic [SQ_BITS:0]    enqueue_idx,        // to dispatch
    output logic [SQ_BITS-1:0]  last_enqueue_idx,   // to load queue

    // de-allocate an entry
    output logic                empty,
    input  logic                dequeue,
    
    output sq_entry_t           front,
    output logic [SQ_BITS-1:0]  front_idx,

    // update an entry
    input logic [SQ_BITS:0]     update_idx,
    input sq_entry_t            update_ent,

    // Recover Store Queue
    input logic                 flush,
    input logic [SQ_BITS:0]     flush_tail
);
    logic [SQ_BITS:0] head;
    logic [SQ_BITS:0] tail;

    sq_entry_t data [0:SQ_DEPTH-1]; 

    always_ff @(posedge clk) begin
        if(rst) begin
            head <= '0;
            tail <= '0;

            for (int i = 0; i < SQ_DEPTH; i++) begin
                data[i] <= '{default:'x, ready_to_request:1'b0};
            end
            
        end else begin
            if (enqueue & dequeue & (full | empty)) begin
                data[tail[SQ_BITS-1:0]] <= '{default:'x, ready_to_request:'0};
                tail <= tail + 1'b1;
                if (full) begin
                    head <= head + 1'b1;
                end
            end else begin
                // Write
                if (enqueue & !full) begin
                    // Allocate a new entry
                    data[tail[SQ_BITS-1:0]] <= '{default:'x, ready_to_request:'0};
                    tail <= tail + 1'b1;
                end
                // Read
                if (dequeue & !empty) begin
                    data[head[SQ_BITS-1:0]].ready_to_request <= 1'b0;  // Invalidate after dequeue, could improvise by checking can_issue only if not empty instead
                    head <= head + 1'b1;
                end
            end

            // Resert LSQ Tail on branch mispred
            if (flush) begin
                tail <= flush_tail;
            end
            
            // Update addr & set ready to issue 
            if (update_ent.ready_to_request) begin
                data[lsq_cast'(update_idx)] <= update_ent;
            end
        end
    end

    // Logic
    always_comb begin
        full  = (head[SQ_BITS] ^ tail[SQ_BITS]) & (head[SQ_BITS-1:0] == tail[SQ_BITS-1:0]);
        empty = (head == tail);
    end

    logic [SQ_BITS:0] flush_enq_tail;
    assign flush_enq_tail = flush_tail - 1'b1;


    always_ff @(posedge clk) begin
        if(rst)             last_enqueue_idx <= '0;
        else if(enqueue)    last_enqueue_idx <= tail[SQ_BITS-1:0];
        
        if (flush) begin
            if (flush_tail == head) begin
                last_enqueue_idx <= head[SQ_BITS-1:0];
            end else begin
                last_enqueue_idx <= flush_enq_tail[SQ_BITS-1:0];
            end

        end
    end 

    // return allocated index to dispatch (tail value in current cycle)
    assign enqueue_idx = tail; // str index

    // return entry at the front
    assign front = data[head[SQ_BITS-1:0]];
    assign front_idx = head[SQ_BITS-1:0];

endmodule 
