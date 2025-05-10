module dispatch
import rv32i_types::*;
import ooo_config::*;
import resv_station::*;
(
    input  logic           clk,
    input  logic           rst,

    fetch_disp.disp        fetch,
    flist_disp.disp        flist,
    disp_rob.disp          rob,
    disp_resv.disp         resv,
    disp_lsq.disp          lsq,
    cdb.disp               cdb
);

    /*
        Dispatch is in charge of querying the current state of the processor, generating the rs entry, and deciding whether to issue

        Dispatch will stall under the following conditions:

        - The Fetch queue is empty/waiting on imem (likely)
        - The free list is empty/The ROB is full/Reservation Stations are full (unlikely)
            - These roughly all coincide, in fact, the flist probably can never be empty on its own
        
        If no stall condition is met it will interface the following:
        - Dequeue the fetch queue
        - Enable a pd update on the RAT
        - Allocate an entry on the ROB
        - Allocate a pd from the flist (if it is not a cond br/str/rd=0)
        - Allocate an entry on the LSQ (if its a ld/str)
        - Enable the reservation station to accept an entry

    */

    typedef struct packed {
        preg  [0:31] reg_table;
        logic [ROB_BITS:0] free_head;
    } checkpoint_t;

    instr_t inst;
    resv_entry_t rstation_next;

    rreg rs1,rs2,rd;
    logic [31:0] imm;
    preg pd;

    logic is_ld_str, is_str;
    logic is_br, is_link;
    logic br_full, br_empty;

    rvfi_t rvfi;

    logic disp_en;
    logic alloc_en;

    // RAT lives here now
    preg [0:31] reg_table;

    // BRANCH CHECKPOINTING
    checkpoint_t checkpoints [BRU_DEPTH];
    logic [BRU_DEPTH-1:0] br_queue;
    logic [BRU_BITS:0] br_head, br_tail;

    logic flush;
    assign flush = cdb.br_resolve.valid & cdb.br_resolve.mispred;
    // logic br_res;

    
    assign br_full = (br_head[BRU_BITS] ^ br_tail[BRU_BITS]) & (br_head[BRU_BITS-1:0] == br_tail[BRU_BITS-1:0]);
    assign br_empty = (br_head == br_tail);

    // Breakout instruction data
    assign inst = fetch.rdata;
    assign rs1  = inst_rs1(inst);
    assign rs2  = inst_rs2(inst);
    assign rd   = inst_rd(inst);
    assign imm  = inst_imm(inst);
    
    
    // Memory Operation Controls
    assign is_ld_str = (inst.r_type.opcode == op_b_store) || (inst.r_type.opcode == op_b_load);
    assign is_str = inst.r_type.opcode == op_b_store;
    // Branch Control
    assign is_br = inst.r_type.opcode inside {op_b_jal, op_b_jalr, op_b_br};
    assign is_link = inst.r_type.opcode inside {op_b_jal, op_b_jalr};
    

    // Dispatch Control 
    assign disp_en = (!fetch.empty && !resv.full && !rob.full  && !((is_str && lsq.sq_full)) && !(is_br && br_full) && !cdb.br_resolve.valid && !flist.empty);
    assign alloc_en = (disp_en & !(rd == '0 || inst.r_type.opcode inside {op_b_br, op_b_store}));

    assign pd   = alloc_en ? flist.phys_reg : '0;
    
    // Reservation Station Setup
    // Regs/Status
    assign rstation_next.pd        = pd;
    assign rstation_next.rd        = rd;
    assign rstation_next.ps1       = reg_table[rs1];
    assign rstation_next.ps1_valid = 1'b0;
    assign rstation_next.ps2       = reg_table[rs2];
    assign rstation_next.ps2_valid = 1'b0;
    // Immidiate
    assign rstation_next.imm = (inst.u_type.opcode == op_b_auipc) ? imm + fetch.pc : imm;
    assign rstation_next.imm_optional = inst_imm_optional(inst);
    // Metadata
    assign rstation_next.micro_op = inst_op(inst);
    assign rstation_next.rob_idx = rob.idx;
    assign rstation_next.lsq_idx = lsq.idx;
    assign rstation_next.branch_mask = br_queue;
    assign rstation_next.bru_idx = br_tail;

    // Load ROB
    assign rob.pd = pd;
    assign rob.rd = rd;
    assign rob.pc = fetch.pc;

    // LSQ Management
    assign lsq.rob_entry = rob.idx;
    assign lsq.branch_mask = br_queue;

    // RVFI Struct Gen
    assign rob.rvfi = '{rs1, rs2, rd, reg_table[rs1], reg_table[rs2], pd, fetch.pc, fetch.pc_next, fetch.rdata};

    // Allocate a Physical Register
    assign flist.dequeue = alloc_en;

    // Dispatch an Instruction
    assign fetch.dequeue = disp_en;
    assign lsq.en = is_ld_str & disp_en;
    assign lsq.is_str = is_str;
    assign rob.en = disp_en;
    assign rstation_next.valid = disp_en;

    // Wire up flist reset.
    assign flist.flush = flush;
    assign flist.ckpt_head = checkpoints[cdb.br_resolve.bru_idx[BRU_BITS-1:0]].free_head;


    // NOTE: BOOM says it's wise to store PC and the PC of the following instruction (e.g. when predicting the target of an unconditional branch)
    // I think without RV32C this is always PC+4, but I could be wrong. If so, this is the place to keep track

    always_ff @(posedge clk) begin
        if (rst | flush) begin
            resv.entry <= '{default:'x, micro_op:exe_t'('x), valid:0};
        end
        else begin
            if (disp_en) begin
                resv.entry <= rstation_next;
            end else if (resv.entry.valid) begin
                resv.entry.valid <= '0;
            end
        end
    end

    // RAT Table management
    always_ff  @(posedge clk) begin
        if (rst) begin
            for (int unsigned i=0; i< 32; i++) begin
                reg_table[i] <= PHYS_BITS'(i);
            end
        end else if (flush) begin
            reg_table <= checkpoints[cdb.br_resolve.bru_idx[BRU_BITS-1:0]].reg_table;
        end else begin
            if (alloc_en) begin
                reg_table[rd] <= pd;
            end
        end
    end

    // Branch Queue Management

    logic br_enq, br_deq;

    // Don't allow an enqueue if we are flushing.
    assign br_enq = disp_en & is_br & !flush;
    // We only pop allocated branches off once we've committed them.
    assign br_deq = rob.br_commit;

    always_ff @(posedge clk) begin
        if (rst) begin
            br_head <= '0;
            br_tail <= '0;
            br_queue <= '0;
        end else begin
            // If we are issuing a branch, then allocate on the queue
            if (br_enq & br_deq & (br_full | br_empty)) begin
                br_queue[br_tail[BRU_BITS-1:0]] <= 1'b1;
                br_tail <= br_tail + 1'b1;
                if (br_full) begin // do dequeue logic
                    br_head <= br_head + 1'b1;
                end
                checkpoints[br_tail[BRU_BITS-1:0]].reg_table <= reg_table;
                checkpoints[br_tail[BRU_BITS-1:0]].free_head <= (is_link & rd != '0) ? flist.cur_head + 1'b1 : flist.cur_head;
                if (is_link) begin
                    checkpoints[br_tail[BRU_BITS-1:0]].reg_table[rd] <= pd;
                end
            end else begin
                if (br_enq & !br_full) begin
                    br_queue[br_tail[BRU_BITS-1:0]] <= 1'b1;
                    br_tail <= br_tail + 1'b1;
                    // Checkpoint this branch
                    checkpoints[br_tail[BRU_BITS-1:0]].reg_table <= reg_table;
                    checkpoints[br_tail[BRU_BITS-1:0]].free_head <= (is_link & rd != '0) ? flist.cur_head + 1'b1 : flist.cur_head;
                
                    if (is_link) begin
                        checkpoints[br_tail[BRU_BITS-1:0]].reg_table[rd] <= pd;
                    end

                end

                if (br_deq & !br_empty) begin
                    br_queue[br_head[BRU_BITS-1:0]] <= 1'b0;
                    br_head <= br_head + 1'b1;
                end

            end

            // If cdb raises a resolved branch
            // deassert the branch queue bit to prevent tagging deps with it
            // if its a mispred, reset the branch queue to the state it was at on mispred
            if (cdb.br_resolve.valid) begin
                if (cdb.br_resolve.mispred) begin
                    br_queue <= br_queue & cdb.br_resolve.branch_mask;
                    br_tail <= cdb.br_resolve.bru_idx + 1'b1;
                end else begin
                    br_queue[cdb.br_resolve.bru_idx[BRU_BITS-1:0]] <= 1'b0;

                end
            end
        end
    end





endmodule
