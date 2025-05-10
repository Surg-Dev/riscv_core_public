/*
*   Common Data Bus (CDB) <-> Everything
*   The common data bus is a bundle of smaller buses from each functional unit.
*   We expose as many ports as necessary on the target interface to support each functional unit
*   TODO: Determine performance of doing this vs arbitration
*/

interface cdb
import ooo_config::*;
import branch_types::*;
();

    logic [31:0] data        [FUNC_UNITS];
    preg         pd          [FUNC_UNITS];
    rreg         rd          [FUNC_UNITS];
    logic        valid       [FUNC_UNITS];
    rob_ent      rob_index   [FUNC_UNITS];

    typedef struct packed {
        logic [31:0] data;
        preg pd;
        rreg rd;
        logic valid;
        rob_ent rob_index;
    } cdb_out_t;

    cdb_out_t alu_c, mul_c, div_c, bru_c, mem_c;

    // If they ask me why there's a massive hole in the wall in my room I'm going to say this is why
    assign data[ALU] = alu_c.data;
    assign pd[ALU] = alu_c.pd;
    assign rd[ALU] = alu_c.rd;
    assign valid[ALU] = alu_c.valid;
    assign rob_index[ALU] = alu_c.rob_index;

    assign data[MUL] = mul_c.data;
    assign pd[MUL] = mul_c.pd;
    assign rd[MUL] = mul_c.rd;
    assign valid[MUL] = mul_c.valid;
    assign rob_index[MUL] = mul_c.rob_index;

    assign data[DIV] = div_c.data;
    assign pd[DIV] = div_c.pd;
    assign rd[DIV] = div_c.rd;
    assign valid[DIV] = div_c.valid;
    assign rob_index[DIV] = div_c.rob_index;

    assign data[MEM] = mem_c.data;
    assign pd[MEM] = mem_c.pd;
    assign rd[MEM] = mem_c.rd;
    assign valid[MEM] = mem_c.valid;
    assign rob_index[MEM] = mem_c.rob_index;

    assign data[BRU] = bru_c.data;
    assign pd[BRU] = bru_c.pd;
    assign rd[BRU] = bru_c.rd;
    assign valid[BRU] = bru_c.valid;
    assign rob_index[BRU] = bru_c.rob_index;
    ebr_t br_resolve;

    // Dispatch + Issue
    // modport rat(
    //     input pd,
    //     input rd,
    //     input valid
    // );

    modport disp(
        input br_resolve
    );

    modport fetch(
        input br_resolve
    );

    modport resv(
        input data,
        input pd,
        input rd,
        input valid,
        input br_resolve
    );

    modport regf(
        input data,
        input pd,
        input valid
    );

    modport rob(
        input valid,
        input rob_index,
        input br_resolve
    );

    // Functional Units
    modport alu(
        output alu_c,
        input br_resolve
    );

    modport mul(
        output mul_c,
        input br_resolve
    );

    modport div(
        output div_c,
        input br_resolve
    );

    modport mem(
        output mem_c,
        input br_resolve
    );
    
    modport bru(
        output bru_c,
        output br_resolve
    );

    modport lsq(
        input br_resolve
    );


endinterface

/*
*   Reservation Stations (RESV) <-> Register File (REGF)
*   The reservation station will continuously make read requests to the regfile
*/
interface resv_regf
import ooo_config::*;
();

    preg ps1 [REGF_READ];
    preg ps2 [REGF_READ];
    modport resv(
        output ps1,
        output ps2
    );

    modport regf(
        input  ps1,
        input  ps2
    );

endinterface

interface disp_resv
import ooo_config::*;
import resv_station::*;
();

    logic        full;  // can resv accept new instructions?
    resv_entry_t entry;

    modport resv(
        input  entry,
        output   full
    );

    modport disp(
        input   full,
        output  entry
    );

endinterface

/*
*   Reservation Stations (RESV) <-> Functional Units (FUNC)
*   The reservation stations will only issue if the functional units are free
*   Also - the functional units will only accept an instruction if the reservation station decides to issue it.
*/
interface resv_func
import ooo_config::*;
import resv_station::*;
();


    logic issue      [FUNC_UNITS];
    resv_entry_t resv_entry [FUNC_UNITS];

    logic alu_issue, mul_issue, div_issue, bru_issue, mem_issue;
    resv_entry_t alu_resv_entry, mul_resv_entry, div_resv_entry, bru_resv_entry, mem_resv_entry;

    assign alu_issue = issue[ALU];
    assign mul_issue = issue[MUL];
    assign div_issue = issue[DIV];
    assign bru_issue = issue[BRU];
    assign mem_issue = issue[MEM];

    assign alu_resv_entry = resv_entry[ALU];
    assign mul_resv_entry = resv_entry[MUL];
    assign div_resv_entry = resv_entry[DIV];
    assign bru_resv_entry = resv_entry[BRU];
    assign mem_resv_entry = resv_entry[MEM];

    modport resv(
        output issue,
        output resv_entry
    );

    // Functional Units
    modport alu(
        input alu_issue,
        input alu_resv_entry
    );
    modport mul(
        input mul_issue,
        input mul_resv_entry
    );
    modport div(
        input  div_issue,
        input  div_resv_entry
    );
    modport bru(
        input  bru_issue,
        input  bru_resv_entry
    );
    modport mem (
        input  mem_issue,
        input  mem_resv_entry
    );

endinterface

/*
*   Register File (REGF) <-> Functional Units (FUNC)
*   The regfile outputs its rdata to the functional units for use
*/

interface regf_func
import ooo_config::*;
();

    logic [31:0] ps1_v [REGF_READ];
    logic [31:0] ps2_v [REGF_READ];

    logic [31:0] alu_ps1_v, mul_ps1_v, div_ps1_v, bru_ps1_v, mem_ps1_v;
    logic [31:0] alu_ps2_v, mul_ps2_v, div_ps2_v, bru_ps2_v, mem_ps2_v;

    assign alu_ps1_v = ps1_v[ALU];
    assign mul_ps1_v = ps1_v[MUL];
    assign div_ps1_v = ps1_v[DIV];
    assign bru_ps1_v = ps1_v[BRU];
    assign mem_ps1_v = ps1_v[MEM];
    
    assign alu_ps2_v = ps2_v[ALU];
    assign mul_ps2_v = ps2_v[MUL];
    assign div_ps2_v = ps2_v[DIV];
    assign bru_ps2_v = ps2_v[BRU];
    assign mem_ps2_v = ps2_v[MEM];
   

    modport regf(
        output ps1_v,
        output ps2_v
    );

    modport alu(
        input alu_ps1_v,
        input alu_ps2_v
    );

    modport mul(
        input mul_ps1_v,
        input mul_ps2_v
    );

    modport div(
        input div_ps1_v,
        input div_ps2_v
    );
    modport mem(
        input mem_ps1_v,
        input mem_ps2_v
    );


    modport bru(
        input bru_ps1_v,
        input bru_ps2_v
    );

endinterface

interface disp_rob
import ooo_config::*;
();

    logic   en;    // instruction is being dispatched
    preg    pd;
    rreg    rd;
    rob_ent idx;   // Allocated ROB index
    logic   full;  // ROB allocation successful?
    logic   is_str;
    logic [31:0] pc;
    logic br_commit;

    //RVFI
    rvfi_t rvfi;

    modport rob(
        input   en,
        input   pd,
        input   rd,
        input   is_str,
        input   pc,
        output  idx,
        output  full,
        output  br_commit,

        input rvfi
    );

    // ventilator be like:
    // "We don't negotiate with terrorists"
    // I tried the diplomatic way! Really! I did!
    modport disp(
        output  en,
        output  pd,
        output  rd,
        output  is_str,
        output  pc,
        input   idx,
        input   full,
        input   br_commit,

        output rvfi
    );

endinterface

interface disp_lsq
import ooo_config::*;
();

    logic   en;    // instruction is being dispatched
    logic   is_str;
    logic   lq_full;
    logic   sq_full;
    lsq_ent idx; // allocated lsq entry 
    rob_ent rob_entry;
    br_mask branch_mask;

    modport disp(
        output  en,
        output  is_str,
        input   lq_full,
        input   sq_full,
        input   idx,
        output rob_entry,
        output branch_mask
    );
    
    modport lsq(
        input   en,
        input   is_str,
        output  lq_full,
        output  sq_full,
        output  idx,
        input   rob_entry,
        input   branch_mask
    );

endinterface


interface mem_lsq
import ooo_config::*;
();
    // Update LSQ entry
    logic           valid;
    logic [31:0]    addr;
    logic [2:0]     ld_str_type;
    
    lsq_ent         lsq_idx;
    rob_ent         rob_idx;
    rreg            rd;
    preg            pd;
    logic [31:0]    str_val;
    logic           is_str;

    // LSQ dequeue
    logic           deq_done;
    logic [31:0]    deq_ld_data;
    rob_ent         deq_rob_idx;
    rreg            deq_rd;
    preg            deq_pd;
 
    modport mem(
        output valid,
        output addr,
        output ld_str_type,
        output lsq_idx,
        output rob_idx,
        output rd,
        output pd,
        output str_val,
        output is_str,

        input deq_done,
        input deq_ld_data,
        input deq_rob_idx,
        input deq_rd,
        input deq_pd
    );

    modport lsq(
        input valid,
        input addr,
        input ld_str_type,
        input lsq_idx,
        input rob_idx,
        input rd,
        input pd,
        input str_val,
        input is_str,

        output deq_done,
        output deq_ld_data,
        output deq_rob_idx,
        output deq_rd,
        output deq_pd
    );

endinterface

interface rob_lsq
import ooo_config::*;
();
    rob_ent head;

    rvfi_mem_t rvfi;
    rob_ent    rvfi_rob_idx;
    logic      rvfi_ready;

    modport rob(
        output  head,
        input   rvfi,
        input   rvfi_rob_idx,
        input   rvfi_ready
    );

    modport lsq(
        input   head,
        output  rvfi,
        output  rvfi_rob_idx,
        output  rvfi_ready
    );

endinterface

/*
*   Reorder Buffer (ROB) <-> Retirement Register Alias Table (RRAT)
*   When the ROB commits, it proves to the RRAT the new in-order alias of the physical regfile.
*/

interface rob_rrat
import ooo_config::*;
();

    logic       regf_we;
    preg        pd;
    logic [4:0] rd;

    modport rob(
        output regf_we,
        output pd,
        output rd
    );
    modport rrat(
        input  regf_we,
        input  pd,
        input  rd
    );

endinterface //rob_rrat

/*
*   ROB <-> Free List
*   ROB needs to enqueue newly-freed physical registers onto the free list for future use.
*/
interface rrat_flist
import ooo_config::*;
();

    logic enqueue;
    preg  phys_reg;

    modport rrat (
        output enqueue,
        output phys_reg
    );

    modport flist (
        input enqueue,
        input phys_reg
    );

endinterface //rrat_flist


/*
*   Free List <-> Dispatch
*   Free List needs to expose free physical registers for dispatch to use in renaming
*/
interface flist_disp
import ooo_config::*;
();

    logic dequeue;
    preg  phys_reg;

    logic full;
    logic empty;

    logic [ROB_BITS:0] cur_head;
    logic [ROB_BITS:0] ckpt_head;

    logic flush;

    modport flist (
        input dequeue,
        output phys_reg,
        output full,
        output empty,
        output cur_head,
        input  ckpt_head,
        input  flush
    );

    modport disp (
        output dequeue,
        input phys_reg,
        input full,
        input empty,
        input cur_head,
        output ckpt_head,
        output flush
    );

endinterface //rrat_flist

interface fetch_disp
import ooo_config::*;
();

    logic [31:0]   rdata;
    logic          empty;
    logic        dequeue;

    logic [31:0] pc;
    logic [31:0] pc_next;

    modport fetch(
        input  dequeue,
        output   rdata,
        output   empty,

        output pc,
        output pc_next
    );

    modport disp(
        input     rdata,
        input     empty,
        output  dequeue,

        input pc,
        input pc_next
    );

endinterface

// // this interface is a certified SSShitstorm
// interface branch
// import branch_types::*;
// import ooo_config::*;
// ();

//     logic [31:0] pc_target;

//     preg [0:31] shadow_rat; // rrat/rat stores these as unpacked, be careful
//     // logic [4:0]  tag;     // TODO probably need a special in-flight tag for branches

//     modport fetch(
//         input pc_target
//     );

//     modport rob(
//         output pc_target
//     );

//     modport rrat(
//         output shadow_rat
//     );
//     modport disp(
//         input  shadow_rat
//     );

//     // modport bp(
//     //     input flush,
//     //     input pc_next, // unnecessary?
//     //     input tag
//     // );

// endinterface

// query PC from rob instead of storing in rs
interface bru_rob
import ooo_config::*;
import branch_types::*;
();

    logic        valid;
    rob_ent      idx;
    logic [31:0] pc;

    modport rob(
        input   valid,
        input   idx,
        output  pc
    );

    modport bru(
        output   valid,
        output   idx,
        input  pc
    );

endinterface
