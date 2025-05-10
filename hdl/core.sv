module core
import rv32i_types::*;
(
    input   logic           clk,
    input   logic           rst,

    output  logic   [31:0]  ic_addr,
    output  logic   [3:0]   ic_rmask,
    input   logic   [31:0]  ic_rdata,
    input   logic           ic_resp,

    output  logic   [31:0]  dc_addr,
    output  logic   [3:0]   dc_rmask,
    output  logic   [3:0]   dc_wmask,
    input   logic   [31:0]  dc_rdata,
    output  logic   [31:0]  dc_wdata,
    input   logic           dc_resp
);

cdb             cdb_itf  (.*);

fetch_disp fetch_disp_itf(.*);

flist_disp flist_disp_itf(.*);
disp_rob   disp_rob_itf  (.*);
disp_resv  disp_resv_itf (.*);

resv_regf  resv_regf_itf(.*);
resv_func  resv_func_itf(.*);
regf_func  regf_func_itf(.*);
bru_rob    bru_rob_itf(.*);

rob_rrat    rob_rrat_itf (.*);
rrat_flist  rrat_flist_itf (.*);

disp_lsq    disp_lsq_itf (.*);
rob_lsq     rob_lsq_itf  (.*);
mem_lsq     mem_lsq_itf  (.*);


fetch fetch(
    .clk(clk),
    .rst(rst),
    .ic_addr(ic_addr),
    .ic_rmask(ic_rmask),
    .ic_rdata(ic_rdata),
    .ic_resp(ic_resp),

    .disp_itf(fetch_disp_itf),
    .cdb(cdb_itf)
);

dispatch dispatch(.clk(clk),
                  .rst(rst),
                  .fetch(fetch_disp_itf),
                  .flist(flist_disp_itf),
                  .rob(disp_rob_itf),
                  .resv(disp_resv_itf),
                  .lsq(disp_lsq_itf),
                  .cdb(cdb_itf));

free_list free_list(.clk(clk),
                    .rst(rst),
                    .rrat(rrat_flist_itf),
                    .disp_itf(flist_disp_itf));

rob       rob(.clk(clk),
              .rst(rst),
              .disp_itf(disp_rob_itf),
              .cdb_itf(cdb_itf),
              .bru_itf(bru_rob_itf),
              .lsq_itf(rob_lsq_itf),
              .rrat(rob_rrat_itf));

resv       resv(.clk(clk),
                .rst(rst),
                .disp(disp_resv_itf),
                .func(resv_func_itf),
                .regf(resv_regf_itf),
                .cdb(cdb_itf));

regfile   regf(.clk(clk),
               .resv(resv_regf_itf),
               .func(regf_func_itf),
               .cdb(cdb_itf));

alu       alu(.clk(clk),
              .rst(rst),
              .resv(resv_func_itf),
              .regf(regf_func_itf),
              .cdb(cdb_itf));

mul       mul(.clk(clk),
              .rst(rst),
              .resv(resv_func_itf),
              .regf(regf_func_itf),
              .cdb(cdb_itf));

div       div(.clk(clk),
              .rst(rst),
              .resv(resv_func_itf),
              .regf(regf_func_itf),
              .cdb(cdb_itf));

mem      mem(.clk(clk),
             .rst(rst),
             .resv(resv_func_itf),
             .regf(regf_func_itf),
             .cdb(cdb_itf),
             .lsq(mem_lsq_itf));

lsq_split    lsq(.clk(clk),
             .rst(rst),
             .disp(disp_lsq_itf),
             .rob(rob_lsq_itf),
             .mem(mem_lsq_itf),
             .cdb(cdb_itf),
             .dc_addr(dc_addr),
             .dc_rmask(dc_rmask),
             .dc_wmask(dc_wmask),
             .dc_rdata(dc_rdata),
             .dc_wdata(dc_wdata),
             .dc_resp(dc_resp));

bru       bru(.clk(clk),
              .rst(rst),
              .resv(resv_func_itf),
              .regf(regf_func_itf),
              .cdb(cdb_itf),
              .rob(bru_rob_itf));

rrat      rrat( .clk(clk),
                .rst(rst),
                .rob(rob_rrat_itf),
                .flist(rrat_flist_itf));

endmodule
