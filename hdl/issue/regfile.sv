/*  The regfile is split into two parts:
    1. The regfile backend - parameterized, source regs, dest regs, wdata, and write enables. Functions very closely to mp_pipeline
    2. The regfile *frontend* - which hooks up to the resvervation station, cdb, and execute interfaces

    The point of this is so that it's easy to view the state of the regfile independent of the interfaces,
    while allowing us to have very specific care in how we hook up the regfile to the interfaces
*/
module regfile_backend
import ooo_config::*;
(
    input  logic                 clk,

    input  logic                 regf_we    [REGF_WRITE],
    input  logic [31:0]          pd_v       [REGF_WRITE],
    input  logic [PHYS_BITS-1:0] pd_s       [REGF_WRITE],

    input  logic [PHYS_BITS-1:0] ps1_s      [REGF_READ],
    input  logic [PHYS_BITS-1:0] ps2_s      [REGF_READ],

    output logic [31:0]          ps1_v      [REGF_READ],
    output logic [31:0]          ps2_v      [REGF_READ]
);

    logic [31:0] data [PHYS_REGS];

    always_ff @(posedge clk) begin
        for (int i = 0; i < REGF_WRITE; i++) begin
            if (regf_we[i] && (pd_s[i] != '0)) begin
                data[pd_s[i]] <= pd_v[i];
            end
        end
        // TODO: More elegant solution? lol
        data[0] <= '0;
    end

    // Forwarding is NOT implemented.
    // If the reservation station can issue the same cycle the CDB marks a preg valid - then we would need to forward
    // Otherwise, its likely safe to assume the state of the issue stage will take 1 cycle of the CDB to resolve.
    always_comb begin
        for (int i = 0; i < REGF_READ; i++) begin
            ps1_v[i] = data[ps1_s[i]];
            ps2_v[i] = data[ps2_s[i]];
        end
    end

endmodule

// use '{a,b,c} for position assignment w/ unpacked arrays
module regfile
import ooo_config::*;
(
    input clk,

    resv_regf.regf resv,
    regf_func.regf func,
    cdb.regf       cdb
);

    logic                 regf_we    [REGF_WRITE];
    logic [31:0]          pd_v       [REGF_WRITE];
    preg                  pd_s       [REGF_WRITE];

    regfile_backend rf( .clk(clk),
                        .regf_we(cdb.valid),
                        .pd_v(cdb.data),
                        .pd_s(cdb.pd),

                        .ps1_s(resv.ps1),
                        .ps2_s(resv.ps2),

                        .ps1_v(func.ps1_v),
                        .ps2_v(func.ps2_v)
                    );

endmodule
