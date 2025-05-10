module rrat
import ooo_config::*;
(
    input logic clk,
    input logic rst,

    rob_rrat.rrat   rob,
    rrat_flist.rrat flist
);

    preg reg_table [32];

    always_ff @(posedge clk) begin
        if (rst) begin
            // Reset the RRAT to assign the first 32 phys registers
            for (int unsigned i = 0; i < 32; i++) begin
                reg_table[i] <= PHYS_BITS'(i);
            end
        end else begin
            // If the ROB wants to commit an instruction, accept the new pd
            if (rob.regf_we) begin
                reg_table[rob.rd] <= rob.pd;
            end
        end
    end

    always_comb begin
        // On ROB Commit, free the current phys_reg before overwrite.
        flist.phys_reg = reg_table[rob.rd];
        // Don't allow enqueue if the destination is p0/x0
        flist.enqueue  = (rob.rd == '0) ? '0 : rob.regf_we;
    end

endmodule : rrat
