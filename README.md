# Basic RISC-V Core

This was the RISC-V Core that we made in ECE411:
 - RV32IM ISA
 - 4kb DCache 16 set, 4 way, 32 bytes
 - 4kB ICache 64 sets, 32 bytes
 - Early Branch Recovery
 - Static Not Taken Branch Prediction (Not Good ™)
 - Clock Gated SRAM blocks in DCache
 - Split Load Store Queue
 - Unified Reservation Stations (8 Depth)
 - Explicit Register Renaming with 40 Physical Registers
 - 8 Depth ROB

 By the end of the competition, we were able to synthesize (using the Synopsys Toolchain and IPs, using FreePDK-45nm) at 168619 µm², 558.66MHz, with roughly a .45 IPC on standard Coremark.
 Power, Delay, and Area were all metrics that we had to optimize for, which led to the very small ROB and Physical Regfile.

 
 As the testbenches and memory models were not written by me, I cannot provide them here. The cache among other things were too simple in accordance with class resources that they were also not published here.
