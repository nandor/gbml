// This file is part of the GBC-ML project.
// Licensing information is available in the LICENSE file.
// (C) 2020 Nandor Licker. All rights reserved.

module SYSTEM
  ( input clk
  , input rst
  );

  wire[15:0] addr;
  wire[7:0]  data_in;
  wire[7:0]  data_out;
  wire       rd_enable;
  wire       wr_enable;
  wire[7:0]  int_pending;
  wire[7:0]  int_enable;
  wire[7:0]  int_clear;

  CPU cpu(
      .clk(clk),
      .rst(rst),
      .addr(addr),
      .data_in(data_in),
      .data_out(data_out),
      .rd_enable(rd_enable),
      .wr_enable(wr_enable),
      .int_pending(int_pending),
      .int_enable(int_enable),
      .int_clear(int_clear)
  );

  MMU mmu(
      .addr(addr),
      .data_in(data_out),
      .data_out(data_in),
      .rd_enable(rd_enable),
      .wr_enable(wr_enable),
      .int_pending(int_pending),
      .int_enable(int_enable)
  );

endmodule

// The test bench generates the clock and the reset signals.
module TESTBENCH;
  reg clk;
  reg rst;

  SYSTEM sys(clk, rst);

  initial begin
    rst = 0; clk = 0;
    #5 rst = 1;
    #5 rst = 0;
  end
  always begin
    clk = #5 ~clk;
    $gpu_tick;
  end

  initial begin #500000000 $finish;
  end
endmodule
