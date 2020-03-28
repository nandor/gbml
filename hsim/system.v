// This file is part of the GBC-ML project.
// Licensing information is available in the LICENSE file.
// (C) 2020 Nandor Licker. All rights reserved.

module SYSTEM
  ( input clk
  , input rst
  );

  wire[15:0]  addr;
  wire[7:0]   data_in;
  wire[7:0]   data_out;
  wire        rd_enable;
  wire        wr_enable;

  wire[7:0]   int_enable;
  reg [7:0]   int_pending;
  wire[7:0]   int_cpu_clear;
  wire[7:0]   int_mmu_clear;
  wire[7:0]   int_mmu_request;
  wire[7:0]   int_hw_request;

  wire        timer_enable;
  wire[1:0]   timer_freq;
  wire[7:0]   timer_div;
  wire[7:0]   timer_new_div;
  wire[7:0]   timer_counter;
  wire        timer_set_counter;
  wire[7:0]   timer_new_counter;
  wire[7:0]   timer_mod;

  wire        timer_request;

  CPU cpu(
      .clk(clk),
      .rst(rst),
      .addr(addr),
      .data_in(data_in),
      .data_out(data_out),
      .rd_enable(rd_enable),
      .wr_enable(wr_enable),
      .int_pending(int_pending & int_enable),
      .int_clear(int_cpu_clear)
  );

  TIMER timer(
      .clk(clk),
      .rst(rst),
      .timer_enable(timer_enable),
      .timer_freq(timer_freq),
      .timer_div(timer_div),
      .timer_new_div(timer_new_div),
      .timer_counter(timer_counter),
      .timer_set_counter(timer_set_counter),
      .timer_new_counter(timer_new_counter),
      .timer_mod(timer_mod),
      .timer_request(timer_request)
  );

  MMU mmu(
      .addr(addr),
      .data_in(data_out),
      .data_out(data_in),
      .rd_enable(rd_enable),
      .wr_enable(wr_enable),
      .int_pending(int_pending),
      .int_enable(int_enable),
      .int_clear(int_mmu_clear),
      .int_request(int_mmu_request),
      .timer_enable(timer_enable),
      .timer_freq(timer_freq),
      .timer_div(timer_div),
      .timer_new_div(timer_new_div),
      .timer_counter(timer_counter),
      .timer_new_counter(timer_new_counter),
      .timer_set_counter(timer_set_counter),
      .timer_mod(timer_mod)
  );

  assign int_hw_request =
    { 3'b00
    , 1'b0
    , 1'b0
    , timer_request
    , 1'b0
    , 1'b0
    };

  always @(posedge |int_cpu_clear or posedge |int_mmu_clear or posedge |int_mmu_request or posedge |int_hw_request)
  begin
    int_pending = (int_pending | int_mmu_request | int_hw_request) & ~int_cpu_clear & ~int_mmu_clear;
  end
endmodule

// The test bench generates the clock and the reset signals.
module TESTBENCH;
  reg clk;
  reg rst;

  SYSTEM sys(clk, rst);

  always @(posedge clk) $gpu_tick;

  initial begin
    rst = 0;
    clk = 0;
    #5 rst = 1;
    #5 rst = 0;
  end

  always begin
    clk = #5 ~clk;
  end
endmodule
