// This file is part of the GBC-ML project.
// Licensing information is available in the LICENSE file.
// (C) 2020 Nandor Licker. All rights reserved.

module TIMER
  ( input clk
  , input rst
  , input             timer_enable
  , input      [1:0]  timer_freq
  , output reg [7:0]  timer_div
  , input      [7:0]  timer_new_div
  , output reg [7:0]  timer_counter
  , input             timer_set_counter
  , input      [7:0]  timer_new_counter
  , input      [7:0]  timer_mod
  , output reg        timer_request
  );

  reg [15:0] timer_div_main;
  reg [10:0] timer_main;

  reg [5:0] threshold;

  always @*
    case (timer_freq)
      2'b00: threshold = 1024;
      2'b01: threshold = 16;
      2'b10: threshold = 64;
      2'b11: threshold = 256;
    endcase

  always @(posedge clk) begin
    if (timer_set_counter) begin
      timer_counter = timer_new_counter;
    end

    if (rst) begin
      timer_div_main <= 0;
      timer_div <= 0;

      timer_main <= 0;

      timer_request <= 0;
      timer_counter <= 0;
    end else begin
      timer_div_main <= timer_div_main + 1;
      timer_div <= timer_div_main[15:8];

      timer_main <= timer_main + 1;

      timer_request <= 0;
      if (timer_enable) begin
        if (timer_main >= threshold) begin
          timer_main <= 0;
          timer_counter <= timer_counter + 1;
          if (timer_counter == 8'hff) begin
            timer_counter <= timer_mod;
            timer_request <= 1;
          end
        end
      end
    end
  end
endmodule
