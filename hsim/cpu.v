// This file is part of the GBC-ML project.
// Licensing information is available in the LICENSE file.
// (C) 2020 Nandor Licker. All rights reserved.

module CPU
  ( input             clk
  , input             rst
  , output reg [15:0] addr
  , input      [ 7:0] data_in
  , output reg [ 7:0] data_out
  , output reg        rd_enable
  , output reg        wr_enable
  , input      [7:0]  int_pending
  , output reg [7:0]  int_clear
  );

  // Cycle the instruction is in.
  reg[15:0] state;
  // Flags.
  reg zf;
  reg nf;
  reg hf;
  reg cf;
  // Registers.
  reg[15:0] pc;
  reg[15:0] sp;
  reg[7:0] b;
  reg[7:0] c;
  reg[7:0] d;
  reg[7:0] e;
  reg[7:0] h;
  reg[7:0] l;
  reg[7:0] a;
  // Interrupt master enable.
  reg ime;
  // Halt flag.
  reg halted;

  // Interrupt state.
  reg [4:0] int_state;

  // ALU & internal registers.
  reg[7:0] alu_lhs;
  reg[7:0] alu_rhs;
  reg[3:0] alu_op;
  reg[7:0] alu_r;
  reg      alu_zf;
  reg      alu_nf;
  reg      alu_hf;
  reg      alu_cf;
  reg[7:0] z;
  reg[7:0] w;
  reg[7:0] tmp;

  wire[4:0] daa_lo;
  wire[4:0] daa_hi;

  ALU alu
    ( alu_lhs
    , alu_rhs
    , alu_op
    , alu_r
    , zf, nf, hf, cf
    , alu_zf, alu_nf, alu_hf, alu_cf
    );

  always @(posedge clk) begin
    if (rst) begin
      state     <= 0;
      zf        <= 0;
      nf        <= 0;
      hf        <= 0;
      cf        <= 0;
      pc        <= 0;
      sp        <= 0;
      b         <= 0;
      c         <= 0;
      d         <= 0;
      e         <= 0;
      h         <= 0;
      l         <= 0;
      a         <= 0;
      ime       <= 0;
      halted    <= 0;
      int_clear <= 0;
      int_state <= 0;
    end else begin
      if (halted && |int_pending) begin
        halted <= 0;
      end

      if (int_state || (int_pending && state[7:0] == 8'h00 && ime)) begin
        case (int_state)
          // M1 T1: INT
          5'h00: begin
            int_state <= 5'h01;
            ime <= 0;
          end
          // M1 T2: INT
          5'h01: begin
            int_state <= 5'h02;
          end
          // M1 T3: INT
          5'h02: begin
            int_state <= 5'h03;
          end
          // M1 T4: INT
          5'h03: begin
            int_state <= 5'h04;
          end
          // M2 T1: INT
          5'h04: begin
            int_state <= 5'h05;
          end
          // M2 T2: INT
          5'h05: begin
            int_state <= 5'h06;
          end
          // M2 T3: INT
          5'h06: begin
            int_state <= 5'h07;
            sp <= sp - 1;
          end
          // M2 T4: INT
          5'h07: begin
            int_state <= 5'h08;
          end
          // M3 T1: INT
          5'h08: begin
            int_state <= 5'h09;
          end
          // M3 T2: INT
          5'h09: begin
            int_state <= 5'h0a;
          end
          // M3 T3: INT
          5'h0a: begin
            int_state <= 5'h0b;
          end
          // M3 T4: INT
          5'h0b: begin
            int_state <= 5'h0c;
          end
          // M4 T1: INT
          5'h0c: begin
            int_state <= 5'h0d;
            addr <= sp;
          end
          // M4 T2: INT
          5'h0d: begin
            int_state <= 5'h0e;
            data_out <= pc[15:8];
            wr_enable <= 1;
          end
          // M4 T3: INT
          5'h0e: begin
            int_state <= 5'h0f;
            wr_enable <= 0;
            sp <= sp - 1;
            pc <= { 8'h00, pc[7:0] };
          end
          // M4 T4: INT
          5'h0f: begin
            int_state <= 5'h10;
          end
          // M5 T1: INT
          5'h10: begin
            int_state <= 5'h11;
            addr <= sp;
          end
          // M5 T2: INT
          5'h11: begin
            int_state <= 5'h12;
            data_out <= pc[7:0];
            wr_enable <= 1;
          end
          // M5 T3: INT
          5'h12: begin
            int_state <= 5'h13;
            wr_enable <= 0;
            if (int_pending & 8'b00000001) begin
              pc <= { pc[15:8], 8'h40 };
              int_clear <= 8'b00000001;
            end else if (int_pending & 8'b00000010) begin
              pc <= { pc[15:8], 8'h48 };
              int_clear <= 8'b00000010;
            end else if (int_pending & 8'b00000100) begin
              pc <= { pc[15:8], 8'h50 };
              int_clear <= 8'b00000100;
            end else if (int_pending & 8'b00001000) begin
              pc <= { pc[15:8], 8'h58 };
              int_clear <= 8'b00001000;
            end else if (int_pending & 8'b00010000) begin
              pc <= { pc[15:8], 8'h60 };
              int_clear <= 8'b00010000;
            end
          end
          // M5 T4: INT
          5'h13: begin
            int_clear <= 8'h00;
            int_state <= 5'h00;
          end
        endcase
      end else if (!halted) begin
        casex (state)
          // M1 T1
          16'bxxxxxxxx00000000: begin
            addr <= pc;
            rd_enable <= 1;
            state <= 16'hxx01;
          end
          // M1 T2
          16'bxxxxxxxx00000001: begin
            state <= {data_in, 8'h02};
            rd_enable <= 0;
          end

          // M1 T3: LD r16, d16
          16'b00xx000100000010: begin
            state <= {state[15:8], 8'h3};
            pc <= pc + 1;
          end
          // M1 T4: LD r16, d16
          16'b00xx000100000011: begin
            state <= {state[15:8], 8'h4};
          end
          // M2 T1: LD r16, d16
          16'b00xx000100000100: begin
            state <= {state[15:8], 8'h5};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: LD r16, d16
          16'b00xx000100000101: begin
            state <= {state[15:8], 8'h6};
          end
          // M2 T3: LD r16, d16
          16'b00xx000100000110: begin
            state <= {state[15:8], 8'h7};
          end
          // M2 T4: LD r16, d16
          16'b00xx000100000111: begin
            state <= {state[15:8], 8'h8};
            rd_enable <= 0;
            pc <= pc + 1;
            case (state[13:12])
              2'b00: c <= data_in;
              2'b01: e <= data_in;
              2'b10: l <= data_in;
              2'b11: sp <= {sp[15:8], data_in};
            endcase
          end
          // M3 T1: LD r16, d16
          16'b00xx000100001000: begin
            addr <= pc;
            rd_enable <= 1;
            state <= {state[15:8], 8'h9};
          end
          // M3 T2: LD r16, d16
          16'b00xx000100001001: begin
            state <= {state[15:8], 8'ha};
          end
          // M3 T3: LD r16, d16
          16'b00xx000100001010: begin
            state <= {state[15:8], 8'hb};
          end
          // M3 T3: LD r16, d16
          16'b00xx000100001011: begin
            state <= {state[15:8], 8'h0};
            rd_enable <= 0;
            pc <= pc + 1;
            case (state[13:12])
              2'b00: b <= data_in;
              2'b01: d <= data_in;
              2'b10: h <= data_in;
              2'b11: sp <= {data_in, sp[7:0]};
            endcase
          end

          // M1 T3: LDI/LDD (HL), r8
          16'b001x001000000010: begin
            state <= {state[15:8], 8'h3};
            pc <= pc + 1;
          end
          // M1 T4: LDI/LDD (HL), r8
          16'b001x001000000011: begin
            state <= {state[15:8], 8'h4};
          end
          // M2 T1: LDI/LDD (HL), r8
          16'b001x001000000100: begin
            state <= {state[15:8], 8'h5};
            addr <= {h, l};
            data_out <= a;
            wr_enable <= 1;
          end
          // M2 T2: LDI/LDD (HL), r8
          16'b001x001000000101: begin
            state <= {state[15:8], 8'h6};
            wr_enable <= 0;
          end
          // M2 T3: LDI/LDD (HL), r8
          16'b001x001000000110: begin
            state <= {state[15:8], 8'h7};
          end
          // M2 T4: LDI/LDD (HL), r8
          16'b001x001000000111: begin
            state <= {state[15:8], 8'h0};
            case (state[12])
              1'b0: {h, l} <= {h, l} + 1;
              1'b1: {h, l} <= {h, l} - 1;
            endcase
          end

          // M1 T3: LDI/LDD r8, (HL)
          16'b001x101000000010: begin
            state <= {state[15:8], 8'h3};
            pc <= pc + 1;
          end
          // M1 T4: LDI/LDD r8, (HL)
          16'b001x101000000011: begin
            state <= {state[15:8], 8'h4};
          end
          // M2 T1: LDI/LDD r8, (HL)
          16'b001x101000000100: begin
            state <= {state[15:8], 8'h5};
            addr <= {h, l};
            rd_enable <= 1;
          end
          // M2 T2: LDI/LDD r8, (HL)
          16'b001x101000000101: begin
            state <= {state[15:8], 8'h6};
            a <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: LDI/LDD r8, (HL)
          16'b001x101000000110: begin
            state <= {state[15:8], 8'h7};
          end
          // M2 T4: LDI/LDD r8, (HL)
          16'b001x101000000111: begin
            state <= {state[15:8], 8'h0};
            case (state[12])
              1'b0: {h, l} <= {h, l} + 1;
              1'b1: {h, l} <= {h, l} - 1;
            endcase
          end

          // M1 T4: ALU (HL)
          16'b10xxx11000000010: begin
            state <= {state[15:8], 8'h3};
            pc <= pc + 1;
          end
          // M1 T3: ALU r8
          16'b10xxxxxx00000010: begin
            state <= {state[15:8], 8'h3};
            pc <= pc + 1;
            alu_lhs <= a;
            case (state[10:8])
              3'b000: alu_rhs <= b;
              3'b001: alu_rhs <= c;
              3'b010: alu_rhs <= d;
              3'b011: alu_rhs <= e;
              3'b100: alu_rhs <= h;
              3'b101: alu_rhs <= l;
              3'b111: alu_rhs <= a;
            endcase
            alu_op <= {1'b0, state[13:11]};
          end
          // M1 T4: ALU (HL)
          16'b10xxx11000000011: begin
            state <= {state[15:8], 8'h4};
          end
          // M1 T4: ALU r8
          16'b10xxxxxx00000011: begin
            state <= {state[15:8], 8'h0};
            a <= alu_r;
            zf <= alu_zf;
            nf <= alu_nf;
            hf <= alu_hf;
            cf <= alu_cf;
          end

          // M2 T1: ALU (HL)
          16'b10xxx11000000100: begin
            state <= {state[15:8], 8'h5};
            addr <= {h, l};
            rd_enable <= 1;
          end
          // M2 T2: ALU (HL)
          16'b10xxx11000000101: begin
            state <= {state[15:8], 8'h6};
            alu_rhs <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: ALU (HL)
          16'b10xxx11000000110: begin
            state <= {state[15:8], 8'h7};
            alu_lhs <= a;
            alu_op <= state[13:11];
          end
          // M2 T4: ALU (HL)
          16'b10xxx11000000111: begin
            state <= {state[15:8], 8'h0};
            a <= alu_r;
            zf <= alu_zf;
            nf <= alu_nf;
            hf <= alu_hf;
            cf <= alu_cf;
          end

          // M1 T3: LDI/LDD (HL), A
          16'b001x001000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: LDI/LDD (HL), A
          16'b001x001000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LDI/LDD (HL), A
          16'b001x001000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= {h, l};
            wr_enable <= 1;
            data_out <= a;
          end
          // M2 T2: LDI/LDD (HL), A
          16'b001x001000000101: begin
            state <= {state[15:8], 8'h06};
            wr_enable <= 0;
          end
          // M2 T3: LDI/LDD (HL), A
          16'b001x001000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LDI/LDD (HL), A
          16'b001x001000000111: begin
            state <= {state[15:8], 8'h00};
            {h, l} <= {h, l} + 1;
          end

          // M1 T3: CB
          16'b1100101100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: CB
          16'b1100101100000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: CB
          16'b1100101100000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: CB
          16'b1100101100000101: begin
            rd_enable <= 0;
            state <= {data_in, 8'h86};
          end

          // M2 T3: CB alu (HL)
          16'b00xxx11010000110: begin
            state <= {state[15:8], 8'h87};
            pc <= pc + 1;
          end
          // M2 T4: CB alu (HL)
          16'b00xxx11010000111: begin
            state <= {state[15:8], 8'h88};
          end
          // M3 T1: CB alu (HL)
          16'b00xxx11010001000: begin
            state <= {state[15:8], 8'h89};
            addr <= {h, l};
            rd_enable <= 1;
          end
          // M3 T2: CB alu (HL)
          16'b00xxx11010001001: begin
            state <= {state[15:8], 8'h8a};
            rd_enable <= 0;
            alu_lhs <= data_in;
          end
          // M3 T3: CB alu (HL)
          16'b00xxx11010001010: begin
            state <= {state[15:8], 8'h8b};
            alu_rhs <= a;
            alu_op <= {1'b1, state[13:11]};
          end
          // M3 T4: CB alu (HL)
          16'b00xxx11010001011: begin
            state <= {state[15:8], 8'h8c};
            z <= alu_r;
            zf <= alu_zf;
            nf <= alu_nf;
            hf <= alu_nf;
            cf <= alu_cf;
          end
          // M4 T1: CB alu (HL)
          16'b00xxx11010001100: begin
            state <= {state[15:8], 8'h8d};
            addr <= {h, l};
            data_out <= z;
            wr_enable <= 1;
          end
          // M4 T2: CB alu (HL)
          16'b00xxx11010001101: begin
            state <= {state[15:8], 8'h8e};
            wr_enable <= 0;
          end
          // M4 T3: CB alu (HL)
          16'b00xxx11010001110: begin
            state <= {state[15:8], 8'h8f};
          end
          // M4 T4: CB alu (HL)
          16'b00xxx11010001111: begin
            state <= {state[15:8], 8'h00};
          end

          // M2 T3: CB alu
          16'b00xxxxxx10000110: begin
            state <= {state[15:8], 8'h87};
            pc <= pc + 1;
            case (state[10:8])
              3'b000: alu_lhs <= b;
              3'b001: alu_lhs <= c;
              3'b010: alu_lhs <= d;
              3'b011: alu_lhs <= e;
              3'b100: alu_lhs <= h;
              3'b101: alu_lhs <= l;
              3'b111: alu_lhs <= a;
            endcase
            alu_rhs <= 8'h00;
            alu_op <= {1'b1, state[13:11]};
          end
          // M2 T4: CB alu
          16'b00xxxxxx10000111: begin
            state <= {state[15:8], 8'h00};
            case (state[10:8])
              3'b000: b <= alu_r;
              3'b001: c <= alu_r;
              3'b010: d <= alu_r;
              3'b011: e <= alu_r;
              3'b100: h <= alu_r;
              3'b101: l <= alu_r;
              3'b111: a <= alu_r;
            endcase
            zf <= alu_zf;
            nf <= alu_nf;
            hf <= alu_hf;
            cf <= alu_cf;
          end

          // M2 T3: CB BIT (HL)
          16'b01xxx11010000110: begin
            state <= {state[15:8], 8'h87};
            pc <= pc + 1;
          end
          // M2 T4: CB BIT (HL)
          16'b01xxx11010000111: begin
            state <= {state[15:8], 8'h88};
          end
          // M3 T1: CB BIT (HL)
          16'b01xxx11010001000: begin
            state <= {state[15:8], 8'h89};
            addr <= {h, l};
            rd_enable <= 1;
          end
          // M3 T2: CB BIT (HL)
          16'b01xxx11010001001: begin
            state <= {state[15:8], 8'h8a};
            tmp <= data_in;
            rd_enable <= 0;
          end
          // M3 T3: CB BIT (HL)
          16'b01xxx11010001010: begin
            state <= {state[15:8], 8'h8b};
          end
          // M3 T4: CB BIT (HL)
          16'b01xxx11010001011: begin
            state <= {state[15:8], 8'h00};
            zf <= !tmp[state[13:11]];
            nf <= 0;
            hf <= 1;
          end

          // M2 T3: CB BIT
          16'b01xxxxxx10000110: begin
            state <= {state[15:8], 8'h87};
            pc <= pc + 1;
          end
          // M2 T4: CB BIT
          16'b01xxxxxx10000111: begin
            state <= {state[15:8], 8'h00};
            case (state[10:8])
              3'b000: zf <= !b[state[13:11]];
              3'b001: zf <= !c[state[13:11]];
              3'b010: zf <= !d[state[13:11]];
              3'b011: zf <= !e[state[13:11]];
              3'b100: zf <= !h[state[13:11]];
              3'b101: zf <= !l[state[13:11]];
              3'b111: zf <= !a[state[13:11]];
            endcase
            nf <= 0;
            hf <= 1;
          end

          // M2 T3: CB RES (HL)
          16'b10xxx11010000110: begin
            state <= {state[15:8], 8'h87};
            pc <= pc + 1;
          end
          // M2 T3: CB RES (HL)
          16'b10xxx11010000111: begin
            state <= {state[15:8], 8'h88};
          end
          // M3 T1: CB RES (HL)
          16'b10xxx11010001000: begin
            state <= {state[15:8], 8'h89};
            addr <= {h, l};
            rd_enable <= 1;
          end
          // M3 T2: CB RES (HL)
          16'b10xxx11010001001: begin
            state <= {state[15:8], 8'h8a};
            tmp <= data_out;
            rd_enable <= 0;
          end
          // M3 T3: CB RES (HL)
          16'b10xxx11010001010: begin
            state <= {state[15:8], 8'h8b};
            tmp <= tmp & ~(8'd1 << state[13:11]);
          end
          // M3 T4: CB RES (HL)
          16'b10xxx11010001011: begin
            state <= {state[15:8], 8'h8c};
          end
          // M4 T1: CB RES (HL)
          16'b10xxx11010001100: begin
            state <= {state[15:8], 8'h8d};
            addr <= {h, l};
            wr_enable <= 1;
            data_out <= tmp;
          end
          // M4 T2: CB RES (HL)
          16'b10xxx11010001101: begin
            state <= {state[15:8], 8'h8e};
            wr_enable <= 0;
          end
          // M4 T3: CB RES (HL)
          16'b10xxx11010001110: begin
            state <= {state[15:8], 8'h8f};
          end
          // M4 T4: CB RES (HL)
          16'b10xxx11010001111: begin
            state <= {state[15:8], 8'h00};
          end

          // M2 T3: CB RES
          16'b10xxxxxx10000110: begin
            state <= {state[15:8], 8'h87};
            pc <= pc + 1;
          end
          // M2 T3: CB RES
          16'b10xxxxxx10000111: begin
            state <= {state[15:8], 8'h00};
            case (state[10:8])
              3'b000: b <= b & ~(8'd1 << state[13:11]);
              3'b001: c <= c & ~(8'd1 << state[13:11]);
              3'b010: d <= d & ~(8'd1 << state[13:11]);
              3'b011: e <= e & ~(8'd1 << state[13:11]);
              3'b100: h <= h & ~(8'd1 << state[13:11]);
              3'b101: l <= l & ~(8'd1 << state[13:11]);
              3'b111: a <= a & ~(8'd1 << state[13:11]);
            endcase
          end

          // M2 T3: CB SET (HL)
          16'b11xxx11010000110: begin
            state <= {state[15:8], 8'h87};
            pc <= pc + 1;
          end
          // M2 T3: CB SET (HL)
          16'b11xxx11010000111: begin
            state <= {state[15:8], 8'h88};
          end
          // M3 T1: CB SET (HL)
          16'b11xxx11010001000: begin
            state <= {state[15:8], 8'h89};
            addr <= {h, l};
            rd_enable <= 1;
          end
          // M3 T2: CB SET (HL)
          16'b11xxx11010001001: begin
            state <= {state[15:8], 8'h8a};
            tmp <= data_out;
            rd_enable <= 0;
          end
          // M3 T3: CB SET (HL)
          16'b11xxx11010001010: begin
            state <= {state[15:8], 8'h8b};
            tmp <= tmp | (8'd1 << state[13:11]);
          end
          // M3 T4: CB SET (HL)
          16'b11xxx11010001011: begin
            state <= {state[15:8], 8'h8c};
          end
          // M4 T1: CB SET (HL)
          16'b11xxx11010001100: begin
            state <= {state[15:8], 8'h8d};
            addr <= {h, l};
            wr_enable <= 1;
            data_out <= tmp;
          end
          // M4 T2: CB SET (HL)
          16'b11xxx11010001101: begin
            state <= {state[15:8], 8'h8e};
            wr_enable <= 0;
          end
          // M4 T3: CB SET (HL)
          16'b11xxx11010001110: begin
            state <= {state[15:8], 8'h8f};
          end
          // M4 T4: CB SET (HL)
          16'b11xxx11010001111: begin
            state <= {state[15:8], 8'h00};
          end

          // M2 T3: CB SET
          16'b11xxxxxx10000110: begin
            state <= {state[15:8], 8'h87};
            pc <= pc + 1;
          end
          // M2 T3: CB SET
          16'b11xxxxxx10000111: begin
            state <= {state[15:8], 8'h00};
            case (state[10:8])
              3'b000: b <= b | (8'd1 << state[13:11]);
              3'b001: c <= c | (8'd1 << state[13:11]);
              3'b010: d <= d | (8'd1 << state[13:11]);
              3'b011: e <= e | (8'd1 << state[13:11]);
              3'b100: h <= h | (8'd1 << state[13:11]);
              3'b101: l <= l | (8'd1 << state[13:11]);
              3'b111: a <= a | (8'd1 << state[13:11]);
            endcase
          end

          // M2 T3: RLCA
          16'b0000011100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
            alu_lhs <= a;
            alu_rhs <= 0;
            alu_op <= 4'b1000;
          end
          // M2 T4: RLCA
          16'b0000011100000011: begin
            state <= {state[15:8], 8'h00};
            a <= alu_r;
            zf <= 0;
            nf <= 0;
            hf <= 0;
            cf <= alu_cf;
          end

          // M2 T3: RLA
          16'b0001011100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
            alu_lhs <= a;
            alu_rhs <= 0;
            alu_op <= 4'b1010;
          end
          // M2 T4: RLA
          16'b0001011100000011: begin
            state <= {state[15:8], 8'h00};
            a <= alu_r;
            zf <= 0;
            nf <= 0;
            hf <= 0;
            cf <= alu_cf;
          end

          // M2 T3: RRCA
          16'b0000111100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
            alu_lhs <= a;
            alu_rhs <= 0;
            alu_op <= 4'b1001;
          end
          // M2 T4: RRCA
          16'b0000111100000011: begin
            state <= {state[15:8], 8'h00};
            a <= alu_r;
            zf <= 0;
            nf <= 0;
            hf <= 0;
            cf <= alu_cf;
          end

          // M2 T3: RRA
          16'b0001111100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
            alu_lhs <= a;
            alu_rhs <= 0;
            alu_op <= 4'b1011;
          end
          // M2 T4: RRA
          16'b0001111100000011: begin
            state <= {state[15:8], 8'h00};
            a <= alu_r;
            zf <= 0;
            nf <= 0;
            hf <= 0;
            cf <= alu_cf;
          end

          // M1 T3: JR CC
          16'b001xx00000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: JR CC
          16'b001xx00000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: JR CC
          16'b001xx00000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: JR CC
          16'b001xx00000000101: begin
            state <= {state[15:8], 8'h06};
            pc <= pc + 1;
            tmp <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: JR CC
          16'b001xx00000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: JR CC
          16'b001xx00000000111: begin
            case (state[12:11])
              2'b00: begin
                state <= {state[15:8], zf ? 8'h00 : 8'h08};
              end
              2'b01: begin
                state <= {state[15:8], zf ? 8'h08 : 8'h00};
              end
              2'b10: begin
                state <= {state[15:8], cf ? 8'h00 : 8'h08};
              end
              2'b11: begin
                state <= {state[15:8], cf ? 8'h08 : 8'h00};
              end
            endcase
          end
          // M3 T1: JR CC
          16'b001xx00000001000: begin
            state <= {state[15:8], 8'h09};
          end
          // M3 T2: JR CC
          16'b001xx00000001001: begin
            state <= {state[15:8], 8'h0a};
          end
          // M3 T3: JR CC
          16'b001xx00000001010: begin
            state <= {state[15:8], 8'h0b};
          end
          // M3 T4: JR CC
          16'b001xx00000001011: begin
            state <= {state[15:8], 8'h00};
            if (tmp[7])
              pc <= pc + {8'hff, tmp};
            else
              pc <= pc + {8'h00, tmp};
          end

          // M1 T3: JR
          16'b0001100000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: JR
          16'b0001100000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: JR
          16'b0001100000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: JR
          16'b0001100000000101: begin
            state <= {state[15:8], 8'h06};
            pc <= pc + 1;
            tmp <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: JR
          16'b0001100000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: JR
          16'b0001100000000111: begin
            state <= {state[15:8], 8'h08};
          end
          // M3 T1: JR
          16'b0001100000001000: begin
            state <= {state[15:8], 8'h09};
          end
          // M3 T2: JR
          16'b0001100000001001: begin
            state <= {state[15:8], 8'h0a};
          end
          // M3 T3: JR
          16'b0001100000001010: begin
            state <= {state[15:8], 8'h0b};
          end
          // M3 T4: JR
          16'b0001100000001011: begin
            state <= {state[15:8], 8'h00};
            if (tmp[7])
              pc <= pc + {8'hff, tmp};
            else
              pc <= pc + {8'h00, tmp};
          end

          // M1 T3: LD (HL), d8
          16'b0011011000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: LD (HL), d8
          16'b0011011000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LD (HL), d8
          16'b0011011000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
          end
          // M2 T2: LD (HL), d8
          16'b0011011000000101: begin
            state <= {state[15:8], 8'h06};
          end
          // M2 T3: LD (HL), d8
          16'b0011011000000110: begin
            state <= {state[15:8], 8'h07};
            rd_enable <= 1;
          end
          // M2 T4: LD (HL), d8
          16'b0011011000000111: begin
            state <= {state[15:8], 8'h08};
            pc <= pc + 1;
            tmp <= data_in;
            rd_enable <= 0;
          end
          // M3 T1: LD (HL), d8
          16'b0011011000001000: begin
            state <= {state[15:8], 8'h09};
            addr <= {h, l};
            data_out <= tmp;
            wr_enable <= 1;
          end
          // M3 T2: LD (HL), d8
          16'b0011011000001001: begin
            state <= {state[15:8], 8'h0a};
            wr_enable <= 0;
          end
          // M3 T3: LD (HL), d8
          16'b0011011000001010: begin
            state <= {state[15:8], 8'h0b};
          end
          // M3 T4: LD (HL), d8
          16'b0011011000001011: begin
            state <= {state[15:8], 8'h00};
          end


          // M1 T3: LD r8, d8
          16'b00xxx11000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: LD r8, d8
          16'b00xxx11000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LD r8, d8
          16'b00xxx11000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: LD r8, d8
          16'b00xxx11000000101: begin
            case (state[13:11])
              3'b000: b <= data_in;
              3'b001: c <= data_in;
              3'b010: d <= data_in;
              3'b011: e <= data_in;
              3'b100: h <= data_in;
              3'b101: l <= data_in;
              3'b111: a <= data_in;
            endcase
            rd_enable <= 0;
            state <= {state[15:8], 8'h06};
          end
          // M2 T3: LD r8, d8
          16'b00xxx11000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LD r8, d8
          16'b00xxx11000000111: begin
            state <= {state[15:8], 8'h00};
            pc <= pc + 1;
          end

          // M1 T3: LD (C), A
          16'b1110001000000010: begin
            pc <= pc + 1;
            state <= {state[15:8], 8'h03};
          end
          // M1 T3: LD (C), A
          16'b1110001000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LD (C), A
          16'b1110001000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= {8'hff, c};
            data_out <= a;
            wr_enable <= 1;
          end
          // M2 T2: LD (C), A
          16'b1110001000000101: begin
            state <= {state[15:8], 8'h06};
            wr_enable <= 0;
          end
          // M2 T3: LD (C), A
          16'b1110001000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LD (C), A
          16'b1110001000000111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: LD A, (C)
          16'b1111001000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T3: LD A, (C)
          16'b1111001000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LD A, (C)
          16'b1111001000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= {8'hff, c};
            rd_enable <= 1;
          end
          // M2 T2: LD A, (C)
          16'b1111001000000101: begin
            state <= {state[15:8], 8'h06};
            a <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: LD A, (C)
          16'b1111001000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LD A, (C)
          16'b1111001000000111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: INC/DEC (HL)
          16'b0011010x00000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: INC/DEC (HL)
          16'b0011010x00000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: INC/DEC (HL)
          16'b0011010x00000100: begin
            state <= {state[15:8], 8'h05};
            addr <= {h, l};
            rd_enable <= 1;
          end
          // M2 T2: INC/DEC (HL)
          16'b0011010x00000101: begin
            state <= {state[15:8], 8'h06};
            alu_lhs <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: INC/DEC (HL)
          16'b0011010x00000110: begin
            state <= {state[15:8], 8'h07};
            alu_rhs <= 8'h01;
            alu_op <= state[8] ? 4'b0010 : 4'b0000;
          end
          // M2 T4: INC/DEC (HL)
          16'b0011010x00000111: begin
            state <= {state[15:8], 8'h08};
            z <= alu_r;
            zf <= alu_zf;
            nf <= alu_nf;
            hf <= alu_hf;
          end
          // M3 T1: INC/DEC (HL)
          16'b0011010x00001000: begin
            state <= {state[15:8], 8'h09};
            addr <= {h, l};
            data_out <= z;
            wr_enable <= 1;
          end
          // M3 T2: INC/DEC (HL)
          16'b0011010x00001001: begin
            state <= {state[15:8], 8'h0a};
            wr_enable <= 0;
          end
          // M3 T3: INC/DEC (HL)
          16'b0011010x00001010: begin
            state <= {state[15:8], 8'h0b};
          end
          // M3 T4: INC/DEC (HL)
          16'b0011010x00001011: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: INC/DEC r8
          16'b00xxx10x00000010: begin
            pc <= pc + 1;
            state <= {state[15:8], 8'h03};
            alu_op <= state[8] ? 4'b0010 : 4'b0000;
            case (state[13:11])
              3'b000: alu_lhs <= b;
              3'b001: alu_lhs <= c;
              3'b010: alu_lhs <= d;
              3'b011: alu_lhs <= e;
              3'b100: alu_lhs <= h;
              3'b101: alu_lhs <= l;
              3'b111: alu_lhs <= a;
            endcase
            alu_rhs <= 8'h01;
          end
          // M1 T4: INC/DEC r8
          16'b00xxx10x00000011: begin
            state <= {state[15:8], 8'h00};
            case (state[13:11])
              3'b000: b <= alu_r;
              3'b001: c <= alu_r;
              3'b010: d <= alu_r;
              3'b011: e <= alu_r;
              3'b100: h <= alu_r;
              3'b101: l <= alu_r;
              3'b111: a <= alu_r;
            endcase
            zf <= alu_zf;
            nf <= alu_nf;
            hf <= alu_hf;
          end

          // M1 T3: HALT
          16'b0111011000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: HALT
          16'b0111011000000011: begin
            state <= {state[15:8], 8'h00};
            halted <= 1;
          end

          // M1 T3: LD (HL), r8
          16'b01110xxx00000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: LD (HL), r8
          16'b01110xxx00000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LD (HL), r8
          16'b01110xxx00000100: begin
            state <= {state[15:8], 8'h05};
            addr <= {h, l};
            case (state[10:8])
              3'b000: data_out <= b;
              3'b001: data_out <= c;
              3'b010: data_out <= d;
              3'b011: data_out <= e;
              3'b100: data_out <= h;
              3'b101: data_out <= l;
              3'b111: data_out <= a;
            endcase
            wr_enable <= 1;
          end
          // M2 T2: LD (HL), r8
          16'b01110xxx00000101: begin
            state <= {state[15:8], 8'h06};
            wr_enable <= 0;
          end
          // M2 T3: LD (HL), r8
          16'b01110xxx00000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LD (HL), r8
          16'b01110xxx00000111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: LD r8, (HL)
          16'b01xxx11000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: LD r8, (HL)
          16'b01xxx11000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LD r8, (HL)
          16'b01xxx11000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= {h, l};
            rd_enable <= 1;
          end
          // M2 T2: LD r8, (HL)
          16'b01xxx11000000101: begin
            state <= {state[15:8], 8'h06};
            rd_enable <= 0;
            case (state[13:11])
              3'b000: b <= data_in;
              3'b001: c <= data_in;
              3'b010: d <= data_in;
              3'b011: e <= data_in;
              3'b100: h <= data_in;
              3'b101: l <= data_in;
              3'b111: a <= data_in;
            endcase
          end
          // M2 T3: LD r8, (HL)
          16'b01xxx11000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LD r8, (HL)
          16'b01xxx11000000111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: LD r8, r8
          16'b01xxxxxx00000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
            case (state[10:8])
              3'b000: tmp <= b;
              3'b001: tmp <= c;
              3'b010: tmp <= d;
              3'b011: tmp <= e;
              3'b100: tmp <= h;
              3'b101: tmp <= l;
              3'b111: tmp <= a;
            endcase
          end
          // M1 T4: LD r8, r8
          16'b01xxxxxx00000011: begin
            state <= {state[15:8], 8'h00};
            case (state[13:11])
              3'b000: b <= tmp;
              3'b001: c <= tmp;
              3'b010: d <= tmp;
              3'b011: e <= tmp;
              3'b100: h <= tmp;
              3'b101: l <= tmp;
              3'b111: a <= tmp;
            endcase
          end

          // M1 T3: LDH (a8), a
          16'b1110000000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: LDH (a8), a
          16'b1110000000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LDH (a8), a
          16'b1110000000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: LDH (a8), a
          16'b1110000000000101: begin
            state <= {state[15:8], 8'h06};
            z <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: LDH (a8), a
          16'b1110000000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LDH (a8), a
          16'b1110000000000111: begin
            state <= {state[15:8], 8'h08};
            pc <= pc + 1;
          end
          // M3 T1: LDH (a8), a
          16'b1110000000001000: begin
            state <= {state[15:8], 8'h09};
            addr <= {8'hff, z};
            data_out <= a;
            wr_enable <= 1;
          end
          // M3 T2: LDH (a8), a
          16'b1110000000001001: begin
            state <= {state[15:8], 8'h0a};
            wr_enable <= 0;
          end
          // M3 T3: LDH (a8), a
          16'b1110000000001010: begin
            state <= {state[15:8], 8'h0b};
          end
          // M3 T4: LDH (a8), a
          16'b1110000000001011: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: LD A, (r16)
          16'b000x101000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: LD A, (r16)
          16'b000x101000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LD A, (r16)
          16'b000x101000000100: begin
            state <= {state[15:8], 8'h05};
            case (state[12])
              1'b0: addr <= {b, c};
              1'b1: addr <= {d, e};
            endcase
            rd_enable <= 1;
          end
          // M2 T2: LD A, (r16)
          16'b000x101000000101: begin
            state <= {state[15:8], 8'h06};
            a <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: LD A, (r16)
          16'b000x101000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LD A, (r16)
          16'b000x101000000111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: CALL
          16'b1100110100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: CALL
          16'b1100110100000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: CALL
          16'b1100110100000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: CALL
          16'b1100110100000101: begin
            state <= {state[15:8], 8'h06};
            z <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: CALL
          16'b1100110100000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: CALL
          16'b1100110100000111: begin
            state <= {state[15:8], 8'h08};
            pc <= pc + 1;
          end
          // M3 T1: CALL
          16'b1100110100001000: begin
            state <= {state[15:8], 8'h09};
            addr <= pc;
            rd_enable <= 1;
          end
          // M3 T2: CALL
          16'b1100110100001001: begin
            state <= {state[15:8], 8'h0a};
            w <= data_in;
            rd_enable <= 0;
          end
          // M3 T3: CALL
          16'b1100110100001010: begin
            state <= {state[15:8], 8'h0b};
          end
          // M3 T4: CALL
          16'b1100110100001011: begin
            state <= {state[15:8], 8'h0c};
            pc <= pc + 1;
          end
          // M1 T3: CALL cc
          16'b110xx10000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: CALL cc
          16'b110xx10000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: CALL cc
          16'b110xx10000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: CALL cc
          16'b110xx10000000101: begin
            state <= {state[15:8], 8'h06};
            z <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: CALL cc
          16'b110xx10000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: CALL cc
          16'b110xx10000000111: begin
            state <= {state[15:8], 8'h08};
            pc <= pc + 1;
          end
          // M3 T1: CALL cc
          16'b110xx10000001000: begin
            state <= {state[15:8], 8'h09};
            addr <= pc;
            rd_enable <= 1;
          end
          // M3 T2: CALL cc
          16'b110xx10000001001: begin
            state <= {state[15:8], 8'h0a};
            w <= data_in;
            rd_enable <= 0;
          end
          // M3 T3: CALL cc
          16'b110xx10000001010: begin
            state <= {state[15:8], 8'h0b};
          end
          // M3 T4: CALL cc
          16'b110xx10000001011: begin
            state <= {state[15:8], 8'h0c};
            pc <= pc + 1;
            case (state[12:11])
              2'b00: begin
                state <= {8'b11001101, zf ? 8'h00 : 8'h0c};
              end
              2'b01: begin
                state <= {8'b11001101, zf ? 8'h0c : 8'h00};
              end
              2'b10: begin
                state <= {8'b11001101, cf ? 8'h00 : 8'h0c};
              end
              2'b11: begin
                state <= {8'b11001101, cf ? 8'h0c : 8'h00};
              end
            endcase
          end
          // M4 T1: CALL + CALL cc
          16'b1100110100001100: begin
            state <= {state[15:8], 8'h0d};
          end
          // M4 T2: CALL + CALL cc
          16'b1100110100001101: begin
            state <= {state[15:8], 8'h0e};
          end
          // M4 T3: CALL + CALL cc
          16'b1100110100001110: begin
            state <= {state[15:8], 8'h0f};
            sp <= sp - 1;
          end
          // M4 T4: CALL + CALL cc
          16'b1100110100001111: begin
            state <= {state[15:8], 8'h10};
          end
          // M5 T1: CALL + CALL cc
          16'b1100110100010000: begin
            state <= {state[15:8], 8'h11};
            addr <= sp;
            data_out <= pc[15:8];
            wr_enable <= 1;
          end
          // M5 T2: CALL + CALL cc
          16'b1100110100010001: begin
            state <= {state[15:8], 8'h12};
            wr_enable <= 0;
          end
          // M5 T3: CALL + CALL cc
          16'b1100110100010010: begin
            state <= {state[15:8], 8'h13};
            sp <= sp - 1;
          end
          // M5 T4: CALL + CALL cc
          16'b1100110100010011: begin
            state <= {state[15:8], 8'h14};
            pc <= {w, pc[7:0]};
          end
          // M6 T1: CALL + CALL cc
          16'b1100110100010100: begin
            state <= {state[15:8], 8'h15};
            addr <= sp;
            data_out <= pc[7:0];
            wr_enable <= 1;
          end
          // M6 T2: CALL + CALL cc
          16'b1100110100010101: begin
            state <= {state[15:8], 8'h16};
            wr_enable <= 0;
          end
          // M6 T3: CALL + CALL cc
          16'b1100110100010110: begin
            state <= {state[15:8], 8'h17};
          end
          // M6 T4: CALL + CALL cc
          16'b1100110100010111: begin
            state <= {state[15:8], 8'h00};
            pc <= {pc[15:8], z};
          end

          // M1 T3: PUSH r16
          16'b11xx010100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: PUSH r16
          16'b11xx010100000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: PUSH r16
          16'b11xx010100000100: begin
            state <= {state[15:8], 8'h05};
          end
          // M2 T2: PUSH r16
          16'b11xx010100000101: begin
            state <= {state[15:8], 8'h06};
          end
          // M2 T3: PUSH r16
          16'b11xx010100000110: begin
            state <= {state[15:8], 8'h07};
            sp <= sp - 1;
          end
          // M2 T4: PUSH r16
          16'b11xx010100000111: begin
            state <= {state[15:8], 8'h08};
          end
          // M3 T1: PUSH r16
          16'b11xx010100001000: begin
            state <= {state[15:8], 8'h09};
            addr <= sp;
            case (state[13:12])
              2'b00: data_out <= b;
              2'b01: data_out <= d;
              2'b10: data_out <= h;
              2'b11: data_out <= a;
            endcase
            wr_enable <= 1;
          end
          // M3 T2: PUSH r16
          16'b11xx010100001001: begin
            state <= {state[15:8], 8'h0a};
            wr_enable <= 0;
          end
          // M3 T3: PUSH r16
          16'b11xx010100001010: begin
            state <= {state[15:8], 8'h0b};
            sp <= sp - 1;
          end
          // M3 T4: PUSH r16
          16'b11xx010100001011: begin
            state <= {state[15:8], 8'h0c};
          end
          // M4 T1: PUSH r16
          16'b11xx010100001100: begin
            state <= {state[15:8], 8'h0d};
            addr <= sp;
            case (state[13:12])
              2'b00: data_out <= c;
              2'b01: data_out <= e;
              2'b10: data_out <= l;
              2'b11: data_out <= { zf, nf, hf, cf, 4'h0 };
            endcase
            wr_enable <= 1;
          end
          // M4 T2: PUSH r16
          16'b11xx010100001101: begin
            state <= {state[15:8], 8'h0e};
            wr_enable <= 0;
          end
          // M4 T3: PUSH r16
          16'b11xx010100001110: begin
            state <= {state[15:8], 8'h0f};
          end
          // M4 T4: PUSH r16
          16'b11xx010100001111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: POP r16
          16'b11xx000100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: POP r16
          16'b11xx000100000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: POP r16
          16'b11xx000100000100: begin
            state <= {state[15:8], 8'h05};
            addr <= sp;
            rd_enable <= 1;
          end
          // M2 T2: POP r16
          16'b11xx000100000101: begin
            state <= {state[15:8], 8'h06};
            rd_enable <= 0;
            case (state[13:12])
              2'b00: c <= data_in;
              2'b01: e <= data_in;
              2'b10: l <= data_in;
              2'b11: { zf, nf, hf, cf } <= data_in[7:4];
            endcase
          end
          // M2 T3: POP r16
          16'b11xx000100000110: begin
            state <= {state[15:8], 8'h07};
            sp <= sp + 1;
          end
          // M2 T4: POP r16
          16'b11xx000100000111: begin
            state <= {state[15:8], 8'h08};
          end
          // M3 T1: POP r16
          16'b11xx000100001000: begin
            state <= {state[15:8], 8'h09};
            addr <= sp;
            rd_enable <= 1;
          end
          // M3 T2: POP r16
          16'b11xx000100001001: begin
            state <= {state[15:8], 8'h0a};
            rd_enable <= 0;
            case (state[13:12])
              2'b00: b <= data_in;
              2'b01: d <= data_in;
              2'b10: h <= data_in;
              2'b11: a <= data_in;
            endcase
          end
          // M3 T3: POP r16
          16'b11xx000100001010: begin
            state <= {state[15:8], 8'h0b};
            sp <= sp + 1;
          end
          // M3 T4: POP r16
          16'b11xx000100001011: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: INC/DEC r16
          16'b00xxx01100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: INC/DEC r16
          16'b00xxx01100000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: INC/DEC r16
          16'b00xxx01100000100: begin
            state <= {state[15:8], 8'h05};
          end
          // M2 T2: INC/DEC r16
          16'b00xxx01100000101: begin
            state <= {state[15:8], 8'h06};
          end
          // M2 T3: INC/DEC r16
          16'b00xxx01100000110: begin
            state <= {state[15:8], 8'h07};
            if (state[11]) begin
              case (state[13:12])
                2'b00: {b, c} <= {b, c} - 1;
                2'b01: {d, e} <= {d, e} - 1;
                2'b10: {h, l} <= {h, l} - 1;
                2'b11: sp <= sp - 1;
              endcase
            end else begin
              case (state[13:12])
                2'b00: {b, c} <= {b, c} + 1;
                2'b01: {d, e} <= {d, e} + 1;
                2'b10: {h, l} <= {h, l} + 1;
                2'b11: sp <= sp + 1;
              endcase
            end
          end
          // M2 T4: INC/DEC r16
          16'b00xxx01100000111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: RET cc
          16'b110xx00000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: RET cc
          16'b110xx00000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: RET cc
          16'b110xx00000000100: begin
            state <= {state[15:8], 8'h05};
          end
          // M2 T2: RET cc
          16'b110xx00000000101: begin
            state <= {state[15:8], 8'h06};
          end
          // M2 T4: RET cc
          16'b110xx00000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: RET cc
          16'b110xx00000000111: begin
            case (state[12:11])
              2'b00: begin
                state <= {8'b11001001, zf ? 8'h00 : 8'h04};
              end
              2'b01: begin
                state <= {8'b11001001, zf ? 8'h04 : 8'h00};
              end
              2'b10: begin
                state <= {8'b11001001, cf ? 8'h00 : 8'h04};
              end
              2'b11: begin
                state <= {8'b11001001, cf ? 8'h04 : 8'h00};
              end
            endcase
          end

          // M1 T3: RET
          16'b110x100100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: RET
          16'b110x100100000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: RET
          16'b110x100100000100: begin
            state <= {state[15:8], 8'h05};
            addr <= sp;
            rd_enable <= 1;
          end
          // M2 T2: RET
          16'b110x100100000101: begin
            state <= {state[15:8], 8'h06};
            z <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: RET
          16'b110x100100000110: begin
            state <= {state[15:8], 8'h07};
            sp <= sp + 1;
          end
          // M2 T4: RET
          16'b110x100100000111: begin
            state <= {state[15:8], 8'h08};
            ime <= state[12] ? 1'b1 : ime;
          end
          // M3 T1: RET
          16'b110x100100001000: begin
            state <= {state[15:8], 8'h09};
            addr <= sp;
            rd_enable <= 1;
          end
          // M3 T2: RET
          16'b110x100100001001: begin
            state <= {state[15:8], 8'h0a};
            w <= data_in;
            rd_enable <= 0;
          end
          // M3 T3: RET
          16'b110x100100001010: begin
            state <= {state[15:8], 8'h0b};
            sp <= sp + 1;
          end
          // M3 T4: RET
          16'b110x100100001011: begin
            state <= {state[15:8], 8'h0c};
          end
          // M4 T1: RET
          16'b110x100100001100: begin
            state <= {state[15:8], 8'h0d};
          end
          // M4 T2: RET
          16'b110x100100001101: begin
            state <= {state[15:8], 8'h0e};
          end
          // M4 T3: RET
          16'b110x100100001110: begin
            state <= {state[15:8], 8'h0f};
            pc <= {w, z};
          end
          // M4 T4: RET
          16'b110x100100001111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: ALU d8
          16'b11xxx11000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: ALU d8
          16'b11xxx11000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: ALU d8
          16'b11xxx11000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: ALU d8
          16'b11xxx11000000101: begin
            state <= {state[15:8], 8'h06};
            alu_rhs <= data_in;
            rd_enable <= 0;
            alu_lhs <= a;
            alu_op <= state[13:11];
          end
          // M2 T3: ALU d8
          16'b11xxx11000000110: begin
            state <= {state[15:8], 8'h07};
            a <= alu_r;
            zf <= alu_zf;
            nf <= alu_nf;
            hf <= alu_hf;
            cf <= alu_cf;
          end
          // M2 T4: ALU d8
          16'b11xxx11000000111: begin
            state <= {state[15:8], 8'h00};
            pc <= pc + 1;
          end

          // M1 T3: LD (d16), a
          16'b1110101000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T3: LD (d16), a
          16'b1110101000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LD (d16), a
          16'b1110101000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: LD (d16), a
          16'b1110101000000101: begin
            state <= {state[15:8], 8'h06};
            z <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: LD (d16), a
          16'b1110101000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LD (d16), a
          16'b1110101000000111: begin
            state <= {state[15:8], 8'h08};
            pc <= pc + 1;
          end
          // M3 T1: LD (d16), a
          16'b1110101000001000: begin
            state <= {state[15:8], 8'h09};
            addr <= pc;
            rd_enable <= 1;
          end
          // M3 T2: LD (d16), a
          16'b1110101000001001: begin
            state <= {state[15:8], 8'h0a};
            w <= data_in;
            rd_enable <= 0;
          end
          // M3 T3: LD (d16), a
          16'b1110101000001010: begin
            state <= {state[15:8], 8'h0b};
          end
          // M3 T4: LD (d16), a
          16'b1110101000001011: begin
            state <= {state[15:8], 8'h0c};
            pc <= pc + 1;
          end
          // M4 T1: LD (d16), a
          16'b1110101000001100: begin
            state <= {state[15:8], 8'h0d};
            addr <= {w, z};
            wr_enable <= 1;
            data_out <= a;
          end
          // M4 T2: LD (d16), a
          16'b1110101000001101: begin
            state <= {state[15:8], 8'h0e};
            wr_enable <= 0;
          end
          // M4 T3: LD (d16), a
          16'b1110101000001110: begin
            state <= {state[15:8], 8'h0f};
          end
          // M4 T4: LD (d16), a
          16'b1110101000001111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: LHD A, (d8)
          16'b1111000000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: LHD A, (d8)
          16'b1111000000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LHD A, (d8)
          16'b1111000000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: LHD A, (d8)
          16'b1111000000000101: begin
            state <= {state[15:8], 8'h06};
            z <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: LHD A, (d8)
          16'b1111000000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LHD A, (d8)
          16'b1111000000000111: begin
            state <= {state[15:8], 8'h08};
            pc <= pc + 1;
          end
          // M3 T1: LHD A, (d8)
          16'b1111000000001000: begin
            state <= {state[15:8], 8'h09};
            addr <= {8'hff, z};
            rd_enable <= 1;
          end
          // M3 T2: LHD A, (d8)
          16'b1111000000001001: begin
            state <= {state[15:8], 8'h0a};
            a <= data_in;
            rd_enable <= 0;
          end
          // M3 T3: LHD A, (d8)
          16'b1111000000001010: begin
            state <= {state[15:8], 8'h0b};
          end
          // M3 T4: LHD A, (d8)
          16'b1111000000001011: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: NOP
          16'b0000000000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: NOP
          16'b0000000000000011: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3:  JP
          16'b1100001100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4:  JP
          16'b1100001100000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1:  JP
          16'b1100001100000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2:  JP
          16'b1100001100000101: begin
            state <= {state[15:8], 8'h06};
            z <= data_in;
            rd_enable <= 0;
          end
          // M2 T3:  JP
          16'b1100001100000110: begin
            state <= {state[15:8], 8'h07};
            pc <= pc + 1;
          end
          // M2 T4:  JP
          16'b1100001100000111: begin
            state <= {state[15:8], 8'h08};
            addr <= pc;
            rd_enable <= 1;
          end
          // M3 T1:  JP
          16'b1100001100001000: begin
            state <= {state[15:8], 8'h09};
            w <= data_in;
            rd_enable <= 0;
          end
          // M3 T2:  JP
          16'b1100001100001001: begin
            state <= {state[15:8], 8'h0a};
          end
          // M3 T3:  JP
          16'b1100001100001010: begin
            state <= {state[15:8], 8'h0b};
            pc <= pc + 1;
          end
          // M3 T4:  JP
          16'b1100001100001011: begin
            state <= {state[15:8], 8'h0c};
          end
          // M4 T1:  JP
          16'b1100001100001100: begin
            state <= {state[15:8], 8'h0d};
          end
          // M4 T2:  JP
          16'b1100001100001101: begin
            state <= {state[15:8], 8'h0e};
          end
          // M4 T3:  JP
          16'b1100001100001110: begin
            state <= {state[15:8], 8'h0f};
            pc <= {w, z};
          end
          // M4 T4:  JP
          16'b1100001100001111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3:  JP cc
          16'b110xx01000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4:  JP cc
          16'b110xx01000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1:  JP cc
          16'b110xx01000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2:  JP cc
          16'b110xx01000000101: begin
            state <= {state[15:8], 8'h06};
            z <= data_in;
            rd_enable <= 0;
          end
          // M2 T3:  JP cc
          16'b110xx01000000110: begin
            state <= {state[15:8], 8'h07};
            pc <= pc + 1;
          end
          // M2 T4:  JP cc
          16'b110xx01000000111: begin
            state <= {state[15:8], 8'h08};
            addr <= pc;
            rd_enable <= 1;
          end
          // M3 T1:  JP cc
          16'b110xx01000001000: begin
            state <= {state[15:8], 8'h09};
            w <= data_in;
            rd_enable <= 0;
          end
          // M3 T2:  JP cc
          16'b110xx01000001001: begin
            state <= {state[15:8], 8'h0a};
          end
          // M3 T3:  JP cc
          16'b110xx01000001010: begin
            state <= {state[15:8], 8'h0b};
            pc <= pc + 1;
          end
          // M3 T4:  JP cc
          16'b110xx01000001011: begin
            case (state[12:11])
              2'b00: begin
                state <= {state[15:8], zf ? 8'h00 : 8'h0c};
              end
              2'b01: begin
                state <= {state[15:8], zf ? 8'h0c : 8'h00};
              end
              2'b10: begin
                state <= {state[15:8], cf ? 8'h00 : 8'h0c};
              end
              2'b11: begin
                state <= {state[15:8], cf ? 8'h0c : 8'h00};
              end
            endcase
          end
          // M4 T1:  JP cc
          16'b110xx01000001100: begin
            state <= {state[15:8], 8'h0d};
          end
          // M4 T2:  JP cc
          16'b110xx01000001101: begin
            state <= {state[15:8], 8'h0e};
          end
          // M4 T3:  JP cc
          16'b110xx01000001110: begin
            state <= {state[15:8], 8'h0f};
            pc <= {w, z};
          end
          // M4 T4:  JP cc
          16'b110xx01000001111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3:  LD (r16), A
          16'b000x001000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4:  LD (r16), A
          16'b000x001000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1:  LD (r16), A
          16'b000x001000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= state[12] ? {d, e} : {b, c};
            data_out <= a;
            wr_enable <= 1;
          end
          // M2 T2:  LD (r16), A
          16'b000x001000000101: begin
            state <= {state[15:8], 8'h06};
            wr_enable <= 0;
          end
          // M2 T3:  LD (r16), A
          16'b000x001000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4:  LD (r16), A
          16'b000x001000000111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3:  DI/EI
          16'b1111x01100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4:  DI/EI
          16'b1111x01100000011: begin
            state <= {state[15:8], 8'h00};
            ime <= state[11];
          end

          // M1 T3: LD A, (d16)
          16'b1111101000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: LD A, (d16)
          16'b1111101000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LD A, (d16)
          16'b1111101000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: LD A, (d16)
          16'b1111101000000101: begin
            state <= {state[15:8], 8'h06};
            z <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: LD A, (d16)
          16'b1111101000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LD A, (d16)
          16'b1111101000000111: begin
            state <= {state[15:8], 8'h08};
            pc <= pc + 1;
          end
          // M3 T1: LD A, (d16)
          16'b1111101000001000: begin
            state <= {state[15:8], 8'h09};
            addr <= pc;
            rd_enable <= 1;
          end
          // M3 T2: LD A, (d16)
          16'b1111101000001001: begin
            state <= {state[15:8], 8'h0a};
            w <= data_in;
            rd_enable <= 0;
          end
          // M3 T3: LD A, (d16)
          16'b1111101000001010: begin
            state <= {state[15:8], 8'h0b};
          end
          // M3 T4: LD A, (d16)
          16'b1111101000001011: begin
            state <= {state[15:8], 8'h0c};
            pc <= pc + 1;
          end
          // M4 T1: LD A, (d16)
          16'b1111101000001100: begin
            state <= {state[15:8], 8'h0d};
            addr <= {w, z};
            rd_enable <= 1;
          end
          // M4 T2: LD A, (d16)
          16'b1111101000001101: begin
            state <= {state[15:8], 8'h0e};
            a <= data_in;
            rd_enable <= 0;
          end
          // M4 T3: LD A, (d16)
          16'b1111101000001110: begin
            state <= {state[15:8], 8'h0f};
          end
          // M4 T4: LD A, (d16)
          16'b1111101000001111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: LD (d16), SP
          16'b0000100000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: LD (d16), SP
          16'b0000100000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LD (d16), SP
          16'b0000100000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: LD (d16), SP
          16'b0000100000000101: begin
            state <= {state[15:8], 8'h06};
            z <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: LD (d16), SP
          16'b0000100000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LD (d16), SP
          16'b0000100000000111: begin
            state <= {state[15:8], 8'h08};
            pc <= pc + 1;
          end
          // M3 T1: LD (d16), SP
          16'b0000100000001000: begin
            state <= {state[15:8], 8'h09};
            addr <= pc;
            rd_enable <= 1;
          end
          // M3 T2: LD (d16), SP
          16'b0000100000001001: begin
            state <= {state[15:8], 8'h0a};
            w <= data_in;
            rd_enable <= 0;
          end
          // M3 T3: LD (d16), SP
          16'b0000100000001010: begin
            state <= {state[15:8], 8'h0b};
          end
          // M3 T4: LD (d16), SP
          16'b0000100000001011: begin
            state <= {state[15:8], 8'h0c};
            pc <= pc + 1;
          end
          // M4 T1: LD (d16), SP
          16'b0000100000001100: begin
            state <= {state[15:8], 8'h0d};
            addr <= {w, z};
            data_out <= sp[7:0];
            wr_enable <= 1;
          end
          // M4 T2: LD (d16), SP
          16'b0000100000001101: begin
            state <= {state[15:8], 8'h0e};
            wr_enable <= 0;
          end
          // M4 T3: LD (d16), SP
          16'b0000100000001110: begin
            state <= {state[15:8], 8'h0f};
            {w, z} <= {w, z} + 16'd1;
          end
          // M4 T4: LD (d16), SP
          16'b0000100000001111: begin
            state <= {state[15:8], 8'h10};
          end
          // M5 T1: LD (d16), SP
          16'b0000100000010000: begin
            state <= {state[15:8], 8'h11};
            addr <= {w, z};
            data_out <= sp[15:8];
            wr_enable <= 1;
          end
          // M5 T2: LD (d16), SP
          16'b0000100000010001: begin
            state <= {state[15:8], 8'h12};
            wr_enable <= 0;
          end
          // M5 T3: LD (d16), SP
          16'b0000100000010010: begin
            state <= {state[15:8], 8'h13};
          end
          // M5 T4: LD (d16), SP
          16'b0000100000010011: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: ADD HL, r16
          16'b00xx100100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
            alu_lhs <= l;
          end
          // M1 T4: ADD HL, r16
          16'b00xx100100000011: begin
            state <= {state[15:8], 8'h04};
            case (state[13:12])
              2'b00: alu_rhs <= c;
              2'b01: alu_rhs <= e;
              2'b10: alu_rhs <= l;
              2'b11: alu_rhs <= sp[7:0];
            endcase
            alu_op <= 4'b0000;
          end
          // M2 T1: ADD HL, r16
          16'b00xx100100000100: begin
            state <= {state[15:8], 8'h05};
            l <= alu_r;
            nf <= alu_nf;
            hf <= alu_hf;
            cf <= alu_cf;
          end
          // M2 T2: ADD HL, r16
          16'b00xx100100000101: begin
            state <= {state[15:8], 8'h06};
            alu_lhs <= h;
          end
          // M2 T3: ADD HL, r16
          16'b00xx100100000110: begin
            state <= {state[15:8], 8'h07};
            case (state[13:12])
              2'b00: alu_rhs <= b;
              2'b01: alu_rhs <= d;
              2'b10: alu_rhs <= h;
              2'b11: alu_rhs <= sp[15:8];
            endcase
            alu_op <= 4'b0001;
          end
          // M2 T4: ADD HL, r16
          16'b00xx100100000111: begin
            state <= {state[15:8], 8'h00};
            h <= alu_r;
            nf <= alu_nf;
            hf <= alu_hf;
            cf <= alu_cf;
          end

          // M1 T3: JP HL
          16'b1110100100000010: begin
            state <= {state[15:8], 8'h03};
          end
          // M1 T4: JP HL
          16'b1110100100000011: begin
            state <= {state[15:8], 8'h00};
            pc <= {h, l};
          end

          // M1 T3: LD SP, HL
          16'b1111100100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: LD SP, HL
          16'b1111100100000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LD SP, HL
          16'b1111100100000100: begin
            state <= {state[15:8], 8'h05};
          end
          // M2 T2: LD SP, HL
          16'b1111100100000101: begin
            state <= {state[15:8], 8'h06};
          end
          // M2 T3: LD SP, HL
          16'b1111100100000110: begin
            state <= {state[15:8], 8'h07};
            sp <= {h, l};
          end
          // M2 T4: LD SP, HL
          16'b1111100100000111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: RST
          16'b11xxx11100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: RST
          16'b11xxx11100000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: RST
          16'b11xxx11100000100: begin
            state <= {state[15:8], 8'h05};
          end
          // M2 T2: RST
          16'b11xxx11100000101: begin
            state <= {state[15:8], 8'h06};
          end
          // M2 T3: RST
          16'b11xxx11100000110: begin
            state <= {state[15:8], 8'h07};
            sp <= sp - 1;
          end
          // M2 T4: RST
          16'b11xxx11100000111: begin
            state <= {state[15:8], 8'h08};
          end
          // M3 T1: RST
          16'b11xxx11100001000: begin
            state <= {state[15:8], 8'h09};
            addr <= sp;
            data_out <= pc[15:8];
            wr_enable <= 1;
          end
          // M3 T2: RST
          16'b11xxx11100001001: begin
            state <= {state[15:8], 8'h0a};
            wr_enable <= 0;
          end
          // M3 T3: RST
          16'b11xxx11100001010: begin
            state <= {state[15:8], 8'h0b};
            sp <= sp - 1;
          end
          // M3 T4: RST
          16'b11xxx11100001011: begin
            state <= {state[15:8], 8'h0c};
            pc <= {8'h00, pc[7:0]};
          end
          // M4 T1: RST
          16'b11xxx11100001100: begin
            state <= {state[15:8], 8'h0d};
            addr <= sp;
            data_out <= pc[7:0];
            wr_enable <= 1;
          end
          // M4 T2: RST
          16'b11xxx11100001101: begin
            state <= {state[15:8], 8'h0e};
            wr_enable <= 0;
          end
          // M4 T3: RST
          16'b11xxx11100001110: begin
            state <= {state[15:8], 8'h0f};
            pc <= {8'h00, 2'b00, state[13:11], 3'b000};
          end
          // M4 T4: RST
          16'b11xxx11100001111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: CPL
          16'b0010111100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: CPL
          16'b0010111100000011: begin
            state <= {state[15:8], 8'h00};
            a <= ~a;
            nf <= 1;
            hf <= 1;
          end

          // M1 T3: CCF
          16'b0011111100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: CCF
          16'b0011111100000011: begin
            state <= {state[15:8], 8'h00};
            nf <= 0;
            hf <= 0;
            cf <= !cf;
          end

          // M1 T3: SCF
          16'b0011011100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: SCF
          16'b0011011100000011: begin
            state <= {state[15:8], 8'h00};
            nf <= 0;
            hf <= 0;
            cf <= 1;
          end

          // M1 T3: ADD SP, r8
          16'b1110100000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: ADD SP, r8
          16'b1110100000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: ADD SP, r8
          16'b1110100000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: ADD SP, r8
          16'b1110100000000101: begin
            state <= {state[15:8], 8'h06};
            rd_enable <= 0;
            alu_lhs <= sp[7:0];
            alu_rhs <= data_in;
            alu_op <= 4'b0000;
          end
          // M2 T3: ADD SP, r8
          16'b1110100000000110: begin
            state <= {state[15:8], 8'h07};
            pc <= pc + 1;
          end
          // M2 T4: ADD SP, r8
          16'b1110100000000111: begin
            state <= {state[15:8], 8'h08};
            sp <= {sp[15:8], alu_r};
            zf <= 0;
            nf <= 0;
            hf <= alu_hf;
            cf <= alu_cf;
          end
          // M3 T1: ADD SP, r8
          16'b1110100000001000: begin
            state <= {state[15:8], 8'h09};
            alu_lhs <= sp[15:8];
            alu_rhs <= alu_rhs[7] ? 8'hff : 8'h00;
            alu_op <= 4'b0001;
          end
          // M3 T2: ADD SP, r8
          16'b1110100000001001: begin
            state <= {state[15:8], 8'h0a};
          end
          // M3 T3: ADD SP, r8
          16'b1110100000001010: begin
            state <= {state[15:8], 8'h0b};
          end
          // M3 T4: ADD SP, r8
          16'b1110100000001011: begin
            state <= {state[15:8], 8'h0c};
            sp <= {alu_r, sp[7:0]};
          end
          // M4 T1: ADD SP, r8
          16'b1110100000001100: begin
            state <= {state[15:8], 8'h0d};
          end
          // M4 T2: ADD SP, r8
          16'b1110100000001101: begin
            state <= {state[15:8], 8'h0e};
          end
          // M4 T3: ADD SP, r8
          16'b1110100000001110: begin
            state <= {state[15:8], 8'h0f};
          end
          // M4 T4: ADD SP, r8
          16'b1110100000001111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: ADD HL, SP+r8
          16'b1111100000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: ADD HL, SP+r8
          16'b1111100000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: ADD HL, SP+r8
          16'b1111100000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: ADD HL, SP+r8
          16'b1111100000000101: begin
            state <= {state[15:8], 8'h06};
            rd_enable <= 0;
            alu_lhs <= sp[7:0];
            alu_rhs <= data_in;
            alu_op <= 4'b0000;
          end
          // M2 T3: ADD HL, SP+r8
          16'b1111100000000110: begin
            state <= {state[15:8], 8'h07};
            pc <= pc + 1;
          end
          // M2 T4: ADD HL, SP+r8
          16'b1111100000000111: begin
            state <= {state[15:8], 8'h08};
            l <= alu_r;
            zf <= 0;
            nf <= 0;
            hf <= alu_hf;
            cf <= alu_cf;
          end
          // M3 T1: ADD HL, SP+r8
          16'b1111100000001000: begin
            state <= {state[15:8], 8'h09};
            alu_lhs <= sp[15:8];
            alu_rhs <= alu_rhs[7] ? 8'hff : 8'h00;
            alu_op <= 4'b0001;
          end
          // M3 T2: ADD HL, SP+r8
          16'b1111100000001001: begin
            state <= {state[15:8], 8'h0a};
          end
          // M3 T3: ADD HL, SP+r8
          16'b1111100000001010: begin
            state <= {state[15:8], 8'h0b};
          end
          // M3 T4: ADD HL, SP+r8
          16'b1111100000001011: begin
            state <= {state[15:8], 8'h00};
            h <= alu_r;
          end

          // M3 T3: DAA
          16'b0010011100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
            if (nf) begin
              if (cf && hf) begin
                a <= a + 8'h9a;
              end else if (cf) begin
                a <= a + 8'ha0;
              end else if (hf) begin
                a <= a + 8'hfa;
              end
            end else begin
              if (cf || a > 8'h99) begin
                cf <= 1;
                if (hf || a[3:0] > 4'h9)
                  a <= a + 8'h66;
                else
                  a <= a + 8'h60;
              end else begin
                if (hf || a[3:0] > 8'h09) begin
                  a <= a + 8'h06;
                end
              end
            end
          end
          // M3 T4: DAA
          16'b0010011100000011: begin
            state <= {state[15:8], 8'h00};
            zf <= a == 0;
            hf <= 0;
          end
        endcase
      end
    end
  end
endmodule
