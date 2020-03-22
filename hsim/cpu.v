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
  , input[7:0]        int_pending
  , input[7:0]        int_enable
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
    end else begin
      if (halted) begin
        $display("CPU halted, not implemented");
        $finish;
      end else if (|(int_pending & int_enable)) begin
        $display("Interrupt, not implemented");
        $finish;
      end else begin
        casez (state)
          // M1 T1
          16'b????????00000000: begin
            addr <= pc;
            rd_enable <= 1;
            state <= 16'hxx01;
          end
          // M1 T2
          16'b????????00000001: begin
            state <= {data_in, 8'h02};
            rd_enable <= 0;
          end

          // M1 T3: LD r16, d16
          16'b00??000100000010: begin
            state <= {state[15:8], 8'h3};
            pc <= pc + 1;
          end
          // M1 T4: LD r16, d16
          16'b00??000100000011: begin
            state <= {state[15:8], 8'h4};
          end
          // M2 T1: LD r16, d16
          16'b00??000100000100: begin
            state <= {state[15:8], 8'h5};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: LD r16, d16
          16'b00??000100000101: begin
            state <= {state[15:8], 8'h6};
          end
          // M2 T3: LD r16, d16
          16'b00??000100000110: begin
            state <= {state[15:8], 8'h7};
          end
          // M2 T4: LD r16, d16
          16'b00??000100000111: begin
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
          16'b00??000100001000: begin
            addr <= pc;
            rd_enable <= 1;
            state <= {state[15:8], 8'h9};
          end
          // M3 T2: LD r16, d16
          16'b00??000100001001: begin
            state <= {state[15:8], 8'ha};
          end
          // M3 T3: LD r16, d16
          16'b00??000100001010: begin
            state <= {state[15:8], 8'hb};
          end
          // M3 T3: LD r16, d16
          16'b00??000100001011: begin
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

          // M1 T3: LDI/LDD r8
          16'b001?001000000010: begin
            state <= {state[15:8], 8'h3};
            pc <= pc + 1;
          end
          // M1 T4: LDI/LDD r8
          16'b001?001000000011: begin
            state <= {state[15:8], 8'h4};
          end
          // M2 T1: LDI/LDD r8
          16'b001?001000000100: begin
            state <= {state[15:8], 8'h5};
            addr <= {h, l};
            data_out <= a;
            wr_enable <= 1;
          end
          // M2 T2: LDI/LDD r8
          16'b001?001000000101: begin
            state <= {state[15:8], 8'h6};
            wr_enable <= 0;
          end
          // M2 T3: LDI/LDD r8
          16'b001?001000000110: begin
            state <= {state[15:8], 8'h7};
          end
          // M2 T4: LDI/LDD r8
          16'b001?001000000111: begin
            state <= {state[15:8], 8'h0};
            case (state[12])
              1'b0: {h, l} <= {h, l} + 1;
              1'b1: {h, l} <= {h, l} - 1;
            endcase
          end

          // M1 T3: ALU A, r8
          16'b10???11000000010: begin
            state <= {state[15:8], 8'h3};
            pc <= pc + 1;
          end
          16'b10??????00000010: begin
            state <= {state[15:8], 8'h3};
            pc <= pc + 1;
            alu_lhs <= a;
            alu_op <= {1'b0, state[13:11]};
            case (state[10:8])
              3'b000: alu_rhs <= b;
              3'b001: alu_rhs <= c;
              3'b010: alu_rhs <= d;
              3'b011: alu_rhs <= e;
              3'b100: alu_rhs <= h;
              3'b101: alu_rhs <= l;
              3'b110: $finish;
              3'b111: alu_rhs <= a;
            endcase;
          end
          // M1 T4: ALU A, r8
          16'b10???11000000011: begin
            state <= {state[15:8], 8'h4};
            pc <= pc + 1;
          end
          16'b10??????00000011: begin
            state <= {state[15:8], 8'h0};
            a <= alu_r;
            zf <= alu_zf;
            nf <= alu_nf;
            hf <= alu_hf;
            cf <= alu_cf;
          end

          // M2 T1: ALU A, r8
          16'b10???11000000100: begin
            state <= {state[15:8], 8'h5};
            addr <= {h, l};
            rd_enable <= 1;
          end
          // M2 T2: ALU A, r8
          16'b10???11000000101: begin
            state <= {state[15:8], 8'h6};
            alu_rhs <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: ALU A, r8
          16'b10???11000000110: begin
            state <= {state[15:8], 8'h7};
            alu_lhs <= a;
            alu_op <= state[13:11];
          end
          // M2 T4: ALU A, r8
          16'b10???11000000111: begin
            state <= {state[15:8], 8'h0};
            a <= alu_r;
            zf <= alu_zf;
            nf <= alu_nf;
            hf <= alu_hf;
            cf <= alu_cf;
          end

          // M1 T3: LDI/LDD (HL), A
          16'b001?001000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: LDI/LDD (HL), A
          16'b001?001000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LDI/LDD (HL), A
          16'b001?001000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= {h, l};
            wr_enable <= 1;
            data_out <= a;
          end
          // M2 T2: LDI/LDD (HL), A
          16'b001?001000000101: begin
            state <= {state[15:8], 8'h06};
            wr_enable <= 0;
          end
          // M2 T3: LDI/LDD (HL), A
          16'b001?001000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LDI/LDD (HL), A
          16'b001?001000000111: begin
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

          // M2 T3: CB BIT
          16'b01??????10000110: begin
            state <= {state[15:8], 8'h87};
            pc <= pc + 1;
          end
          // M2 T4: CB BIT
          16'b01???11010000111: begin
            $display("CB WTF");
            $finish;
          end
          16'b01??????10000111: begin
            state <= {state[15:8], 8'h00};
            case (state[10:8])
              3'b000: zf = !b[state[13:11]];
              3'b001: zf = !c[state[13:11]];
              3'b010: zf = !d[state[13:11]];
              3'b011: zf = !e[state[13:11]];
              3'b100: zf = !h[state[13:11]];
              3'b101: zf = !l[state[13:11]];
              3'b110: $finish;
              3'b111: zf = !a[state[13:11]];
            endcase
            nf <= 0;
            hf <= 1;
          end

          // M2 T3: CB alu
          16'b00???11010000110: begin
            $display("CB alu WTF");
          end
          16'b00??????10000110: begin
            state <= {state[15:8], 8'h87};
            pc <= pc + 1;
            alu_lhs <= 8'h00;
            case (state[10:8])
              3'b000: alu_rhs <= b;
              3'b001: alu_rhs <= c;
              3'b010: alu_rhs <= d;
              3'b011: alu_rhs <= e;
              3'b100: alu_rhs <= h;
              3'b101: alu_rhs <= l;
              3'b110: $finish;
              3'b111: alu_rhs <= a;
            endcase;
            alu_op <= {1'b1, state[13:11]};
          end
          // M2 T4: CB alu
          16'b00???11010000111: begin
            $display("CB alu WTF2");
          end
          16'b00??????10000111: begin
            state <= {state[15:8], 8'h00};
            case (state[10:8])
              3'b000: b <= alu_r;
              3'b001: c <= alu_r;
              3'b010: d <= alu_r;
              3'b011: e <= alu_r;
              3'b100: h <= alu_r;
              3'b101: l <= alu_r;
              3'b110: $finish;
              3'b111: a <= alu_r;
            endcase;
            zf <= alu_zf;
            nf <= alu_nf;
            hf <= alu_hf;
            cf <= alu_cf;
          end

          // M2 T3: RLCA
          16'b0000011100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
            alu_lhs <= 0;
            alu_rhs <= a;
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
            alu_lhs <= 0;
            alu_rhs <= a;
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

          // M1 T3: JR CC
          16'b001??00000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: JR CC
          16'b001??00000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: JR CC
          16'b001??00000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: JR CC
          16'b001??00000000101: begin
            state <= {state[15:8], 8'h06};
            pc <= pc + 1;
            tmp <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: JR CC
          16'b001??00000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: JR CC
          16'b001??00000000111: begin
            case (state[12:11])
              2'b00: begin
                state <= zf ? 16'h0000 : {state[15:8], 8'h08};
              end
              2'b01: begin
                state <= zf ? {state[15:8], 8'h08} : 16'h0000;
              end
              2'b10: begin
                state <= cf ? 16'h0000 : {state[15:8], 8'h08};
              end
              2'b11: begin
                state <= cf ? {state[15:8], 8'h08} : 16'h0000;
              end
            endcase
          end
          // M3 T1: JR CC
          16'b001??00000001000: begin
            state <= {state[15:8], 8'h09};
          end
          // M3 T2: JR CC
          16'b001??00000001001: begin
            state <= {state[15:8], 8'h0a};
          end
          // M3 T3: JR CC
          16'b001??00000001010: begin
            state <= {state[15:8], 8'h0b};
          end
          // M3 T4: JR CC
          16'b001??00000001011: begin
            state <= {state[15:8], 8'h00};
            pc <= pc + { {8{tmp[7]}}, tmp};
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
            pc <= pc + { {8{tmp[7]}}, tmp};
          end

          // M1 T3: LD r8, d8
          16'b00???11000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: LD r8, d8
          16'b00???11000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LD r8, d8
          16'b00???11000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: LD r8, d8
          16'b00???11000000101: begin
            case (state[13:11])
              3'b000: b <= data_in;
              3'b001: c <= data_in;
              3'b010: d <= data_in;
              3'b011: e <= data_in;
              3'b100: h <= data_in;
              3'b101: l <= data_in;
              3'b110: $finish;
              3'b111: a <= data_in;
            endcase
            rd_enable <= 0;
            state <= {state[15:8], 8'h06};
          end
          // M2 T3: LD r8, d8
          16'b00???11000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LD r8, d8
          16'b00???11000000111: begin
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

          // M1 T3: INC?DEC r8
          16'b00???10?00000010: begin
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
              3'b110: $finish;
              3'b111: alu_lhs <= a;
            endcase;
            alu_rhs <= 8'h01;
          end
          // M1 T4: INC/DEC r8
          16'b00???10?00000011: begin
            state <= {state[15:8], 8'h00};
            case (state[13:11])
              3'b000: b <= alu_r;
              3'b001: c <= alu_r;
              3'b010: d <= alu_r;
              3'b011: e <= alu_r;
              3'b100: h <= alu_r;
              3'b101: l <= alu_r;
              3'b110: $finish;
              3'b111: a <= alu_r;
            endcase;
            zf <= alu_zf;
            nf <= alu_nf;
            hf <= alu_hf;
          end
          // HALT
          16'b0111011000000010: begin
            $display("HALT");
            $finish;
          end
          // M1 T3: LD (HL), r8
          16'b01110???00000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: LD (HL), r8
          16'b01110???00000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LD (HL), r8
          16'b01110???00000100: begin
            state <= {state[15:8], 8'h05};
            addr <= {h, l};
            case (state[10:8])
              3'b000: data_out <= b;
              3'b001: data_out <= c;
              3'b010: data_out <= d;
              3'b011: data_out <= e;
              3'b100: data_out <= h;
              3'b101: data_out <= l;
              3'b110: $finish;
              3'b111: data_out <= a;
            endcase;
            wr_enable <= 1;
          end
          // M2 T2: LD (HL), r8
          16'b01110???00000101: begin
            state <= {state[15:8], 8'h06};
            wr_enable <= 0;
          end
          // M2 T3: LD (HL), r8
          16'b01110???00000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LD (HL), r8
          16'b01110???00000111: begin
            state <= {state[15:8], 8'h00};
          end

          // LD r8, (HL)
          16'b01???11000000010: begin
            $display("LD r8, (HL)");
            $finish;
          end

          // M1 T3: LD r8, r8
          16'b01??????00000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
            case (state[10:8])
              3'b000: tmp <= b;
              3'b001: tmp <= c;
              3'b010: tmp <= d;
              3'b011: tmp <= e;
              3'b100: tmp <= h;
              3'b101: tmp <= l;
              3'b110: $finish;
              3'b111: tmp <= a;
            endcase;
          end
          // M1 T4: LD r8, r8
          16'b01??????00000011: begin
            state <= {state[15:8], 8'h00};
            case (state[13:11])
              3'b000: b <= tmp;
              3'b001: c <= tmp;
              3'b010: d <= tmp;
              3'b011: e <= tmp;
              3'b100: h <= tmp;
              3'b101: l <= tmp;
              3'b110: $finish;
              3'b111: a <= tmp;
            endcase;
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
          16'b000?101000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: LD A, (r16)
          16'b000?101000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: LD A, (r16)
          16'b000?101000000100: begin
            state <= {state[15:8], 8'h05};
            case (state[12])
              1'b0: addr <= {b, c};
              1'b1: addr <= {d, e};
            endcase
            rd_enable <= 1;
          end
          // M2 T2: LD A, (r16)
          16'b000?101000000101: begin
            state <= {state[15:8], 8'h06};
            a <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: LD A, (r16)
          16'b000?101000000110: begin
            state <= {state[15:8], 8'h07};
          end
          // M2 T4: LD A, (r16)
          16'b000?101000000111: begin
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
          // M4 T1: CALL
          16'b1100110100001100: begin
            state <= {state[15:8], 8'h0d};
          end
          // M4 T2: CALL
          16'b1100110100001101: begin
            state <= {state[15:8], 8'h0e};
          end
          // M4 T3: CALL
          16'b1100110100001110: begin
            state <= {state[15:8], 8'h0f};
            sp <= sp - 1;
          end
          // M4 T4: CALL
          16'b1100110100001111: begin
            state <= {state[15:8], 8'h10};
          end
          // M5 T1: CALL
          16'b1100110100010000: begin
            state <= {state[15:8], 8'h11};
            addr <= sp;
            data_out <= pc[15:8];
            wr_enable <= 1;
          end
          // M5 T2: CALL
          16'b1100110100010001: begin
            state <= {state[15:8], 8'h12};
            wr_enable <= 0;
          end
          // M5 T3: CALL
          16'b1100110100010010: begin
            state <= {state[15:8], 8'h13};
            sp <= sp - 1;
          end
          // M5 T4: CALL
          16'b1100110100010011: begin
            state <= {state[15:8], 8'h14};
            pc <= {w, pc[7:0]};
          end
          // M6 T1: CALL
          16'b1100110100010100: begin
            state <= {state[15:8], 8'h15};
            addr <= sp;
            data_out <= pc[7:0];
            wr_enable <= 1;
          end
          // M6 T2: CALL
          16'b1100110100010101: begin
            state <= {state[15:8], 8'h16};
            wr_enable <= 0;
          end
          // M6 T3: CALL
          16'b1100110100010110: begin
            state <= {state[15:8], 8'h17};
          end
          // M6 T4: CALL
          16'b1100110100010111: begin
            state <= {state[15:8], 8'h00};
            pc <= {pc[15:8], z};
          end

          16'b110??10000000010: begin
            $display("CALL cc not implemented");
            $finish;
          end

          // M1 T3: PUSH r16
          16'b11??010100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: PUSH r16
          16'b11??010100000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: PUSH r16
          16'b11??010100000100: begin
            state <= {state[15:8], 8'h05};
          end
          // M2 T2: PUSH r16
          16'b11??010100000101: begin
            state <= {state[15:8], 8'h06};
          end
          // M2 T3: PUSH r16
          16'b11??010100000110: begin
            state <= {state[15:8], 8'h07};
            sp <= sp - 1;
          end
          // M2 T4: PUSH r16
          16'b11??010100000111: begin
            state <= {state[15:8], 8'h08};
          end
          // M3 T1: PUSH r16
          16'b11??010100001000: begin
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
          16'b11??010100001001: begin
            state <= {state[15:8], 8'h0a};
            wr_enable <= 0;
          end
          // M3 T3: PUSH r16
          16'b11??010100001010: begin
            state <= {state[15:8], 8'h0b};
            sp <= sp - 1;
          end
          // M3 T4: PUSH r16
          16'b11??010100001011: begin
            state <= {state[15:8], 8'h0c};
          end
          // M4 T1: PUSH r16
          16'b11??010100001100: begin
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
          16'b11??010100001101: begin
            state <= {state[15:8], 8'h0e};
            wr_enable <= 0;
          end
          // M4 T3: PUSH r16
          16'b11??010100001110: begin
            state <= {state[15:8], 8'h0f};
          end
          // M4 T4: PUSH r16
          16'b11??010100001111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: POP r16
          16'b11??000100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: POP r16
          16'b11??000100000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: POP r16
          16'b11??000100000100: begin
            state <= {state[15:8], 8'h05};
            addr <= sp;
            rd_enable <= 1;
          end
          // M2 T2: POP r16
          16'b11??000100000101: begin
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
          16'b11??000100000110: begin
            state <= {state[15:8], 8'h07};
            sp <= sp + 1;
          end
          // M2 T4: POP r16
          16'b11??000100000111: begin
            state <= {state[15:8], 8'h08};
          end
          // M3 T1: POP r16
          16'b11??000100001000: begin
            state <= {state[15:8], 8'h09};
            addr <= sp;
            rd_enable <= 1;
          end
          // M3 T2: POP r16
          16'b11??000100001001: begin
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
          16'b11??000100001010: begin
            state <= {state[15:8], 8'h0b};
            sp <= sp + 1;
          end
          // M3 T4: POP r16
          16'b11??000100001011: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: INC r16
          16'b00??001100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: INC r16
          16'b00??001100000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: INC r16
          16'b00??001100000100: begin
            state <= {state[15:8], 8'h05};
          end
          // M2 T2: INC r16
          16'b00??001100000101: begin
            state <= {state[15:8], 8'h06};
          end
          // M2 T3: INC r16
          16'b00??001100000110: begin
            state <= {state[15:8], 8'h07};
            case (state[13:12])
              2'b00: {b, c} <= {b, c} + 1;
              2'b01: {d, e} <= {d, e} + 1;
              2'b10: {h, l} <= {h, l} + 1;
              2'b11: sp <= sp + 1;
            endcase
          end
          // M2 T4: INC r16
          16'b00??001100000111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: RET
          16'b110?100100000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: RET
          16'b110?100100000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: RET
          16'b110?100100000100: begin
            state <= {state[15:8], 8'h05};
            addr <= sp;
            rd_enable <= 1;
          end
          // M2 T2: RET
          16'b110?100100000101: begin
            state <= {state[15:8], 8'h06};
            z <= data_in;
            rd_enable <= 0;
          end
          // M2 T3: RET
          16'b110?100100000110: begin
            state <= {state[15:8], 8'h07};
            sp <= sp + 1;
          end
          // M2 T4: RET
          16'b110?100100000111: begin
            state <= {state[15:8], 8'h08};
            ime <= state[12] ? 1'b1 : ime;
          end
          // M3 T1: RET
          16'b110?100100001000: begin
            state <= {state[15:8], 8'h09};
            addr <= sp;
            rd_enable <= 1;
          end
          // M3 T2: RET
          16'b110?100100001001: begin
            state <= {state[15:8], 8'h0a};
            w <= data_in;
            rd_enable <= 0;
          end
          // M3 T3: RET
          16'b110?100100001010: begin
            state <= {state[15:8], 8'h0b};
            sp <= sp + 1;
          end
          // M3 T4: RET
          16'b110?100100001011: begin
            state <= {state[15:8], 8'h0c};
          end
          // M4 T1: RET
          16'b110?100100001100: begin
            state <= {state[15:8], 8'h0d};
          end
          // M4 T2: RET
          16'b110?100100001101: begin
            state <= {state[15:8], 8'h0e};
          end
          // M4 T3: RET
          16'b110?100100001110: begin
            state <= {state[15:8], 8'h0f};
            pc <= {w, z};
          end
          // M4 T4: RET
          16'b110?100100001111: begin
            state <= {state[15:8], 8'h00};
          end

          // M1 T3: ALU d8
          16'b11???11000000010: begin
            state <= {state[15:8], 8'h03};
            pc <= pc + 1;
          end
          // M1 T4: ALU d8
          16'b11???11000000011: begin
            state <= {state[15:8], 8'h04};
          end
          // M2 T1: ALU d8
          16'b11???11000000100: begin
            state <= {state[15:8], 8'h05};
            addr <= pc;
            rd_enable <= 1;
          end
          // M2 T2: ALU d8
          16'b11???11000000101: begin
            state <= {state[15:8], 8'h06};
            alu_lhs <= a;
            alu_rhs <= data_in;
            rd_enable <= 0;
            alu_op <= state[13:11];
          end
          // M2 T3: ALU d8
          16'b11???11000000110: begin
            state <= {state[15:8], 8'h07};
            pc <= pc + 1;
          end
          // M2 T4: ALU d8
          16'b11???11000000111: begin
            state <= {state[15:8], 8'h00};
            zf <= alu_zf;
            nf <= alu_nf;
            hf <= alu_hf;
            cf <= alu_cf;
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

          default: begin
            $display("ERROR: %x %x %x", pc, state[15:8], state[7:0]);
            $finish;
          end
        endcase
      end
    end
  end
endmodule
