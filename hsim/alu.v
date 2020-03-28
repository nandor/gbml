// This file is part of the GBC-ML project.
// Licensing information is available in the LICENSE file.
// (C) 2020 Nandor Licker. All rights reserved.

module ALU
  ( input      [7:0] lhs
  , input      [7:0] rhs
  , input      [3:0] op
  , output reg [7:0] r
  , input            zf_in
  , input            nf_in
  , input            hf_in
  , input            cf_in
  , output reg       zf_out
  , output reg       nf_out
  , output reg       hf_out
  , output reg       cf_out
  );

  logic [4:0] half;
  logic [4:0] full;

  always @(op or lhs or rhs or zf_in or nf_in or hf_in or cf_in) begin
    case (op)
      // ADD
      4'b0000: begin
        half = lhs[3:0] + rhs[3:0];
        full = lhs[7:4] + rhs[7:4] + {3'b000, half[4]};
        r <= {full[3:0], half[3:0]};
        zf_out <= !(|full[3:0] | |half[3:0]);
        nf_out <= 0;
        hf_out <= half[4];
        cf_out <= full[4];
      end
      // ADC
      4'b0001: begin
        half = lhs[3:0] + rhs[3:0] + {3'b000, cf_in};
        full = lhs[7:4] + rhs[7:4] + {3'b000, half[4]};
        r <= {full[3:0], half[3:0]};
        zf_out <= !(|full[3:0] | |half[3:0]);
        nf_out <= 0;
        hf_out <= half[4];
        cf_out <= full[4];
      end
      // SUB
      4'b0010: begin
        half = lhs[3:0] - rhs[3:0];
        full = lhs[7:4] - rhs[7:4] - {3'b000, half[4]};
        r <= {full[3:0], half[3:0]};
        zf_out <= !(|full[3:0] | |half[3:0]);
        nf_out <= 1;
        hf_out <= half[4];
        cf_out <= full[4];
      end
      // SBC
      4'b0011: begin
        half = lhs[3:0] - rhs[3:0] - {3'b000, cf_in};
        full = lhs[7:4] - rhs[7:4] - {3'b000, half[4]};
        r <= {full[3:0], half[3:0]};
        zf_out <= !(|full[3:0] | |half[3:0]);
        nf_out <= 1;
        hf_out <= half[4];
        cf_out <= full[4];
      end
      // AND
      4'b0100: begin
        r <= lhs & rhs;
        zf_out <= !(|(lhs & rhs));
        nf_out <= 0;
        hf_out <= 1;
        cf_out <= 0;
      end
      // XOR
      4'b0101: begin
        r <= lhs ^ rhs;
        zf_out <= !(|(lhs ^ rhs));
        nf_out <= 0;
        hf_out <= 0;
        cf_out <= 0;
      end
      // OR
      4'b0110: begin
        r <= lhs | rhs;
        zf_out <= !(|(lhs | rhs));
        nf_out <= 0;
        hf_out <= 0;
        cf_out <= 0;
      end
      // CP
      4'b0111: begin
        half = lhs[3:0] - rhs[3:0];
        full = lhs[7:4] - rhs[7:4] - {3'b000, half[4]};
        r <= lhs;
        zf_out <= !(|full[3:0] | |half[3:0]);
        nf_out <= 1;
        hf_out <= half[4];
        cf_out <= full[4];
      end
      // RLC
      4'b1000: begin
        r <= { lhs[6:0], lhs[7] };
        zf_out <= !(|lhs);
        nf_out <= 0;
        hf_out <= 0;
        cf_out <= lhs[7];
      end
      // RRC
      4'b1001: begin
        r <= { lhs[0], lhs[7:1] };
        zf_out <= !(|lhs);
        nf_out <= 0;
        hf_out <= 0;
        cf_out <= lhs[0];
      end
      // RL
      4'b1010: begin
        r <= { lhs[6:0], cf_in };
        zf_out <= !(|lhs[6:0] | cf_in);
        nf_out <= 0;
        hf_out <= 0;
        cf_out <= lhs[7];
      end
      // RR
      4'b1011: begin
        r <= { cf_in, lhs[7:1] };
        zf_out <= !( cf_in | |lhs[7:1]);
        nf_out <= 0;
        hf_out <= 0;
        cf_out <= lhs[0];
      end
      // SLA
      4'b1100: begin
        r <= { lhs[6:0], 1'b0 };
        zf_out <= !(|lhs[6:0]);
        nf_out <= 0;
        hf_out <= 0;
        cf_out <= lhs[7];
      end
      // SRA
      4'b1101: begin
        r <= { lhs[7], lhs[7:1] };
        zf_out <= !(|lhs[7:1]);
        nf_out <= 0;
        hf_out <= 0;
        cf_out <= lhs[0];
      end
      // SWAP
      4'b1110: begin
        r <= {lhs[3:0], lhs[7:4]};
        zf_out <= !(|lhs);
        nf_out <= 0;
        hf_out <= 0;
        cf_out <= 0;
      end
      // SRL
      4'b1111: begin
        r <= { 1'b0, lhs[7:1] };
        zf_out <= !(|lhs[7:1]);
        nf_out <= 0;
        hf_out <= 0;
        cf_out <= lhs[0];
      end
    endcase
  end
endmodule
