# This file is part of the GBC-ML project.
# Licensing information is available in the LICENSE file.
# (C) 2020 Nandor Licker. All rights reserved.

all: gbsim.vvp gbenv.vpi

gbsim.vvp: alu.v mmu.v timer.v system.v cpu.v
	iverilog -o$@ -g2012 $^

gbenv.vpi: gbenv.cpp cart.cpp gpu.cpp input.cpp
	iverilog-vpi $^ -o $@ -lSDL2

clean:
	rm -f *.o
	rm -f *.vpi
	rm -f *.vvp
