# This file is part of the GBC-ML project.
# Licensing information is available in the LICENSE file.
# (C) 2020 Nandor Licker. All rights reserved.

all:
	$(MAKE) -C interp

.PHONY: clean
clean:
	$(MAKE) -C interp clean
