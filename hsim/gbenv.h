// This file is part of the GBC-ML project.
// Licensing information is available in the LICENSE file.
// (C) 2020 Nandor Licker. All rights reserved.

#pragma once

#include "cart.h"
#include "gpu.h"

typedef struct {
  cart_t cart;
  gpu_t gpu;
} env_t;
