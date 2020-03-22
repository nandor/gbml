// This file is part of the GBC-ML project.
// Licensing information is available in the LICENSE file.
// (C) 2020 Nandor Licker. All rights reserved.

#pragma once

typedef enum {
  MBC1,
  MBC2,
} cart_type_t;

typedef struct {
  bool has_ram;
  bool has_battery;
  char *ram;
  char *rom;
  bool mode;
  unsigned rom_bank;
  unsigned ram_bank;
} cart_mbc1_t;

typedef struct {
  char *data;
  cart_type_t type;
  union {
    cart_mbc1_t *mbc1;
  };
} cart_t;

bool cart_read(cart_t *cart, const char *path);
void cart_write(cart_t *cart, uint16_t addr, uint8_t val);
uint8_t cart_read(cart_t *cart, uint16_t addr);
