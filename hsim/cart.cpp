// This file is part of the GBC-ML project.
// Licensing information is available in the LICENSE file.
// (C) 2020 Nandor Licker. All rights reserved.

#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <vpi_user.h>
#include "cart.h"

static cart_mbc1_t *load_mbc1(
    char *data,
    unsigned nbanks,
    bool has_ram,
    bool has_battery)
{
  cart_mbc1_t *cart = new cart_mbc1_t;
  cart->has_ram = has_ram;
  cart->has_battery = has_battery;
  cart->ram = nullptr;
  cart->rom = data;
  cart->mode = false;
  cart->rom_bank = 0;
  cart->ram_bank = 0;
  return cart;
}

bool cart_read(cart_t *cart, const char *path)
{
  FILE *file = fopen(path, "rb");
  if (!file)
    return false;

  fseek(file, 0, SEEK_END);
  size_t size = ftell(file);
  fseek(file, 0, SEEK_SET);

  cart->data = new char[size];
  if (!cart->data)
    return false;

  if (fread(cart->data, size, 1, file) != 1)
    return false;

  unsigned nbanks;
  switch (cart->data[0x148]) {
    case 0x00: nbanks = 2; break;
    case 0x01: nbanks = 4; break;
    case 0x02: nbanks = 8; break;
    case 0x03: nbanks = 16; break;
    case 0x04: nbanks = 32; break;
    case 0x05: nbanks = 64; break;
    case 0x06: nbanks = 128; break;
    case 0x07: nbanks = 256; break;
    case 0x08: nbanks = 512; break;
    case 0x52: nbanks = 72; break;
    case 0x53: nbanks = 80; break;
    case 0x54: nbanks = 96; break;
    default:
      return false;
  }

  if (size != nbanks * 0x4000)
    return false;

  switch (cart->data[0x147]) {
    case 0x00:
    case 0x01: {
      cart->type = MBC1;
      cart->mbc1 = load_mbc1(cart->data, nbanks, false, false);
      break;
    }
    case 0x02: {
      cart->type = MBC1;
      cart->mbc1 = load_mbc1(cart->data, nbanks, true, false);
      break;
    }
    case 0x03: {
      cart->type = MBC1;
      cart->mbc1 = load_mbc1(cart->data, nbanks, true, true);
      break;
    }
    default: {
      return false;
    }
  }

  return cart->mbc1 != nullptr;
}

void cart_write(cart_t *cart, uint16_t addr, uint8_t val)
{
  fprintf(stderr, "WTF0\n");
}

uint8_t cart_read(cart_t *cart, uint16_t addr)
{
  switch (cart->type) {
    case MBC1: {
      switch (addr & 0xF000) {
        case 0x0000: case 0x1000: case 0x2000: case 0x3000: {
          return cart->mbc1->rom[addr];
        }
        case 0x4000: case 0x5000: case 0x6000: case 0x7000: {
          abort();
        }
        default: {
          assert(!"invalid address");
        }
      }
    }
    case MBC2: {
      assert(!"not implemented");
    }
  }
}
