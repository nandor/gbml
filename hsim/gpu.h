// This file is part of the GBC-ML project.
// Licensing information is available in the LICENSE file.
// (C) 2020 Nandor Licker. All rights reserved.

#pragma once

typedef enum {
  HBlank,
  VBlank,
  OAMRead,
  VRAMRead,
} gpu_state_t;

typedef struct {
  char vram[0x2000];

  uint8_t scroll_x;
  uint8_t scroll_y;

  bool enable;
  bool window_tile_map;
  bool window_display;
  bool bg_window_tile_data;
  bool bg_window_tile_map;
  bool bg_window_display;
  bool sprite_size;
  bool sprite_display;

  uint8_t bg_palette;

  uint8_t ly;
  uint8_t lyc;
  uint8_t wx;
  uint8_t wy;

  bool stat_equ;

  gpu_state_t state;
  unsigned cycles;
} gpu_t;


void gpu_init(gpu_t *gpu);
void gpu_tick(gpu_t *gpu);

void gpu_vram_write(gpu_t *gpu, uint16_t addr, uint8_t val);

void gpu_set_lcdc(gpu_t *gpu, uint8_t val);
void gpu_set_scroll_x(gpu_t *gpu, uint8_t val);
void gpu_set_scroll_y(gpu_t *gpu, uint8_t val);
void gpu_set_bgp(gpu_t *gpu, uint8_t val);

uint8_t gpu_get_lcdc(gpu_t *gpu);
uint8_t gpu_get_scroll_x(gpu_t *gpu);
uint8_t gpu_get_scroll_y(gpu_t *gpu);
uint8_t gpu_get_bgp(gpu_t *gpu);

uint8_t gpu_get_ly(gpu_t *gpu);
