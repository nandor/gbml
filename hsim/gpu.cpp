// This file is part of the GBC-ML project.
// Licensing information is available in the LICENSE file.
// (C) 2020 Nandor Licker. All rights reserved.

#include <cassert>
#include <cstring>
#include <cstdlib>
#include <algorithm>

#include <vpi_user.h>
#include <SDL2/SDL.h>

#include "gpu.h"

#define ROWS 144
#define COLS 160
#define SCALE 1

static SDL_Window *gWindow;
static SDL_Surface *gSurface;
char gData[(ROWS * SCALE) * (COLS * SCALE) * 3];

static void gpu_update(gpu_t *gpu)
{
  SDL_Surface *windowSurface = SDL_GetWindowSurface(gWindow);
  SDL_BlitSurface(gSurface, nullptr, windowSurface, nullptr);
  SDL_UpdateWindowSurface(gWindow);
}

static void gpu_setup(gpu_t *gpu)
{
  SDL_Init(SDL_INIT_VIDEO);

  gWindow = SDL_CreateWindow(
      "GB",
      SDL_WINDOWPOS_UNDEFINED,
      SDL_WINDOWPOS_UNDEFINED,
      COLS * SCALE,
      ROWS * SCALE,
      0
  );
  if (!gWindow) {
    vpi_printf("Cannot create SDL window\n");
    vpi_control(vpiFinish);
  }

  gSurface = SDL_CreateRGBSurfaceFrom(
      gData,
      COLS * SCALE,
      ROWS * SCALE,
      3 * 8,
      COLS * SCALE * 3,
      0x0000FF,
      0x00FF00,
      0xFF0000,
      0
  );

  gpu_update(gpu);
}

static void fill_pixel(gpu_t *gpu, unsigned y, unsigned x, char c) {
  for (unsigned dy = 0; dy < SCALE; ++dy) {
    for (unsigned dx = 0; dx < SCALE; ++dx) {
      unsigned idx = (y * SCALE + dy) * COLS + (x * SCALE + dx);
      gData[idx * 3 + 0] = c;
      gData[idx * 3 + 1] = c;
      gData[idx * 3 + 2] = c;
    }
  }
}

uint8_t get_tile_pixel(gpu_t *gpu, int8_t tile, uint8_t l, uint8_t c, uint16_t base)
{
  uint16_t idx = base + (l << 1);
  if (base == 0x1000) {
    idx += ((uint16_t)tile) << 4;
  } else {
    idx += ((int16_t)tile) << 4;
  }

  uint8_t b0 = gpu->vram[idx + 0];
  uint8_t b1 = gpu->vram[idx + 1];
  uint8_t p0 = ((b0 & (1 << c)) >> c) << 0;
  uint8_t p1 = ((b1 & (1 << c)) >> c) << 1;
  return p0 | p1;
}

uint8_t bg_pixel(gpu_t *gpu, unsigned y, unsigned x)
{
  uint16_t map_base = gpu->bg_window_tile_map ? 0x1C00 : 0x1800;
  uint16_t addr =  map_base + (y >> 3) * 32 + (x >> 3);
  uint8_t tile = gpu->vram[addr];
  uint8_t l = y & 0x7;
  uint8_t c = 7 - (x & 0x7);
  uint16_t data_base = gpu->bg_window_tile_data ? 0x0000 : 0x0800;
  uint8_t pixel = get_tile_pixel(gpu, tile, l, c, data_base);
  uint8_t pxp = (~((gpu->bg_palette >> (pixel * 2)) & 0x3)) & 0x3;
  return (pxp << 6) | (pxp << 4) | (pxp << 2) | (pxp << 0);
}

uint8_t wnd_pixel(gpu_t *gpu, unsigned y, unsigned x)
{
  assert(!"not implemented");
}

static void gpu_scanline(gpu_t *gpu)
{
  SDL_Event event;
  while (SDL_PollEvent(&event)) {
    switch (event.type) {
      case SDL_QUIT: {
        SDL_FreeSurface(gSurface);
        SDL_DestroyWindow(gWindow);
        SDL_Quit();
        vpi_control(vpiFinish);
        return;
      }
    }
  }

  unsigned ly = gpu->ly - 1;
  if (gpu->bg_window_display) {
    {
      unsigned y = (gpu->scroll_y + ly) & 0xFF;
      for (unsigned lx = 0; lx < COLS; ++lx) {
        unsigned x = (gpu->scroll_x + lx) & 0xFF;
        fill_pixel(gpu, ly, lx, bg_pixel(gpu, y, x));
      }
    }

    if (gpu->window_display && ly >= gpu->wy) {
      for (unsigned lx = std::max(0, gpu->wx - 7); lx < COLS; ++lx) {
        unsigned x = lx - gpu->wx + 7;
        unsigned y = ly - gpu->wy;
        fill_pixel(gpu, ly, lx, wnd_pixel(gpu, y, x));
      }
    }
  }

  if (gpu->sprite_display) {
    assert(!"not implemented");
  }

  gpu_update(gpu);
}

static void gpu_vblank(gpu_t *gpu)
{
  gpu_update(gpu);
}

void gpu_init(gpu_t *gpu)
{
  memset(gpu->vram, 0x00, 0x2000);

  gpu->scroll_x = 0x00;
  gpu->scroll_y = 0x00;
  gpu->enable = false;
  gpu->window_tile_map = false;
  gpu->window_display = false;
  gpu->bg_window_tile_data = false;
  gpu->bg_window_tile_map = false;
  gpu->bg_window_display = false;
  gpu->sprite_size = false;
  gpu->sprite_display = false;

  gpu->bg_palette = 0x00;

  gpu->ly = 0x00;
  gpu->lyc = 0x00;
  gpu->wx = 0x00;
  gpu->wy = 0x00;
  gpu->stat_equ = false;

  gpu->state = HBlank;
  gpu->cycles = 0;

  gpu_setup(gpu);
}

void gpu_tick(gpu_t *gpu)
{
  if (!gpu->enable) {
    return;
  }
  switch (gpu->state) {
    case HBlank:{
      if (gpu->cycles < 51 * 4) {
        ++gpu->cycles;
        return;
      }
      gpu->cycles = 0;
      gpu->ly++;
      gpu->stat_equ = gpu->ly == gpu->lyc;
      gpu->state = gpu->ly >= 144 ? VBlank : OAMRead;
      gpu_scanline(gpu);
      return;
    }
    case VBlank:{
      if (gpu->cycles < 114 * 4) {
        ++gpu->cycles;
        return;
      }
      gpu->cycles = 0;
      if (gpu->ly >= 153) {
        gpu_vblank(gpu);
        gpu->ly = 0;
        gpu->stat_equ = gpu->lyc == 0;
        gpu->state = OAMRead;
      } else {
        gpu->ly++;
        gpu->stat_equ = gpu->ly == gpu->lyc;
        gpu->state = VBlank;
      }
      return;
    }
    case OAMRead:{
      if (gpu->cycles < 20 * 4) {
        ++gpu->cycles;
        return;
      }
      gpu->cycles = 0;
      gpu->state = VRAMRead;
      return;
    }
    case VRAMRead:{
      if (gpu->cycles < 43 * 4) {
        ++gpu->cycles;
        return;
      }
      gpu->cycles = 0;
      gpu->state = HBlank;
      return;
    }
  }
}

void gpu_vram_write(gpu_t *gpu, uint16_t addr, uint8_t val)
{
  gpu->vram[addr] = val;
}

void gpu_set_lcdc(gpu_t *gpu, uint8_t val)
{
  bool enabled = (val & 0x80) != 0;
  if (enabled && !gpu->enable) {
    gpu->state = HBlank;
    gpu->cycles = 0;
  }
  if (!enabled && gpu->enable) {
    if (gpu->state != VBlank) {
      vpi_printf("LCD disabled outside of VBlank\n");
      vpi_control(vpiFinish);
      return;
    }
    gpu->state = HBlank;
    gpu->cycles = 0;
  }
  gpu->enable = enabled;
  gpu->window_tile_map = (val & 0x40) != 0;
  gpu->window_display = (val & 0x20) != 0;
  gpu->bg_window_tile_data = (val & 0x10) != 0;
  gpu->bg_window_tile_map = (val & 0x08) != 0;
  gpu->bg_window_display = (val & 0x01) != 0;
  gpu->sprite_size = (val & 0x04) != 0;
  gpu->sprite_display = (val & 0x02) != 0;
}

uint8_t gpu_get_lcdc(gpu_t *gpu)
{
  uint8_t val = 0;
  val |= gpu->enable ? 0x80 : 0x00;
  val |= gpu->window_tile_map ? 0x40 : 0x00;
  val |= gpu->window_display ? 0x20 : 0x00;
  val |= gpu->bg_window_tile_data ? 0x10 : 0x00;
  val |= gpu->bg_window_tile_map ? 0x08 : 0x00;
  val |= gpu->bg_window_display ? 0x01 : 0x00;
  val |= gpu->sprite_size ? 0x04 : 0x00;
  val |= gpu->sprite_display ? 0x02 : 0x00;
  return val;
}

void gpu_set_scroll_x(gpu_t *gpu, uint8_t val)
{
  gpu->scroll_x = val;
}

uint8_t gpu_get_scroll_x(gpu_t *gpu)
{
  return gpu->scroll_x;
}

void gpu_set_scroll_y(gpu_t *gpu, uint8_t val)
{
  gpu->scroll_y = val;
}

uint8_t gpu_get_scroll_y(gpu_t *gpu)
{
  return gpu->scroll_y;
}

void gpu_set_bgp(gpu_t *gpu, uint8_t val)
{
  gpu->bg_palette = val;
}

uint8_t gpu_get_bgp(gpu_t *gpu)
{
  return gpu->bg_palette;
}

uint8_t gpu_get_ly(gpu_t *gpu)
{
  return gpu->ly;
}
