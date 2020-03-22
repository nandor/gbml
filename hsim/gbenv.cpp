// This file is part of the GBC-ML project.
// Licensing information is available in the LICENSE file.
// (C) 2020 Nandor Licker. All rights reserved.

#include <cstdlib>
#include <cstring>
#include <climits>
#include <type_traits>
#include <vpi_user.h>

#include "gbenv.h"
#include "gpu.h"


template <typename... Args>
struct Checker;

template <typename T, typename... Args>
struct Checker<T, Args...> {
  int operator() (vpiHandle call, vpiHandle args)
  {
    if (args == NULL || vpi_scan(args) == NULL) {
      vpi_printf(
          "ERROR: %s:%d: missing argument\n",
          vpi_get_str(vpiFile, call),
          (int)vpi_get(vpiLineNo, call)
      );
      vpi_control(vpiFinish);
      return 0;
    }

    return Checker<Args...>{}(call, args);
  }
};

template <>
struct Checker<> {
  int operator() (vpiHandle call, vpiHandle args)
  {
    // No further arguments.
    if (args != NULL && vpi_scan(args)) {
      vpi_printf(
          "ERROR: %s:%d: too many arguments\n",
          vpi_get_str(vpiFile, call),
          (int)vpi_get(vpiLineNo, call)
      );
      vpi_control(vpiFinish);
      return 0;
    }

    return 0;
  }
};

template <typename... ArgTys>
int Compile(char *user_data)
{
  (void) user_data;

  // Get a handle to the arguments.
  vpiHandle systf = vpi_handle(vpiSysTfCall, NULL);
  if (systf == NULL) {
    vpi_printf("Cannot get system task\n");
    vpi_control(vpiFinish);
    return 0;
  }

  // Check the arguments.
  return Checker<ArgTys...>{}(systf, vpi_iterate(vpiArgument, systf));
}

template <typename RetT>
int Size(char *user_data)
{
  (void) user_data;
  return sizeof(RetT) * CHAR_BIT;
}

template <typename T>
int GetArg(vpiHandle args)
{
  vpiHandle arg_addr = vpi_scan(args);
  s_vpi_value arg_val;
  arg_val.format = vpiIntVal;
  vpi_get_value(arg_addr, &arg_val);
  return arg_val.value.integer;
}

template <typename T>
void Return(vpiHandle call, T val)
{
  s_vpi_value ret_val;
  ret_val.format = vpiIntVal;
  ret_val.value.integer = val;
  vpi_put_value(call, &ret_val, 0, vpiNoDelay);
}

int vpi_gpu_tick(char *data)
{
  gpu_tick((gpu_t *)data);
  return 0;
}

int vpi_gpu_vram_write(char *data)
{
  vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle args = vpi_iterate(vpiArgument, call);

  auto addr = GetArg<uint16_t>(args);
  auto val = GetArg<uint8_t>(args);
  gpu_vram_write((gpu_t *)data, addr, val);
  return 0;
}

int vpi_gpu_set_lcdc(char *data)
{
  vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle args = vpi_iterate(vpiArgument, call);

  auto val = GetArg<uint8_t>(args);
  gpu_set_lcdc((gpu_t *)data, val);
  return 0;
}

int vpi_gpu_get_lcdc(char *data)
{
  vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
  Return(call, gpu_get_lcdc((gpu_t *)data));
  return 0;
}

int vpi_gpu_set_scroll_y(char *data)
{
  vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle args = vpi_iterate(vpiArgument, call);

  auto val = GetArg<uint8_t>(args);
  gpu_set_scroll_y((gpu_t *)data, val);
  return 0;
}

int vpi_gpu_get_scroll_y(char *data)
{
  vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
  Return(call, gpu_get_scroll_y((gpu_t *)data));
  return 0;
}

int vpi_gpu_set_bgp(char *data)
{
  vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle args = vpi_iterate(vpiArgument, call);

  auto val = GetArg<uint8_t>(args);
  gpu_set_bgp((gpu_t *)data, val);
  return 0;
}

int vpi_gpu_get_bgp(char *data)
{
  vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
  Return(call, gpu_get_bgp((gpu_t *)data));
  return 0;
}

int vpi_gpu_get_ly(char *data)
{
  vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
  Return(call, gpu_get_ly((gpu_t *)data));
  return 0;
}

int vpi_cart_write(char *data)
{
  vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle args = vpi_iterate(vpiArgument, call);

  auto addr = GetArg<uint16_t>(args);
  auto val = GetArg<uint8_t>(args);
  cart_write((cart_t *)data, addr, val);
  return 0;
}

int vpi_cart_read(char *data)
{
  vpiHandle call = vpi_handle(vpiSysTfCall, NULL);
  vpiHandle args = vpi_iterate(vpiArgument, call);

  auto addr = GetArg<uint16_t>(args);
  Return(call, cart_read((cart_t *)data, addr));
  return 0;
}

void gbenv_register(void) {
  // Allocate the environment object.
  env_t *env = (env_t *)malloc(sizeof(env_t));
  if (env == NULL) {
    vpi_printf("Cannot create the environment\n");
    exit(-1);
  }
  gpu_init(&env->gpu);

  // Find the path to the ROM file.
  s_vpi_vlog_info info;
  vpi_get_vlog_info(&info);
  const char *rom_path = nullptr;
  for (int i = 0; i < info.argc; ++i) {
    if (strncmp(info.argv[i], "rom=", 4) == 0) {
      rom_path = &info.argv[i][4];
    }
  }
  if (!rom_path) {
    vpi_printf("No ROM specified\n");
    exit(-1);
  }
  // Load the ROM.
  if (!cart_read(&env->cart, rom_path)) {
    vpi_printf("Cannot load ROM\n");
    exit(-1);
  }

  s_vpi_systf_data data;

  data.type      = vpiSysTask;
  data.tfname    = "$gpu_tick";
  data.calltf    = vpi_gpu_tick;
  data.compiletf = Compile<>;
  data.sizetf    = nullptr;
  data.user_data = (char *)&env->gpu;
  vpi_register_systf(&data);

  data.type      = vpiSysTask;
  data.tfname    = "$gpu_vram_write";
  data.calltf    = vpi_gpu_vram_write;
  data.compiletf = Compile<uint16_t, uint8_t>;
  data.sizetf    = nullptr;
  data.user_data = (char *)&env->gpu;
  vpi_register_systf(&data);

  data.type      = vpiSysTask;
  data.tfname    = "$gpu_set_bgp";
  data.calltf    = vpi_gpu_set_bgp;
  data.compiletf = Compile<uint8_t>;
  data.sizetf    = nullptr;
  data.user_data = (char *)&env->gpu;
  vpi_register_systf(&data);

  data.type      = vpiSysFunc;
  data.tfname    = "$gpu_get_bgp";
  data.calltf    = vpi_gpu_get_bgp;
  data.compiletf = Compile<>;
  data.sizetf    = Size<uint8_t>;
  data.user_data = (char *)&env->gpu;
  vpi_register_systf(&data);

  data.type      = vpiSysTask;
  data.tfname    = "$gpu_set_scroll_y";
  data.calltf    = vpi_gpu_set_scroll_y;
  data.compiletf = Compile<uint8_t>;
  data.sizetf    = nullptr;
  data.user_data = (char *)&env->gpu;
  vpi_register_systf(&data);

  data.type      = vpiSysFunc;
  data.tfname    = "$gpu_get_scroll_y";
  data.calltf    = vpi_gpu_get_scroll_y;
  data.compiletf = Compile<>;
  data.sizetf    = Size<uint8_t>;
  data.user_data = (char *)&env->gpu;
  vpi_register_systf(&data);

  data.type      = vpiSysTask;
  data.tfname    = "$gpu_set_lcdc";
  data.calltf    = vpi_gpu_set_lcdc;
  data.compiletf = Compile<uint8_t>;
  data.sizetf    = nullptr;
  data.user_data = (char *)&env->gpu;
  vpi_register_systf(&data);

  data.type      = vpiSysFunc;
  data.tfname    = "$gpu_get_lcdc";
  data.calltf    = vpi_gpu_get_lcdc;
  data.compiletf = Compile<>;
  data.sizetf    = Size<uint8_t>;
  data.user_data = (char *)&env->gpu;
  vpi_register_systf(&data);

  data.type      = vpiSysFunc;
  data.tfname    = "$gpu_get_ly";
  data.calltf    = vpi_gpu_get_ly;
  data.compiletf = Compile<>;
  data.sizetf    = Size<uint8_t>;
  data.user_data = (char *)&env->gpu;
  vpi_register_systf(&data);

  data.type      = vpiSysTask;
  data.tfname    = "$cart_write";
  data.calltf    = vpi_cart_write;
  data.compiletf = Compile<uint16_t, uint8_t>;
  data.sizetf    = nullptr;
  data.user_data = (char *)&env->cart;
  vpi_register_systf(&data);

  data.type      = vpiSysFunc;
  data.tfname    = "$cart_read";
  data.calltf    = vpi_cart_read;
  data.compiletf = Compile<uint16_t>;
  data.sizetf    = Size<uint8_t>;
  data.user_data = (char *)&env->cart;
  vpi_register_systf(&data);
}

typedef void (*register_fn)(void);

extern "C" {

register_fn vlog_startup_routines[] = {
  gbenv_register,
  NULL
};

}
