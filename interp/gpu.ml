(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

open Types

type gpu_state =
  | ST_HBlank of int
  | ST_VBlank of int
  | ST_OAMRead of int
  | ST_VRAMRead of int

type stat =
  | HBlank
  | VBlank
  | OAM
  | LCD

type t =
  { scroll_x: u8
  ; scroll_y: u8

  ; lcd_enable: bool
  ; lcd_window_tile_map: bool
  ; lcd_window_display: bool
  ; lcd_bg_window_tile_data: bool
  ; lcd_bg_window_tile_map: bool
  ; lcd_bg_window_display: bool
  ; lcd_bg_palette: Graphics.color array
  ; lcd_sprite_size: bool
  ; lcd_sprite_display: bool

  ; lcd_stat_lyc: bool
  ; lcd_stat_oam: bool
  ; lcd_stat_vblank: bool
  ; lcd_stat_hblank: bool
  ; lcd_stat_equ: bool

  ; lcd_obj0_palette: Graphics.color array
  ; lcd_obj1_palette: Graphics.color array

  ; lcd_ly: int
  ; lcd_lyc: int
  ; lcd_wx: int
  ; lcd_wy: int

  ; vram: bytes
  ; oam: bytes

  ; state: gpu_state
  ; time: float
  }

let c00 = Graphics.rgb 0xFF 0xFF 0xFF
let c01 = Graphics.rgb 0xAA 0xAA 0xAA
let c10 = Graphics.rgb 0x66 0x66 0x66
let c11 = Graphics.rgb 0x00 0x00 0x00

let cols = 160
let rows = 144
let scale = 1

let vram =
  Array.init (rows * scale) (fun _ ->
    Array.init (cols * scale) (fun _ -> Graphics.black)
  )

let fill_pixel y x c =
  for dy = 0 to scale - 1 do
    for dx = 0 to scale - 1 do
      vram.(y * scale + dy).(x * scale + dx) <- c
    done
  done

let graphics_init () =
  let mode = Printf.sprintf " %dx%d " (cols * scale) (rows * scale) in
  Graphics.open_graph mode


let graphics_scanline gpu =
  let get_tile_pixel tile l c base =
    let tile = if base = 0x1000 then i8_of_u8 tile else tile in
    let idx = base + (tile lsl 4) + (l lsl 1) in
    let b0 = Char.code (Bytes.get gpu.vram (idx + 0)) in
    let b1 = Char.code (Bytes.get gpu.vram (idx + 1)) in
    let p0 = ((b0 land (1 lsl c)) lsr c) lsl 0 in
    let p1 = ((b1 land (1 lsl c)) lsr c) lsl 1 in
    p0 lor p1
  in

  let bg_pixel y x =
    let map_base = if gpu.lcd_bg_window_tile_map then 0x1C00 else 0x1800 in
    let addr = map_base + (y lsr 3) * 32 + (x lsr 3) in
    let tile = Char.code (Bytes.get gpu.vram addr) in
    let l = y land 7 in
    let c = 7 - (x land 7) in
    let data_base = if gpu.lcd_bg_window_tile_data then 0x0000 else 0x0800 in
    let pixel = get_tile_pixel tile l c data_base in
    gpu.lcd_bg_palette.(pixel)
  in

  let wnd_pixel _y _x =
    failwith "not implemented"
  in

  let ly = gpu.lcd_ly in
  if gpu.lcd_bg_window_display then begin
    let y = (gpu.scroll_y + ly) land 0xFF in
    for lx = 0 to cols - 1 do
      let x = (lx + gpu.scroll_x) land 0xFF in
      fill_pixel ly lx (bg_pixel y x)
    done;

    if gpu.lcd_window_display && ly >= gpu.lcd_wy then begin
      for lx = max 0 (gpu.lcd_wx - 7) to cols - 1 do
        let x = lx - gpu.lcd_wx + 7 in
        let y = ly - gpu.lcd_wy in
        fill_pixel ly lx (wnd_pixel y x)
      done
    end;
  end;

  if gpu.lcd_sprite_display then begin
    failwith "not implemented"
  end

let graphics_vblank gpu =
  (* Display the image *)
  let image = Graphics.make_image vram in
  Graphics.draw_image image 0 0;

  (* Wait until the frame ends *)
  let time = Unix.gettimeofday () in
  let elapsed = time -. gpu.time in
  let remaining = (1.0 /. 59.73) -. elapsed in

  if remaining > 0. && false then begin
    Unix.sleepf remaining
  end


let create () =
  graphics_init ();

  { scroll_x = 0
  ; scroll_y = 0
  (* LCDC *)
  ; lcd_enable = false
  ; lcd_window_tile_map = false
  ; lcd_window_display = false
  ; lcd_bg_window_tile_data = false
  ; lcd_bg_window_tile_map = false
  ; lcd_bg_window_display = false
  ; lcd_bg_palette = Array.init 4 (fun _ -> c00)
  ; lcd_sprite_size = false
  ; lcd_sprite_display = false
  (* STAT *)
  ; lcd_stat_lyc = false
  ; lcd_stat_oam = false
  ; lcd_stat_vblank = false
  ; lcd_stat_hblank = false
  ; lcd_stat_equ = false

  ; lcd_obj0_palette = Array.init 4 (fun _ -> c00)
  ; lcd_obj1_palette = Array.init 4 (fun _ -> c00)

  ; lcd_ly = 0
  ; lcd_lyc = 0
  ; lcd_wx = 0
  ; lcd_wy = 0

  ; vram = Bytes.make 0x2000 (Char.chr 0)
  ; oam = Bytes.make 0xA0 (Char.chr 0)

  ; state = ST_HBlank 0
  ; time = Unix.gettimeofday ()
  }

let vram_read gpu addr =
  Some (Char.code (Bytes.get gpu.vram addr))

let vram_write gpu addr v =
  let vram = Bytes.copy gpu.vram in
  Bytes.set vram addr (Char.chr v);
  Some { gpu with vram }

let oam_read gpu addr =
  Some (Char.code (Bytes.get gpu.oam addr))

let oam_write gpu addr v =
  let oam = Bytes.copy gpu.oam in
  Bytes.set oam addr (Char.chr v);
  Some { gpu with oam }

let mask_to_palette v =
  Array.init 4 (fun i ->
    match (v lsr (i * 2)) land 0x3 with
    | 0 -> c00
    | 1 -> c01
    | 2 -> c10
    | 3 -> c11
    | _ -> failwith "unreachable"
  )

let set_bgp gpu v = Some { gpu with lcd_bg_palette = mask_to_palette v}
let set_obp0 gpu v = Some { gpu with lcd_obj0_palette = mask_to_palette v}
let set_obp1 gpu v = Some { gpu with lcd_obj1_palette = mask_to_palette v}

let set_lyc gpu lcd_lyc =
  Some { gpu with
    lcd_lyc;
    lcd_stat_equ = if gpu.lcd_enable then lcd_lyc = gpu.lcd_ly else gpu.lcd_stat_equ;
  }
let get_lyc gpu =
  Some gpu.lcd_lyc

let set_scroll_y gpu scroll_y = Some { gpu with scroll_y }
let get_scroll_y gpu = Some gpu.scroll_y
let set_scroll_x gpu scroll_x = Some { gpu with scroll_x }
let get_scroll_x gpu = Some gpu.scroll_x

let set_window_x gpu lcd_wx = Some { gpu with lcd_wx }
let get_window_x gpu = Some gpu.lcd_wx
let set_window_y gpu lcd_wy = Some { gpu with lcd_wy }
let get_window_y gpu = Some gpu.lcd_wy

let set_lcdc
  gpu
  lcd_enable
  lcd_window_tile_map
  lcd_window_display
  lcd_bg_window_tile_data
  lcd_bg_window_tile_map
  lcd_bg_window_display
  lcd_sprite_size
  lcd_sprite_display =
  let lcd_ly, lcd_stat_equ, state =
    if lcd_enable && not gpu.lcd_enable then begin
      (* LCD turned on *)
      0, gpu.lcd_lyc = 0, ST_HBlank 0
    end else if not lcd_enable && gpu.lcd_enable then begin
      (* LCD turned off *)
      (match gpu.state with
      | ST_VBlank _ -> ()
      | _ -> failwith "LCD turned out outside V-Sync"
      );
      0, gpu.lcd_stat_equ, ST_HBlank 0
    end else
      (* No change *)
      gpu.lcd_ly, gpu.lcd_stat_equ, gpu.state
  in
  Some { gpu with
    lcd_enable;
    lcd_window_tile_map;
    lcd_window_display;
    lcd_bg_window_tile_data;
    lcd_bg_window_tile_map;
    lcd_bg_window_display;
    lcd_sprite_size;
    lcd_sprite_display;
    lcd_ly;
    lcd_stat_equ;
    state;
  }

let set_stat
  gpu
  lcd_stat_lyc
  lcd_stat_oam
  lcd_stat_vblank
  lcd_stat_hblank =
  Some { gpu with
    lcd_stat_lyc;
    lcd_stat_oam;
    lcd_stat_vblank;
    lcd_stat_hblank;
  }

let get_lcd_enable gpu = gpu.lcd_enable
let get_lcd_window_tile_map gpu = gpu.lcd_window_tile_map
let get_lcd_window_display gpu = gpu.lcd_window_display
let get_lcd_bg_window_tile_data gpu = gpu.lcd_bg_window_tile_data
let get_lcd_bg_window_tile_map gpu = gpu.lcd_bg_window_tile_map
let get_lcd_bg_window_display gpu = gpu.lcd_bg_window_display
let get_lcd_sprite_size gpu = gpu.lcd_sprite_size
let get_lcd_sprite_display gpu = gpu.lcd_sprite_display

let get_lcd_stat_lyc gpu = gpu.lcd_stat_lyc
let get_lcd_stat_oam gpu = gpu.lcd_stat_oam
let get_lcd_stat_vblank gpu = gpu.lcd_stat_vblank
let get_lcd_stat_hblank gpu = gpu.lcd_stat_hblank
let get_lcd_stat_equ gpu = gpu.lcd_stat_equ
let get_lcd_stat_mode gpu =
  match gpu.state with
  | ST_HBlank _ -> HBlank
  | ST_VBlank _ -> VBlank
  | ST_OAMRead _ -> OAM
  | ST_VRAMRead _ -> LCD


let get_ly gpu = Some gpu.lcd_ly

let update gpu =
  match gpu.state with
  | ST_HBlank n when n < 51 ->
    Some (
      (false, false, false, false),
      { gpu with state = ST_HBlank (n + 1) }
    )
  | ST_HBlank _ ->
    let lcd_ly = gpu.lcd_ly + 1 in
    let lcd_stat_equ = lcd_ly = gpu.lcd_lyc in
    if lcd_ly >= 144 then begin
      (* VBlank here *)
      Some (
        (false, true, false, lcd_stat_equ),
        { gpu with lcd_ly; state = ST_VBlank 0; lcd_stat_equ }
      )
    end else begin
      Some (
        (false, false, false, lcd_stat_equ),
        { gpu with lcd_ly; state = ST_OAMRead 0; lcd_stat_equ }
      )
    end

  | ST_VBlank n when n < 114 ->
    Some (
      (false, false, false, false),
      { gpu with state = ST_VBlank (n + 1) }
    )
  | ST_VBlank _ ->
    let ended = gpu.lcd_ly >= 153 in
    if ended then graphics_vblank gpu;
    let lcd_ly = if ended then 0 else gpu.lcd_ly + 1 in
    let state = if ended then ST_OAMRead 0 else ST_VBlank 0 in
    let lcd_stat_equ = gpu.lcd_lyc = lcd_ly in
    Some (
      (false, false, ended, lcd_stat_equ),
      { gpu with lcd_ly; state; lcd_stat_equ }
    )

  | ST_OAMRead n when n < 20 ->
    Some (
      (false, false, false, false),
      { gpu with state = ST_OAMRead (n + 1) }
    )
  | ST_OAMRead _ ->
    (* HBlank here *)
    Some (
      (true, false, false, false),
      { gpu with state = ST_VRAMRead 0 }
    )

  | ST_VRAMRead n when n < 43 ->
    Some (
      (false, false, false, false),
      { gpu with state = ST_VRAMRead (n + 1) }
    )
  | ST_VRAMRead _ ->
    graphics_scanline gpu;
    Some (
      (false, false, false, false),
      { gpu with state = ST_HBlank 0; time = Unix.gettimeofday () }
    )

let tick gpu =
  if gpu.lcd_enable then
    update gpu |> Option.map (fun ((hblank, vblank, oam, equ), gpu) ->
      let int_stat =
        (gpu.lcd_stat_lyc && equ) ||
        (gpu.lcd_stat_hblank && hblank) ||
        (gpu.lcd_stat_vblank && vblank) ||
        (gpu.lcd_stat_oam && oam)
      in
      (int_stat, vblank), gpu
    )
  else
    Some ((false, false), gpu)
