(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

open Types

type gpu_state =
  | HBlank of int
  | VBlank of int
  | OAMRead of int
  | VRAMRead of int

type t =
  { scroll_x: u8
  ; scroll_y: u8
  ; lcd_enable: bool
  ; lcd_window_tile_map: u16
  ; lcd_window_display: bool
  ; lcd_bg_window_tile_data: u16
  ; lcd_bg_window_tile_map: u16
  ; lcd_bg_window_display: bool
  ; lcd_bg_palette: Graphics.color array
  ; lcd_sprite_size: u8
  ; lcd_sprite_display: bool
  ; lcd_ly: int
  ; lcd_wx: int
  ; lcd_wy: int
  ; vram: bytes
  ; state: gpu_state
  ; time: float
  }

type i =
  { igStat: bool
  ; igVBlank: bool
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
    let addr = gpu.lcd_bg_window_tile_map + (y lsr 3) * 32 + (x lsr 3) in
    let tile = Char.code (Bytes.get gpu.vram addr) in
    let l = y land 7 in
    let c = 7 - (x land 7) in
    let pixel = get_tile_pixel tile l c gpu.lcd_bg_window_tile_data in
    gpu.lcd_bg_palette.(pixel)
  in

  let wnd_pixel y x =
    Graphics.red
  in

  let ly = gpu.lcd_ly - 1 in
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
    failwith "lcd_sprite_display";
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
  ; lcd_enable = false
  ; lcd_window_tile_map = 0x1800
  ; lcd_window_display = false
  ; lcd_bg_window_tile_data = 0x0800
  ; lcd_bg_window_tile_map = 0x1800
  ; lcd_bg_window_display = false
  ; lcd_bg_palette = Array.init 4 (fun _ -> c00)
  ; lcd_sprite_size = 8
  ; lcd_sprite_display = false
  ; lcd_ly = 0
  ; lcd_wx = 0
  ; lcd_wy = 0
  ; vram = Bytes.make 0x2000 (Char.chr 0)
  ; state = HBlank 0
  ; time = Unix.gettimeofday ()
  }

let vram_read _gpu _addr =
  failwith "not implemented"

let vram_write gpu addr v =
  let vram = Bytes.copy gpu.vram in
  Bytes.set vram addr (Char.chr v);
  Some { gpu with vram }

let set_bgp gpu v =
  let lcd_bg_palette =
    Array.init 4 (fun i ->
      match (v lsr (i * 2)) land 0x3 with
      | 0 -> c00
      | 1 -> c01
      | 2 -> c10
      | 3 -> c11
      | _ -> failwith "unreachable"
    )
  in
  Some { gpu with lcd_bg_palette }

let set_scroll_y gpu scroll_y = Some { gpu with scroll_y }
let get_scroll_y gpu = Some gpu.scroll_y

let set_scroll_x gpu scroll_x = Some { gpu with scroll_x }
let get_scroll_x gpu = Some gpu.scroll_x

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
  Some { gpu with
    lcd_enable;
    lcd_window_tile_map;
    lcd_window_display;
    lcd_bg_window_tile_data;
    lcd_bg_window_tile_map;
    lcd_bg_window_display;
    lcd_sprite_size;
    lcd_sprite_display;
  }


let get_ly gpu = Some gpu.lcd_ly

let no_i = (false, false)

let tick gpu =
  match gpu.state with
  | HBlank n when n < 51 ->
    Some (no_i, { gpu with state = HBlank (n + 1) })
  | HBlank n ->
    let lcd_ly = gpu.lcd_ly + 1 in
    if lcd_ly > rows then begin
      (* VBlank here *)
      let gpu = { gpu with lcd_ly; state = VBlank 0 } in
      graphics_vblank gpu;
      Some (no_i, gpu)
    end else begin
      (* Scanline here *)
      let gpu = { gpu with lcd_ly; state = HBlank 0 } in
      graphics_scanline gpu;
      Some (no_i, gpu)
    end

  | VBlank n when n < 114 ->
    Some (no_i, { gpu with state = VBlank (n + 1) })
  | VBlank _ ->
    if gpu.lcd_ly >= rows + 10 then
      Some (no_i, { gpu with state = OAMRead 0; lcd_ly = 0 })
    else
      Some (no_i, { gpu with state = VBlank 0; lcd_ly = gpu.lcd_ly + 1 })

  | OAMRead n when n < 20 ->
    Some (no_i, { gpu with state = OAMRead (n + 1) })
  | OAMRead _ ->
    (* HBlank here *)
    Some (no_i, { gpu with state = VRAMRead 0 })

  | VRAMRead n when n < 43 ->
    Some (no_i, { gpu with state = VRAMRead (n + 1) })
  | VRAMRead _ ->
    (* Frame done here - wait for it to finish. *)
    Some (no_i, { gpu with state = HBlank 0; time = Unix.gettimeofday () })
