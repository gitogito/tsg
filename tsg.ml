open Printf
open! ExtLib

module MyGraphics = struct
  type t = {
    mutable x1 : float;
    mutable x2 : float;
    mutable y1 : float;
    mutable y2 : float;
  }

  let to_int_position self x y =
    let nx = int_of_float ((x -. self.x1) /. (self.x2 -. self.x1) *. float (Graphics.size_x ())) in
    let ny = int_of_float ((y -. self.y1) /. (self.y2 -. self.y1) *. float (Graphics.size_y ())) in
    (nx, ny)

  let to_float_position self nx ny =
    let x = self.x1 +. float nx *. (self.x2 -. self.x1) /. float (Graphics.size_x ()) in
    let y = self.y1 +. float ny *. (self.y2 -. self.y1) /. float (Graphics.size_y ()) in
    (x, y)

  let init ?(x1 = 0.0) ?(x2 = 1.0) ?(y1 = 0.0) ?(y2 = 1.0) geometry =
    Graphics.open_graph (" " ^ geometry);
    { x1; x2; y1; y2 }

  let set_x1x2y1y2 self ~x1 ~x2 ~y1 ~y2 =
    self.x1 <- x1;
    self.x2 <- x2;
    self.y1 <- y1;
    self.y2 <- y2

  let moveto self x y =
    let nx, ny = to_int_position self x y in
    Graphics.moveto nx ny

  let lineto self x y =
    let nx, ny = to_int_position self x y in
    Graphics.lineto nx ny

  let draw_poly self ary =
    let ary = Array.map (fun (x, y) -> to_int_position self x y) ary in
    Graphics.draw_poly ary

  let draw_poly_line self ary =
    let ary = Array.map (fun (x, y) -> to_int_position self x y) ary in
    Graphics.draw_poly_line ary
end

let min_of_list l =
  List.fold_left (fun a accum -> if a < accum then a else accum) (List.hd l) l

let max_of_list l =
  List.fold_left (fun a accum -> if a > accum then a else accum) (List.hd l) l

let transpose ary =
  let nx = Array.length ary in
  let ny = Array.length ary.(0) in
  Array.init ny
    (fun j ->
       Array.init nx (fun i-> float_of_string ary.(i).(j)))

let load_data ic =
  let sep = Re_pcre.regexp "\\s+" in
  input_list ic
  |> List.map
       (fun s ->
          Re_pcre.split ~rex:sep s)
  |> List.map Array.of_list
  |> Array.of_list
  |> transpose

let rgb_of_hsv h s v =
  if s = 0.0 then
    (v, v, v)
  else begin
    let hi = int_of_float (h /. 60.0) in
    let f = h /. 60.0 -. float hi in
    let p = v *. (1.0 -. s) in
    let q = v *. (1.0 -. f *. s) in
    let t = v *. (1.0 -. (1.0 -. f) *. s) in

    match hi with
    | 0 | 6 -> (v, t, p)
    | 1 -> (q, v, p)
    | 2 -> (p, v, t)
    | 3 -> (p, q, v)
    | 4 -> (t, p, v)
    | 5 -> (v, p, q)
    | _ ->
        failwith "bug?"
  end

let array_combine aary bary =
  let na = Array.length aary in
  let nb = Array.length bary in
  if na <> nb then raise (invalid_arg "array_combine");
  Array.mapi (fun i a -> (a, bary.(i))) aary

let get_axis_with_n mi ma n =
  let base = 10.0 ** float n in
  let da = if mi < 0.0 then -1e-9 else +1e-9 in
  let na = floor (mi /. base *. (1.0 +. da)) in      (* multiply a number slightly beyond 1.0 to avoid an error *)
  let db = if ma < 0.0 then +1e-9 else -1e-9 in
  let nb = ceil (ma /. base *. (1.0 +. db)) in       (* multiply a number slightly below 1.0 to avoid an error *)
  let a = base *. na in
  let b = base *. nb in
  (a, b, base)

let rec adjust_base a b base =
  let k = (b -. a) /. base in
  if k > 10.0 then
    adjust_base a b (base *. 2.0)
  else if k >= 4.0 then
    (a, b, base)
  else
    adjust_base a b (base /. 2.0)

let get_axis mi ma =
  if mi = ma then
    (0.0, mi *. 2.0, 1.0)
  else begin
    let n = truncate (ceil (log10 (ma -. mi))) in
    let a, b, base = get_axis_with_n mi ma n in
    let k = (ma -. mi) /. (b -. a) in
    if k > 0.5 then begin
      adjust_base a b (base /. 10.0)
    end else begin
      let a, b, base = get_axis_with_n mi ma (n - 1) in
      adjust_base a b base
    end
  end

let get_axis_xy xary ysary =
  let xmin, xmax =
    Array.fold_left
      (fun (xmin, xmax) x ->
         let xmin = if x < xmin then x else xmin in
         let xmax = if x > xmax then x else xmax in
         (xmin, xmax))
      (max_float, -.max_float)
      xary
  in
  let ymin, ymax =
    Array.fold_left
      (fun (ymin, ymax) rows ->
         Array.fold_left
           (fun (ymin, ymax) y ->
              let ymin = if y < ymin then y else ymin in
              let ymax = if y > ymax then y else ymax in
              (ymin, ymax))
           (ymin, ymax)
           rows)
      (max_float, -.max_float)
     ysary
  in
  let xmin, xmax, xstep = get_axis xmin xmax in
  let ymin, ymax, ystep = get_axis ymin ymax in
  (xmin, xmax, xstep, ymin, ymax, ystep)

let draw_frame g xmin xmax xstep ymin ymax ystep =
  let nx = truncate ((xmax -. xmin) /. xstep +. 0.5) in
  let ny = truncate ((ymax -. ymin) /. ystep +. 0.5) in
  for i = 0 to nx do
    let x = xmin +. xstep *. float i in
    MyGraphics.moveto g x ymin;
    Graphics.set_color (Graphics.rgb 210 210 210);
    MyGraphics.lineto g x ymax;

    let s = sprintf "%g" x in
    let tsx, tsy = Graphics.text_size s in
    MyGraphics.moveto g x ymin;
    Graphics.rmoveto (-tsx / 2) (-10 - tsy);
    Graphics.set_color (Graphics.rgb 0 0 0);
    Graphics.draw_string s
  done;
  for i = 0 to ny do
    let y = ymin +. ystep *. float i in
    MyGraphics.moveto g xmin y;
    Graphics.set_color (Graphics.rgb 210 210 210);
    MyGraphics.lineto g xmax y;

    let s = sprintf "%g" y in
    let tsx, tsy = Graphics.text_size s in
    MyGraphics.moveto g xmin y;
    Graphics.rmoveto (-10 - tsx) (-tsy / 2);
    Graphics.set_color (Graphics.rgb 0 0 0);
    Graphics.draw_string s
  done

let reduce_brightness (r, g, b) =
  let a = 0.7 in
  (truncate (float r *. a),
   truncate (float g *. a),
   truncate (float b *. a))

let rainbow i n =
  let h = 360.0 *. float i /. float (n + 1) in
  let s = 1.0 in
  let v = 1.0 in
  let red, green, blue = rgb_of_hsv h s v in
  let red, green, blue = truncate (red *. 256.0 *. 0.999),
                         truncate (green *. 256.0 *. 0.999),
                         truncate (blue *. 256.0 *. 0.999) in
  (red, green, blue)

let () =
  let opt_geometry = ref "" in
  let opt_rest_args = ref [] in
  let speclist = [
    ("-geometry", Arg.Set_string opt_geometry, "geometry");
    ("--", Arg.Rest (fun s -> opt_rest_args := !opt_rest_args @ [s]), "rest");
  ] in
  let anon_args = ref [] in
  Arg.parse
    speclist
    (fun s ->
       anon_args := !anon_args @ [s])
    "Usage:";

  let g = MyGraphics.init !opt_geometry in
  Graphics.auto_synchronize false;
  let ary = load_data stdin in
  let xary = ary.(0) in
  let ysary = Array.sub ary 1 (Array.length ary - 1) in
  let xmin, xmax, xstep, ymin, ymax, ystep = get_axis_xy xary ysary in
  MyGraphics.set_x1x2y1y2
    g
    ~x1:(xmin -. (xmax -. xmin) /. 10.0)
    ~x2:(xmax +. (xmax -. xmin) /. 20.0)
    ~y1:(ymin -. (ymax -. ymin) /. 10.0)
    ~y2:(ymax +. (ymax -. ymin) /. 20.0);
  draw_frame g xmin xmax xstep ymin ymax ystep;
  Array.iteri
    (fun i yary ->
       let xyary = array_combine xary yary in
       let red, green, blue = rainbow i (Array.length ysary - 1) |> reduce_brightness in
       Graphics.set_color (Graphics.rgb red green blue);
       MyGraphics.draw_poly_line g xyary)
    ysary;
  Graphics.synchronize ();
  while true do
    let status = Graphics.wait_next_event [Graphics.Key_pressed; Graphics.Button_down] in
    begin match status.Graphics.key with
    | 'q' | 'Q' | '\027' -> exit 0
    | _ -> ()
    end;
    if status.Graphics.button then begin
      let x, y = MyGraphics.to_float_position g status.Graphics.mouse_x status.Graphics.mouse_y in
      printf "%g, %g\n" x y;
      flush stdout
    end
  done
