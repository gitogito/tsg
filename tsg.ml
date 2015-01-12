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

let get_axis min max =
  if min >= max then failwith "min >= max";
  let diff = max -. min in
  let n = truncate (log10 diff) in
  let base = 10.0 ** float n in
  let left = float (truncate (min /. base)) *. base in
  let left = if left > min then left -. base else left in
  let right = float (truncate (max /. base)) *. base in
  let right = if right < max then right +. base else right in
  if left > min then failwith "bug (left > min)";
  if right < max then failwith "bug (right < max)";
  (left, right, base)

let get_axis_xy ary =
  let xmin, xmax, ymin, ymax =
    Array.fold_left
      (fun (xmin, xmax, ymin, ymax) (x, y) ->
         let xmin = if x < xmin then x else xmin in
         let xmax = if x > xmax then x else xmax in
         let ymin = if y < ymin then y else ymin in
         let ymax = if y > ymax then y else ymax in
         (xmin, xmax, ymin, ymax))
      (max_float, -.max_float, max_float, -.max_float)
      ary
  in
  let xmin, xmax, xstep = get_axis xmin xmax in
  let ymin, ymax, ystep = get_axis ymin ymax in
  (xmin, xmax, xstep, ymin, ymax, ystep)

let draw_frame g xmin xmax xstep ymin ymax ystep =
  let nx = truncate ((xmax -. xmin) /. xstep) in
  let ny = truncate ((ymax -. ymin) /. ystep) in
  for i = 0 to nx do
    let x = xmin +. xstep *. float i in
    MyGraphics.moveto g x ymin;
    Graphics.set_color (Graphics.rgb 210 210 210);
    MyGraphics.lineto g x ymax;

    let s = sprintf "%g" x in
    let tsx, tsy = Graphics.text_size s in
    MyGraphics.moveto g x ymin;
    Graphics.rmoveto (-tsx / 2) (-tsy * 2);
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
    Graphics.rmoveto (-tsx * 2) (-tsy / 2);
    Graphics.set_color (Graphics.rgb 0 0 0);
    Graphics.draw_string s
  done

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
  let rex = Pcre.regexp "[ \t]+" in
  let ary = input_lines stdin
            |> Enum.map
                 (fun s -> 
                    let xy = Pcre.split ~rex s |> List.map float_of_string in
                    match xy with
                    | [x; y] -> (x, y)
                    | _ -> failwith "invalid input")
            |> Array.of_enum in
  let xmin, xmax, xstep, ymin, ymax, ystep = get_axis_xy ary in
  MyGraphics.set_x1x2y1y2
    g
    ~x1:(xmin -. (xmax -. xmin) /. 10.0)
    ~x2:(xmax +. (xmax -. xmin) /. 10.0)
    ~y1:(ymin -. (ymax -. ymin) /. 10.0)
    ~y2:(ymax +. (ymax -. ymin) /. 10.0);
  draw_frame g xmin xmax xstep ymin ymax ystep;
  Graphics.set_color (Graphics.rgb 0 0 0);
  MyGraphics.draw_poly_line g ary;
  Graphics.synchronize ();
  while true do
    let status = Graphics.wait_next_event [Graphics.Key_pressed; Graphics.Button_down] in
    begin match status.Graphics.key with
    | 'q' | 'Q' -> exit 0
    | _ -> ()
    end;
    if status.Graphics.button then begin
      let x, y = MyGraphics.to_float_position g status.Graphics.mouse_x status.Graphics.mouse_y in
      printf "%g, %g\n" x y;
      flush stdout
    end
  done
