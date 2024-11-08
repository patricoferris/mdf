open Astring

type t = { doc : Cmarkit.Doc.t }

let v doc = { doc }
let markdown t = t.doc

let with_width ?(width_offset = 0) size s =
  (* A string may not start at the pages 0 width.
     We keep track of this width using [width_offset].
  *)
  let rec loop (width_offset, last_cut_point, last_space, lines) s =
    match String.find ~start:(last_space + 1) (Char.equal ' ') s with
    | None ->
        let last_line =
          String.sub ~start:last_cut_point s |> String.Sub.to_string
        in
        let width =
          Pdfstandard14.textwidth false Pdftext.WinAnsiEncoding
            Pdftext.TimesItalic last_line
        in
        (width_offset + width, List.rev (last_line :: lines))
    | Some next_space ->
        (* Note: next_space + 1 in order to include the whitespace in the line
           that is broken, rather than starting the next line with a blankspace *)
        let line =
          String.sub ~start:last_cut_point ~stop:(next_space + 1) s
          |> String.Sub.to_string
        in
        let width =
          Pdfstandard14.textwidth false Pdftext.MacRomanEncoding
            Pdftext.TimesItalic line
        in
        if width_offset + width > size then
          loop (0, next_space + 1, next_space, line :: lines) s
        else loop (width_offset, last_cut_point, next_space, lines) s
  in
  loop (width_offset, 0, -1, []) s |> fun (l, v) ->
  (l, List.concat_map (fun v -> Pdsl.[ txt v; newline ]) v)
  |> (* Remove last newline *)
  fun (l, v) -> (l, List.rev v |> List.tl |> List.rev)

let ops_with_width ~width ops =
  (* We collect subgroups of text to be able to apply cross
     operation text widths. For example: we might have some
     text and then a codespan which is separate. We would still
     like to break inside the codespan if needed. *)
  let add_text op = function
    | [] -> [ `Text [ op ] ]
    | `Text ops :: rest -> `Text (op :: ops) :: rest
    | `Other ops :: rest -> `Text [ op ] :: `Other ops :: rest
  in
  let add_other op = function
    | [] -> [ `Other [ op ] ]
    | `Other ops :: rest -> `Other (op :: ops) :: rest
    | `Text ops :: rest -> `Other [ op ] :: `Text ops :: rest
  in
  let rec collect_text_subgroups acc = function
    | ((Pdfops.Op_Tj _ | Pdfops.Op_Tf _) as v) :: rest ->
        collect_text_subgroups (add_text v acc) rest
    | other :: rest -> collect_text_subgroups (add_other other acc) rest
    | [] ->
        List.map
          (function
            | `Text ops -> `Text (List.rev ops)
            | `Other ops -> `Other (List.rev ops))
          acc
        |> List.rev
  in
  let groups = collect_text_subgroups [] ops in
  let apply_widths offset = function
    | `Other ops -> ops
    | `Text ops ->
        let rec loop width_offset = function
          | [] -> []
          | Pdfops.Op_Tj s :: next ->
              let new_off, ops = with_width ~width_offset width s in
              ops @ loop new_off next
          | (Pdfops.Op_Tf _ as v) :: next -> v :: loop width_offset next
          | _ -> assert false
        in
        loop offset ops
  in
  List.concat_map (apply_widths 0) groups

let fold ~inline ~block doc : Pdsl.t list =
  let inline _folder acc il =
    let v = inline il in
    Cmarkit.Folder.ret (v @ acc)
  in
  let block _folder acc il =
    let v = block il in
    Cmarkit.Folder.ret (v @ acc)
  in
  let fold = Cmarkit.Folder.make ~inline ~block () in
  Cmarkit.Folder.fold_doc fold [] doc

module Theme = struct
  type t = {
    font : Pdsl.name;
    code_font : Pdsl.name;
    h1_size : float;
    default_size : float;
    width : int;
  }
end

module Default (T : sig
  val theme : Theme.t
end) =
struct
  let rec inline (v : Cmarkit.Inline.t) =
    match v with
    | Cmarkit.Inline.Text (s, _) -> Pdsl.[ txt s ]
    | Cmarkit.Inline.Inlines (ils, _) -> List.concat_map inline ils
    | Cmarkit.Inline.Strong_emphasis (i, _) ->
        let v = Cmarkit.Inline.Emphasis.inline i |> inline in
        Pdsl.set_text_font T.theme.font T.theme.default_size :: v
    | Cmarkit.Inline.Code_span (span, _) ->
        let text = Cmarkit.Inline.Code_span.code span in
        Pdsl.
          [
            set_text_font T.theme.code_font T.theme.default_size;
            txt text;
            set_text_font T.theme.font T.theme.default_size;
          ]
    | _ -> []

  let rec block (v : Cmarkit.Block.t) =
    match v with
    | Cmarkit.Block.Heading (h, _meta) ->
        let level = Cmarkit.Block.Heading.level h in
        let level_to_font = function
          | 1 -> Pdsl.set_text_font T.theme.font T.theme.h1_size
          | _ -> Pdsl.set_text_font T.theme.font T.theme.default_size
        in
        let title = inline (Cmarkit.Block.Heading.inline h) in
        let title = level_to_font level :: title in
        ops_with_width ~width:T.theme.width title @ [ Pdsl.break 20. ]
    | Cmarkit.Block.Blocks (lst, _meta) -> List.concat_map block lst
    | Cmarkit.Block.Paragraph (p, _meta) ->
        let inline = Cmarkit.Block.Paragraph.inline p |> inline in
        let inline =
          Pdsl.set_text_font T.theme.font T.theme.default_size :: inline
        in
        ops_with_width ~width:T.theme.width inline @ [ Pdsl.break 20. ]
    | Cmarkit.Block.Code_block (cb, _) ->
        let lines = Cmarkit.Block.Code_block.code cb in
        let lines = List.map Cmarkit.Block_line.to_string lines in
        ops_with_width ~width:T.theme.width
        @@ Pdsl.set_text_font T.theme.code_font T.theme.default_size
           :: List.map Pdsl.txt lines
        @ [ Pdsl.break 20. ]
    | _ -> []
end

let default_theme ~font ~code_font =
  Theme.
    {
      font = Pdsl.name font;
      code_font = Pdsl.name code_font;
      default_size = 10.;
      h1_size = 22.;
      width = 20_000;
    }

let to_pdf v =
  let doc = markdown v in
  let font = "F0" and code_font = "F1" in
  let module T = struct
    let theme = default_theme ~font ~code_font
  end in
  let module D = Default (T) in
  let ops = fold ~inline:D.inline ~block:D.block doc in
  let ops =
    Pdsl.transform [ Pdftransform.Translate (50., 770.) ]
    :: Pdsl.set_leading 11. :: Pdsl.with_txt ops
  in
  let main_font = Font.times_new_roman Regular in
  let code_main_font = Font.courier Regular in
  let page =
    {
      (Pdfpage.blankpage Pdfpaper.a4) with
      Pdfpage.content = [ Pdfops.stream_of_ops ops ];
      Pdfpage.resources =
        Pdf.Dictionary
          [
            ( "/Font",
              Pdf.Dictionary
                [
                  ("/F0", Font.to_pdf_object main_font);
                  ("/F1", Font.to_pdf_object code_main_font);
                ] );
          ];
    }
  in
  let pdf, pageroot = Pdfpage.add_pagetree [ page ] (Pdf.empty ()) in
  let pdf = Pdfpage.add_root pageroot [] pdf in
  pdf
