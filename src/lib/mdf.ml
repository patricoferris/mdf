open Astring

type t = { doc : Cmarkit.Doc.t }

let v doc = { doc }
let markdown t = t.doc

let with_width size s =
  let rec loop (last_cut_point, last_space, lines) s =
    match String.find ~start:(last_space + 1) (Char.equal ' ') s with
    | None ->
        let last_line =
          String.sub ~start:last_cut_point s |> String.Sub.to_string
        in
        List.rev (last_line :: lines)
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
        if width > size then loop (next_space + 1, next_space, line :: lines) s
        else loop (last_cut_point, next_space, lines) s
  in
  loop (0, -1, []) s |> List.concat_map (fun v -> Pdsl.[ txt v; newline ])

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

let ops_with_width w =
  List.concat_map (function Pdfops.Op_Tj s -> with_width w s | v -> [ v ])

module Theme = struct
  type t = {
    fontname : Pdsl.name;
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
    | Cmarkit.Inline.Inlines (ils, _) ->
        let rec merge = function
          | Pdfops.Op_Tj s :: Pdfops.Op_Tj s' :: rest ->
              Pdfops.Op_Tj (s ^ " " ^ s') :: merge rest
          | x :: xs -> x :: merge xs
          | [] -> []
        in
        List.concat_map inline ils |> merge
    | Cmarkit.Inline.Strong_emphasis (i, _) ->
        let v = Cmarkit.Inline.Emphasis.inline i |> inline in
        Pdsl.set_text_font T.theme.fontname T.theme.default_size :: v
    | _ -> []

  let rec block (v : Cmarkit.Block.t) =
    match v with
    | Cmarkit.Block.Heading (h, _meta) ->
        let level = Cmarkit.Block.Heading.level h in
        let level_to_font = function
          | 1 -> Pdsl.set_text_font T.theme.fontname T.theme.h1_size
          | _ -> Pdsl.set_text_font T.theme.fontname T.theme.default_size
        in
        let title = inline (Cmarkit.Block.Heading.inline h) in
        let title = level_to_font level :: title in
        ops_with_width T.theme.width title @ [ Pdsl.break 10. ]
    | Cmarkit.Block.Blocks (lst, _meta) -> List.concat_map block lst
    | Cmarkit.Block.Paragraph (p, _meta) ->
        let inline = Cmarkit.Block.Paragraph.inline p |> inline in
        let inline =
          Pdsl.set_text_font T.theme.fontname T.theme.default_size :: inline
        in
        ops_with_width T.theme.width inline
    | _ -> []
end

let default_theme =
  Theme.
    {
      fontname = Pdsl.name "F0";
      default_size = 10.;
      h1_size = 22.;
      width = 20_000;
    }

let to_pdf v =
  let doc = markdown v in
  let module T = struct
    let theme = default_theme
  end in
  let module D = Default (T) in
  let font =
    Pdf.Dictionary
      [
        ("/Type", Pdf.Name "/Font");
        ("/Subtype", Pdf.Name "/Type1");
        ("/BaseFont", Pdf.Name "/Times-Italic");
      ]
  and ops = fold ~inline:D.inline ~block:D.block doc in
  let ops =
    Pdsl.transform [ Pdftransform.Translate (50., 770.) ]
    :: Pdsl.set_leading 11. :: Pdsl.with_txt ops
  in
  let page =
    {
      (Pdfpage.blankpage Pdfpaper.a4) with
      Pdfpage.content = [ Pdfops.stream_of_ops ops ];
      Pdfpage.resources =
        Pdf.Dictionary [ ("/Font", Pdf.Dictionary [ ("/F0", font) ]) ];
    }
  in
  let pdf, pageroot = Pdfpage.add_pagetree [ page ] (Pdf.empty ()) in
  let pdf = Pdfpage.add_root pageroot [] pdf in
  pdf
