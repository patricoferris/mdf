open Astring

type t = { doc : Cmarkit.Doc.t }

let v doc = { doc }
let markdown t = t.doc

let with_width size s =
  let rec loop (last_cut_point, last_space, lines) s =
    match String.find ~start:(last_space + 1) (Char.equal ' ') s with
    | None ->
        let last_line =
          String.sub ~start:last_cut_point s
          |> String.Sub.to_string |> String.trim
        in
        List.rev (last_line :: lines)
    | Some next_space ->
        let line =
          String.sub ~start:last_cut_point ~stop:next_space s
          |> String.Sub.to_string
        in
        let width =
          Pdfstandard14.textwidth false Pdftext.MacRomanEncoding
            Pdftext.TimesItalic line
        in
        if width > size then loop (next_space, next_space, line :: lines) s
        else loop (last_cut_point, next_space, lines) s
  in
  loop (0, -1, []) s |> List.concat_map (fun v -> Pdsl.[ txt v; newline ])

let to_pdf v =
  let doc_string = markdown v |> Cmarkit_commonmark.of_doc in
  let font =
    Pdf.Dictionary
      [
        ("/Type", Pdf.Name "/Font");
        ("/Subtype", Pdf.Name "/Type1");
        ("/BaseFont", Pdf.Name "/Times-Italic");
      ]
  and ops =
    Pdsl.(
      [
        transform [ Pdftransform.Translate (50., 770.) ];
        begin_txt;
        set_leading 14.;
        set_text_font "/F0" 12.;
      ]
      @ with_width 30_000 doc_string
      @ [ newline; txt "Some more text"; end_txt ])
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
