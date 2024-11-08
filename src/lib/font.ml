type kind = Italic | Bold | BoldItalic | Regular | Oblique
type t = { name : string }

let times_new_roman kind =
  let name =
    match kind with
    | Italic -> "/Times-Italic"
    | Bold -> "/Times-Bold"
    | BoldItalic -> "/Times-BoldItalic"
    | _ -> "/Times-New-Roman-Regular"
  in
  { name }

let courier kind =
  let name =
    match kind with
    | Bold -> "/Courier-Bold"
    | Oblique -> "/Courier-Oblique"
    | _ -> "/Courier"
  in
  { name }

let to_pdf_object t =
  Pdf.Dictionary
    [
      ("/Type", Name "/Font");
      ("/Subtype", Name "/Type1");
      ("/BaseFont", Name t.name);
    ]
