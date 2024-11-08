type source = Stdin | File of string
type sink = Stdout | File of string

let with_source s f =
  match s with Stdin -> f stdin | File fn -> In_channel.with_open_bin fn f

let with_sink s f =
  match s with Stdout -> f stdout | File fn -> Out_channel.with_open_bin fn f

let () =
  let src =
    match Sys.argv.(1) with
    | (exception Invalid_argument _) | "-" -> Stdin
    | fn -> File fn
  in
  let sink =
    match Sys.argv.(2) with
    | exception Invalid_argument _ -> Stdout
    | fn -> File fn
  in
  let document = with_source src In_channel.input_all in
  let pdf = Mdf.to_pdf (Mdf.v @@ Cmarkit.Doc.of_string document) in
  with_sink sink @@ fun oc -> Pdfwrite.pdf_to_channel None false pdf oc
