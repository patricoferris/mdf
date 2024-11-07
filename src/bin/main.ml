let () =
  let file = Sys.argv.(1) in
  let document = In_channel.with_open_bin file In_channel.input_all in
  let pdf = Mdf.to_pdf (Mdf.v @@ Cmarkit.Doc.of_string document) in
  Pdfwrite.pdf_to_file pdf "main.pdf"
