type t = Pdfops.t

let transform t = Pdfops.Op_cm (Pdftransform.matrix_of_transform t)
let begin_txt = Pdfops.Op_BT
let end_txt = Pdfops.Op_ET
let set_text_font f s = Pdfops.Op_Tf (f, s)
let set_leading s = Pdfops.Op_TL s
let txt s = Pdfops.Op_Tj s
let txtf fmt = Printf.ksprintf (fun s -> Pdfops.Op_Tj s) fmt
let newline = Pdfops.Op_T'
