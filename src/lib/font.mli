type t
(** A font *)

type kind =
  | Italic
  | Bold
  | BoldItalic
  | Regular
  | Oblique  (** Font variations *)

val times_new_roman : kind -> t
(** Times New Roman font *)

val courier : kind -> t
(** A courier font *)

val to_pdf_object : t -> Pdf.pdfobject
(** A pdf object for passing to a PDF document as a resource *)
