(** {1 PDF of Markdown }*)

type t
(** A document *)

val v : Cmarkit.Doc.t -> t
(** Constructor for documents *)

val markdown : t -> Cmarkit.Doc.t
(** The Cmarkit markdown document content *)

val to_pdf : t -> Pdf.t
(** Convert a document to a PDF *)
