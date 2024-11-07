type t = Pdfops.t
(** A PDF Graphic operator *)

val transform : Pdftransform.transform -> t
(** Apply a transform *)

val begin_txt : t
(** Begin a text object *)

val end_txt : t
(** End a text object *)

val set_text_font : string -> float -> t
(** Sets the text font name and size *)

val set_leading : float -> t
(** Set the text leading *)

val txt : string -> t
(** Show text *)

val txtf : ('a, unit, string, t) format4 -> 'a
(** The same as {! txt} but with a format string *)

val newline : t
(** Insert a newline *)
