type t = Pdfops.t
(** A PDF Graphic operator *)

type name = private string
(** Names of objects *)

val name : string -> name
(** Create a name *)

val transform : Pdftransform.transform -> t
(** Apply a transform *)

val begin_txt : t
(** Begin a text object *)

val end_txt : t
(** End a text object *)

val with_txt : t list -> t list
(** [with_txt ops] wraps [ops] with [begin_txt] and [end_txt] *)

val set_text_font : name -> float -> t
(** Sets the text font name and size *)

val set_leading : float -> t
(** Set the text leading *)

val txt : string -> t
(** Show text *)

val txtf : ('a, unit, string, t) format4 -> 'a
(** The same as {! txt} but with a format string *)

val newline : t
(** Insert a newline, this will use whatever the {! set_leading}
    is. *)

val break : float -> t
(** This will insert a break of some amount of unscaled text space units *)
