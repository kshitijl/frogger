(** The grid system:

    0. The positions of all objects are snapped onto a coarse grid.
    1. The frog  is 1x1
    2. Every car is 1x1
    3. Every log is 3x1
*)

(** The playable area of the screen will be referred to as the [board]. *)
module Board : sig

  (** Every row of the game board is one of these three kinds. *)
  module Row : sig
    type t =
      | Safe_strip
      | Road
      | River
  end

  val num_cols : int

  (** The first and last rows are guaranteed to be [Safe_strip]s. *)
  val rows : Row.t list
end

(** This is a position in the grid system of the game, not in screen pixels or
    anything else like that. *)
module Position : sig
  type t =
    { x : int
    ; y : int
    } [@@deriving fields]

  val create : x:int -> y:int -> t
end

(** Note that all these images have unit height. *)
module Image : sig
  type t = string

  val frog_up     : t
  val frog_down   : t
  val frog_left   : t
  val frog_right  : t

  val frog_on_log : t

  val car1_left   : t
  val car1_right  : t
  val car2_left   : t
  val car2_right  : t

  val log         : t

  val skull_and_crossbones : t
end

module Display_list : sig
  (** The [Display_command] [(image, pos)] represents a command to draw [image]
      with its leftmost grid point at [pos].

      Note that logs can be partially off the screen. This is represented by
      wrapping a wide image around the edge of the game board if a part of it
      would have ended up off the screen. Passing in a [Position.t] with a
      negative coordinate is an error. *)
  module Display_command : sig
    type nonrec t = Image.t * Position.t
  end

  type t = Display_command.t list
end

module Key : sig
  type t =
    | Arrow_up
    | Arrow_down
    | Arrow_left
    | Arrow_right
end

module Event : sig
  type t =
    | Tick     
    | Keypress of Key.t
end
