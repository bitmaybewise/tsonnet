module Env =
  struct
    type t = string
    let compare = String.compare
  end

module Map = Map.Make(Env)

let empty = Map.empty
