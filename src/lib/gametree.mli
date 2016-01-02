open Types

type gametree = private Gametree of (position * (gametree list Lazy.t))

val create_gametree : position -> gametree
val get_evaluation : gametree -> eval_t
