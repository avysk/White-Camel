type gametree = private Gametree of (Types.position * (gametree list Lazy.t))

val create_gametree : Types.position -> gametree
