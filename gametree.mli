type gametree = private Gametree of (Types.position * (gametree list lazy_t))

val create_gametree : Types.position -> gametree
