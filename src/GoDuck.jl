module GoDuck


export @void, List, EmptyList, Cons
include("./GoDuck/Utils.jl")
using .Utils


export GoDuckExpr, expr_src, KwArgs, ArgsPack, @uaid, Entity, nil
include("./GoDuck/RT.jl")
using .RT: GoDuckExpr, expr_src, KwArgs, ArgsPack, @uaid, Entity, nil


export LexInProgress, femtoParse, Unmet, Incomplete
include("./GoDuck/Femtoparsec.jl")
using .Femtoparsec


end # module GoDuck
