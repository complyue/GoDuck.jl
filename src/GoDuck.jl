module GoDuck


export @void, List, EmptyList, Cons
include("./GoDuck/Utils.jl")
using .Utils


export SrcPosition, SrcRange, SrcLocation, NilPosition, NilRange
export SyntaxError, SyntaxWarn
export Diagnosis, diagWhere, Error, Warn, Hint
include("./GoDuck/Diag.jl")


export LexInProgress, femtoParse, Unmet, Incomplete
include("./GoDuck/Femtoparsec.jl")
using .Femtoparsec


export GoDuckExpr, expr_src, KwArgs, ArgsPack, @uaid, Entity, nil
include("./GoDuck/RT.jl")
using .RT: GoDuckExpr, expr_src, KwArgs, ArgsPack, @uaid, Entity, nil


end # module GoDuck
