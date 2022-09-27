
module RT

using UUIDs

using ..Utils


export SrcPosition, SrcRange, SrcLocation, NilPosition, NilRange
export Token, EOF, WhiteSpace, Phrase, Identifier, LitNumber
export Diagnosis, diagWhere, Error, Warn, Hint
include("./RT/roots.jl")

export GoDuckExpr, expr_src, SynNode
include("./RT/expr.jl")

include("./RT/vocab.jl")

export UAID, AttrKey, KwArgs, ArgsPack, take, @uaid
include("./RT/attrs.jl")

export Entity, nil
include("./RT/entity.jl")

include("./RT/scope.jl")

include("./RT/modu.jl")

include("./RT/exec.jl")


end # module GoDuck.RT
