
module RT

using UUIDs

using ..Utils


export Token, EOF, WhiteSpace, Phrase, Identifier, LitNumber
using ..Femtoparsec


export SrcPosition, SrcRange, SrcLocation, NilPosition, NilRange
export SyntaxError, SyntaxWarn
export Diagnosis, diagWhere, Error, Warn, Hint
using ..Diag


export GoDuckExpr, expr_src
include("./RT/expr.jl")

include("./RT/vocab.jl")

export UAID, AttrKey, KwArgs, ArgsPack, take, @uaid
include("./RT/attrs.jl")

include("./RT/ast.jl")

export Entity, nil
include("./RT/entity.jl")

include("./RT/scope.jl")

include("./RT/modu.jl")

include("./RT/exec.jl")


export parseGoDuck
include("./RT/Parser.jl")
using .Parser


end # module GoDuck.RT
