
module Parser

export parseGoDuck

export SyntaxError, SyntaxWarn
using ..Diag

using ..RT
using ...Femtoparsec
using ...Femtoparsec.Lexer


"""    parse the specified input stream with a `@parser function`

  NOTE: `f::Function` has to be a `@parser` or `@lexeme` decorated function
  """
parseGoDuck(io::IO, f::Function, args...; kwargs...
)::Tuple{Union{GoDuckExpr,Unmet},Vector{Diagnosis}} =
  femtoParse(LexInProgress(lex(io)), f, args...; kwargs...)

"""    parse the specified string with a `@parser function`

NOTE: `f::Function` has to be a `@parser` or `@lexeme` decorated function
"""
parseGoDuck(str::AbstractString, f::Function, args...; kwargs...
)::Tuple{Union{GoDuckExpr,Unmet},Vector{Diagnosis}} =
  femtoParse(LexInProgress(lex(str)), f, args...; kwargs...)


@lexeme function comma()
  @expectToken OperId(",")
end




end # module GoDuck.Compiler.Parser
