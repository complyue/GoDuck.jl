
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
parseGoDuck(io::IO)::Tuple{Union{GoDuckExpr,Unmet},Vector{Diagnosis}} =
  femtoParse(LexInProgress(lex(io)), topExpr)

"""    parse the specified string with a `@parser function`

NOTE: `f::Function` has to be a `@parser` or `@lexeme` decorated function
"""
parseGoDuck(str::AbstractString)::Tuple{Union{GoDuckExpr,Unmet},Vector{Diagnosis}} =
  femtoParse(LexInProgress(lex(str)), topExpr)


@lexeme function comma()
  @expectToken OperId(",")
end


@parser function topExpr()::Union{GoDuckExpr,Unmet}
  @sc
  @choiceFor("Toplevel Expression",
    @parse(importExpr()),
    @parse(arithExpr()),
  )
end


@lexeme function importExpr()::Union{ImportExpr,Unmet}

end



end # module GoDuck.Compiler.Parser
