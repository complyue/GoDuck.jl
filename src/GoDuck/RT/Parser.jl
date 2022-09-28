
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
  steps = GoDuckExpr[]
  @sc
  (_, first_span) = @tokenAhead()
  while true
    if @tokAhead() isa EOF
      full_span = SrcLocation(first_span.start, @lastPos())
      if isempty(steps)
        return ExpectTerms(full_span, "Some Toplevel Expression")
      end
      if length(steps) == 1
        return steps[1]
      end
      return Sequential(steps, full_span)
    end
    push!(steps, @choiceFor("Toplevel Expression",
      @parse(importExpr()),
      @parse(arithExpr()),
    ))
  end
  @assert false "bug: never encounter EOF?!"
end


@lexeme function importExpr()::Union{ImportExpr,Unmet}
  @expectToken AlphaId("import")
  @sc
  spec = @expectTokenOf LitStr
  # ...
  return spec.value

end



end # module GoDuck.Compiler.Parser
