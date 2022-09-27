
module Combinators

export collectBracketed


using ..Femtoparsec
using ..Femtoparsec.Lexer


# TODO: more combinators


# TODO: support customization w.r.t. leading/trailing spaces, comma etc.
@parser function collectBracketed(::Type{T},
  obc::Char, cbc::Char, mod::String,
  # @parser function itemParser()::Union{T,Unmet}
  itemParser::Function, args...; kwargs...
)::Union{Vector{T},Unmet} where {T}
  (tk, span) = @nextToken()
  otk, ctk = Open(obc, mod), Close(cbc, mod)
  if tk != otk
    return ExpectBracketOpen(span, obc, mod)
  end
  @sc
  coll = T[]
  properlyClosed = false
  while true
    if @tokAhead() == ctk
      @nextTok()
      @sc
      properlyClosed = true
      break
    end
    if @lastTok() == ctk
      @sc
      properlyClosed = true
      break
    end
    _, spanAhead = @tokenAhead()
    p!result::Union{T,Unmet} = @parse itemParser(args...; kwargs...)
    if p!result isa Unmet # unmet
      _, spanAhead1 = @tokenAhead()
      if spanAhead1 == spanAhead # no progress after parsing the item, most prolly it has bt enabled
        # consume one more lexeme to make some progress, or we'll loop infinitely
        (tk, span) = @nextToken()
        if tk isa EOF
          break
        else
          @sc
          # diag as a problem on that victim token
          # TODO: improve term name, make it customizable?
          @diagWith ExpectTerms(span, string(nameof(T)))
        end
      else # some token(s) consumed and diagnosis given, log that
        @diagWith p!result # report the problem
      end
      if @lastTok() isa EOF # tokens exhausted
        break
      end
    else
      push!(coll, p!result) # TODO: document/improve type safety here
    end
  end
  if !properlyClosed
    (tk, span) = @lastToken()
    @diagWith ExpectBracketClose(span, cbc, mod)
  end
  return coll
end


end # module GoDuck.Femtoparsec.Combinators
