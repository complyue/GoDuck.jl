
# root type of lexical tokens
abstract type Token end

# end of file as a special token
struct EOF <: Token end

# root type of white spaces
abstract type WhiteSpace <: Token end

# root type of non-white-space tokens
abstract type Phrase <: Token end

# root type of identifiers
# including operators (notably +-*/), even colons (:), and commas (,) and semicolons (;)
abstract type Identifier <: Phrase end

# root type of numerical literals
abstract type LitNumber <: Phrase end


"""    for special perser procedure return value, stating unmet expectations
"""
abstract type Unmet <: Diagnosis end

abstract type Incomplete <: Error end


struct ExpectTerms <: Unmet
  span::SrcRange
  terms::Vector{String}        # term names to be listed for expectation at the unmet position
  suggestions::Vector{String}  # suggestions to be shown at the unmet position
  ExpectTerms() = new(NilRange, [], [])
  ExpectTerms(span::SrcRange, term::AbstractString) = new(span, [string(term)], [])
  ExpectTerms(span::SrcRange, terms::Vector{String}) = new(span, terms, [])
  ExpectTerms(span::SrcRange, terms::Vector{String}, suggs::Vector{String}) = new(span, terms, suggs)
end

diagWhere(x::ExpectTerms) = x.span


struct ExpectBracketOpen <: Unmet
  span::SrcRange
  bracket::Char
  mod::String
end

diagWhere(x::ExpectBracketOpen) = x.span


struct ExpectBracketClose <: Incomplete
  span::SrcRange
  bracket::Char
  mod::String
end

diagWhere(x::ExpectBracketClose) = x.span


struct LexInProgress
  # source of tokens
  lexer::Any
  # lexer iteration state
  lis::Ref{Any}
  function LexInProgress(lexer::Any, lis::Any=nil)
    new(lexer, lis === nil ? Ref{Any}() : Ref{Any}(lis))
  end
end

struct ParseInProgress
  # lexing in progress
  lip::LexInProgress
  # back-tracking vector of historic tokens
  btv::Vector{Tuple{Token,SrcRange}}
  # index into `ParseInProgress.btv` for next token,
  nxi::Ref{Int} # invariant: always >= 1
  # diagnoses accumulated
  problems::Vector{Diagnosis}

  function ParseInProgress(lip::LexInProgress)
    new(lip, [], 1, [])
  end
end


const _pip!sym = Symbol("#!parse-in-progress")
const _pbt!sym = Symbol("#!parse-backtracking")


"""    parse the specified lexer (iterable source of tokens) with a `@parser function`

NOTE: `f::Function` has to be a `@parser` or `@lexeme` decorated function
"""
function femtoParse(lip::LexInProgress, f::Function, args...; kwargs...)::Tuple{Union{Any,Unmet},Vector{Diagnosis}}
  @eval begin
    $_pip!sym = $ParseInProgress($lip)
    $_pbt!sym = false
    p!result = $f($args...; $kwargs..., $_pip!sym, $_pbt!sym)
    return p!result, $_pip!sym.problems
  end
end


macro diagWith(problem)
  return quote
    push!($(esc(_pip!sym)).problems, $(esc(problem)))
  end
end


function _parser(fd, asmet=nothing)
  @assert Meta.isexpr(fd, :function, 2)
  top_sig, body = fd.args
  rtn_type = Any
  sig = if top_sig.head === :where # w/ type params
    top_sig.args[1]
  else
    top_sig
  end
  if sig.head === :(::) # w/ return type anno
    rtn_type = esc(sig.args[2])
    sig = sig.args[1]
  end
  @assert sig.head === :call
  kwargs = nothing
  for arg in sig.args[2:end]
    if arg isa Expr && arg.head === :parameters
      kwargs = arg.args
      break
    end
  end
  if kwargs === nothing
    let params = Expr(:parameters)
      insert!(sig.args, 2, params)
      kwargs = params.args
    end
  end
  splice!(kwargs, 1:0, [
    Expr(:(::), _pip!sym, ParseInProgress),
    Expr(:(::), _pbt!sym, Bool),
  ])

  return Expr(:function, esc(top_sig), quote
    let pip = $(esc(_pip!sym)), pbt = $(esc(_pbt!sym))

      p!result = (() -> $(esc(body)))()
      if p!result isa Unmet # unmet

        return p!result # early-return the unmet

      else # met

        $asmet

        if !pbt && (l = length(pip.btv)) > 1 # met and no backtracking
          # keep only the last token, forget older ones
          splice!(pip.btv, 1:l-1)
          pip.nxi[] = 2
        end

        return p!result::$rtn_type # return the result

      end
    end
  end)
end

"""    declare a parser function
"""
macro parser(fd)
  return _parser(fd)
end

"""    declare a lexeme parser function, who consumes all white spaces following its payload
"""
macro lexeme(fd)
  return _parser(fd, esc(quote
    while @tokAhead() isa $WhiteSpace
      @nextToken()
    end
  end))
end


macro lastToken()
  return quote
    let pip = $(esc(_pip!sym))
      lti = pip.nxi[] - 1
      if 1 <= lti <= length(pip.btv)
        @inbounds pip.btv[lti]
      else
        nothing, NilRange
      end
    end
  end
end

macro lastTok()
  return quote
    @lastToken()[1]
  end
end


function _fetchNextToken()
  return quote
    if pip.nxi[] > length(pip.btv)
      @assert pip.nxi[] == length(pip.btv) + 1 "went too far?!"
      tok = EOF(), NilRange
      next_tok = if isassigned(pip.lip.lis)
        Base.iterate(pip.lip.lexer, pip.lip.lis[])
      else
        Base.iterate(pip.lip.lexer)
      end
      if next_tok !== nothing
        tok, pip.lip.lis[] = next_tok::Tuple{Tuple{Token,SrcRange},Any}
      end
      push!(pip.btv, tok)
    end
    @assert(
      1 <= pip.nxi[] <= length(pip.btv),
      "not so expected ParseInProgress situation nxi=$(pip.nxi[]) vs $(length(pip.btv))"
    )
    @inbounds pip.btv[pip.nxi[]]
  end
end

macro tokAhead()
  return quote
    let pip = $(esc(_pip!sym))
      tok, _span = $(_fetchNextToken())
      tok
    end
  end
end

macro nextTok()
  return quote
    let pip = $(esc(_pip!sym))
      @assert(
        pip.nxi[] <= length(pip.btv) + 1,
        "skip some for nextToken?!"
      )
      tok, _span = $(_fetchNextToken())
      pip.nxi[] += 1
      tok
    end
  end
end

macro tokenAhead()
  return quote
    let pip = $(esc(_pip!sym))
      $(_fetchNextToken())
    end
  end
end

macro nextToken()
  return quote
    let pip = $(esc(_pip!sym))
      @assert pip.nxi[] <= length(pip.btv) + 1 "skip some for nextToken?!"
      tok_n_span = $(_fetchNextToken())
      pip.nxi[] += 1
      tok_n_span
    end
  end
end

macro expectTokenOf(tkt::Symbol)
  quote
    let (tk, span) = @nextToken()
      t = $(esc(tkt))
      if tk isa t # met
        tk # eval to the token value of expected type
      else
        return ExpectTerms(span, string(nameof(t)))
      end
    end
  end
end

macro expectToken(tkx)
  quote
    let (tk, span) = @nextToken()
      tkv = $(esc(tkx))
      if tk == tkv # met
        tkv # eval to the expected token value
      else # unmet
        return ExpectTerms(span, repr(tkv))
      end
    end
  end
end


macro collectBracketed(term, into, obc!x, item!x, cbc!x, mod!x="")
  quote
    let (tk, span) = @nextToken()
      obc::AbstractChar, cbc::AbstractChar, mod::AbstractString = $(esc(obc!x)), $(esc(cbc!x)), $(esc(mod!x))
      otk, ctk = $Open(obc, mod), $Close(cbc, mod)
      if tk != otk
        return ExpectBracketOpen(span, obc, mod)
      end
      @sc
      properlyClosed = false
      while true
        if @tokAhead() == ctk
          @nextTok()
        end
        if @lastTok() == ctk
          @sc
          properlyClosed = true
          break
        end
        _, spanAhead = @tokenAhead()
        p!result = (() -> $(esc(item!x)))()
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
              @diagWith ExpectTerms(span, string($(esc(term))))
            end
          else # some token(s) consumed and diagnosis given, log that
            @diagWith p!result # report the problem
          end
          if @lastTok() isa EOF # tokens exhausted
            break
          end
        else
          push!($(esc(into)), p!result) # TODO: document/improve type safety here
        end
      end
      if !properlyClosed
        (tk, span) = @lastToken()
        @diagWith ExpectBracketClose(span, cbc, mod)
      end
      true # indicating the opening bracket is present, or an `ExpectBracketOpen` will be early-returned as above
    end
  end
end


"""    call a `@parser function` with in-progress parsing context currently in scope

NOTE: all `@parser function`s have to be called with this `@parse` macro
"""
macro parse(top_fc)
  fc = if Meta.isexpr(top_fc, :do)
    top_fc.args[1]
  else
    top_fc
  end
  @assert fc.head === :call "@parse can only be used to make a single function call"
  kwargs = nothing
  for arg in fc.args[2:end]
    if arg isa Expr && arg.head === :parameters
      kwargs = arg.args
      break
    end
  end
  if kwargs === nothing
    let params = Expr(:parameters)
      insert!(fc.args, 2, params)
      kwargs = params.args
    end
  end
  push!(kwargs,
    Expr(:kw, _pip!sym, _pip!sym),
    Expr(:kw, _pbt!sym, _pbt!sym),
  )
  return esc(top_fc)
end


"""    do some parsing

evaluates to the met result, or unmet of parsing will trigger immediate return with the `Unmet` result
"""
macro expect(body)
  quote
    p!result = $(esc(body))
    if p!result isa Unmet # unmet
      return p!result # early-return the unmet
    else # met
      p!result # evaluates to the result
    end
  end
end

"""    do some parsing, and further repeat the parsing until no more met

unmet of parsing at the first time will trigger immediate return with the `Unmet` result, and without consuming input, or the result will always be `nothing`, you just expect the side-effect(s) of the parsing(s)
"""
macro expectOneOrMore(body)
  quote
    let pip = $(esc(_pip!sym)), pbt = $(esc(_pbt!sym))

      # expect it met for the 1st time
      p!result = (() -> $(esc(body)))()
      if p!result isa Unmet # unmet
        return p!result # early-return the unmet
      end

      # attempt subsequent parsing
      let $(esc(_pbt!sym)) = true # ensure backtracking
        while true # keep parsing until unmet
          local oxi = pip.nxi[] # save original index for next token
          r = (() -> $(esc(body)))()
          if r isa Unmet # unmet
            pip.nxi[] = oxi # restore to where we'd been
            break
          end
        end
      end

      if !pbt && (l = length(pip.btv)) > 1 # met and no backtracking
        # keep only the last token, forget older ones
        splice!(pip.btv, 1:l-1)
        pip.nxi[] = 2
      end

      nothing # always evaluates to nothing
    end
  end
end

"""    try some parsing

evaluates to the met result, or the result will be `nothing` if unmet, and without consuming input
"""
macro maybe(body)
  quote
    let pip = $(esc(_pip!sym)), pbt = $(esc(_pbt!sym))
      local $(esc(_pbt!sym)) = true # ensure backtracking

      local oxi = pip.nxi[] # save original index for next token
      p!result = (() -> $(esc(body)))()
      if p!result isa Unmet # unmet
        pip.nxi[] = oxi # restore to where we'd been
        nothing # evaluates to nothing
      else # met
        if !pbt && (l = length(pip.btv)) > 1 # met and no backtracking
          # keep only the last token, forget older ones
          splice!(pip.btv, 1:l-1)
          pip.nxi[] = 2
        end
        p!result # evaluates to the result
      end
    end
  end
end

"""    try some parsing, repeat until no more met

the result will always be `nothing`, you just expect the side-effect(s) of the parsing(s)
"""
macro expectZeroOrMore(body)
  quote
    let pip = $(esc(_pip!sym)), pbt = $(esc(_pbt!sym))
      local $(esc(_pbt!sym)) = true # ensure backtracking

      while true # keep parsing until unmet
        local oxi = pip.nxi[] # save original index for next token
        p!result = (() -> $(esc(body)))()
        if p!result isa Unmet # unmet
          pip.nxi[] = oxi # restore to where we'd been
          break
        end
      end

      if !pbt && (l = length(pip.btv)) > 1 # met and no backtracking
        # keep only the last token, forget older ones
        splice!(pip.btv, 1:l-1)
        pip.nxi[] = 2
      end

      nothing # always evaluates to nothing
    end
  end
end


macro choiceFor(term::String, choices...)
  quote
    let pip = $(esc(_pip!sym)), pbt = $(esc(_pbt!sym))
      local $(esc(_pbt!sym)) = true # ensure backtracking

      local oxi = pip.nxi[] # save original index for next token

      p!result = (() -> $([
        quote
          # those `choices` can be code blocks,
          # this way they can early `return` from within there without a similar wrapping construct
          let r = (() -> $(esc(cho)))()
            if r isa Unmet # this choice unmet
              pip.nxi[] = oxi # restore to where we'd been, as to attempt next choice
              # fallthrough to try next choice
              r # but evaluates to the unmet, this is necessary as for the last choice
            else # this choice met
              return r # done with the result
            end
          end
        end
        for cho in choices
      ]...))()

      if p!result isa Unmet # all choices unmet
        pip.nxi[] = oxi # restore to where we'd been
        local span = if oxi <= length(pip.btv)
          @inbounds pip.btv[oxi][2]
        else # todo: this really possible?
          NilRange
        end
        # TODO: collect reified expectations and return those elaborated terms
        return ExpectTerms(span, $term)
      else # met
        if !pbt && (l = length(pip.btv)) > 1 # met and no backtracking
          # keep only the last token, forget older ones
          splice!(pip.btv, 1:l-1)
          pip.nxi[] = 2
        end
        p!result # evaluates to the result
      end
    end
  end
end


"""    consume contiguous white spaces if any, since current parsing position

CAVEAT: this is expected to be use within a `@parser` function
"""
macro sc()
  esc(quote
    while @tokAhead() isa $WhiteSpace
      @nextToken()
    end
  end)
end
