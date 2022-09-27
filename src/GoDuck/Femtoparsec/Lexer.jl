
module Lexer

using ...RT


export bracket_pair, is_bracket_modifier_char
export is_operator_char, is_name_start_char, is_name_part_char, is_uom_char
export LexIt, lex
export Token, EOF, UnexpectedToken
export WhiteSpace, SPC, SPC1, SPCN, TAB, TAB1, TABN, LBR, LBR1, LBRN, Comment
export Phrase, LitStr, Open, Close
export Identifier, AlphaId, OperId
export LitNumber, LitInt, LitHex, LitQty


bracket_pair(::Val{'{'})::Tuple{Char,Char} = '{', '}'
bracket_pair(::Val{'}'})::Tuple{Char,Char} = '{', '}'
bracket_pair(::Val{'['})::Tuple{Char,Char} = '[', ']'
bracket_pair(::Val{']'})::Tuple{Char,Char} = '[', ']'
bracket_pair(::Val{'('})::Tuple{Char,Char} = '(', ')'
bracket_pair(::Val{')'})::Tuple{Char,Char} = '(', ')'


is_bracket_modifier_char(::Val{'}'}, c::Char)::Bool =
  is_bracket_modifier_char(Val('{'), c)
function is_bracket_modifier_char(::Val{'{'}, c::Char)::Bool
  c in (
    '.', '=',
    '!', '@', '$', '%', '^',
    '&', '|', ':', '?',
    '+', '-', '*',
  )
end

is_bracket_modifier_char(::Val{']'}, c::Char)::Bool =
  is_bracket_modifier_char(Val('['), c)
function is_bracket_modifier_char(::Val{'['}, c::Char)::Bool
  c in (
    '.', '=',
    '!', '$', '%', '^',
    '&', '|', ':', '?',
  )
end

is_bracket_modifier_char(::Val{')'}, c::Char)::Bool =
  is_bracket_modifier_char(Val('('), c)
function is_bracket_modifier_char(::Val{'('}, c::Char)::Bool
  c in (
    '.', '=',
    '!', '$', '%', '^',
    '&', '|', ':', '?',
  )
end


function is_operator_char(c::Char)::Bool
  if codepoint(c) < 128
    c in (
      ',', ';', '.', '=',
      '!', '@', '#', '$', '%', '^',
      '&', '|', ':', '<', '>', '?',
      '+', '-', '*', '/',
    )
  else
    Base.Unicode.category_code(c) in (
      Base.Unicode.UTF8PROC_CATEGORY_SM, # Symbol math
      Base.Unicode.UTF8PROC_CATEGORY_SC, # Symbol currency
      Base.Unicode.UTF8PROC_CATEGORY_SK, # Symbol modifier
      Base.Unicode.UTF8PROC_CATEGORY_SO, # Symbol other
      Base.Unicode.UTF8PROC_CATEGORY_PD, # Punctuation dash
      Base.Unicode.UTF8PROC_CATEGORY_PO, # Punctuation other
    )
  end
end


function is_name_start_char(c::Char)::Bool
  c in ('_',) || Base.Unicode.isletter(c)
end

function is_name_part_char(c::Char)::Bool
  # (!) allowed following Julia, (') allowed following Haskell
  c in ('_', '!', '\'') || Base.Unicode.isletter(c) || Base.Unicode.isnumeric(c)
end


function is_uom_char(c::Char)::Bool
  if codepoint(c) < 128
    c in ('\'', '"', '$', '%',) || Base.Unicode.isletter(c)
  else
    Base.Unicode.isletter(c) || Base.Unicode.category_code(c) in (
      Base.Unicode.UTF8PROC_CATEGORY_SM, # Symbol math
      Base.Unicode.UTF8PROC_CATEGORY_SC, # Symbol currency
      Base.Unicode.UTF8PROC_CATEGORY_SO, # Symbol other
    )
  end
end


# each token type should decide whether it can accept one more char and turn into a new token
struct DoneToken
  token::Token
end
struct ContinueToken
  token::Token
end
# target token is done before the new char, a new token starts from the new char
struct SplitToken
  done::Token
  cont::Token
end
function accept_one_more(::Token, ::Char)::Union{DoneToken,ContinueToken,SplitToken,Nothing}
  return nothing
end


# unexpected token
struct UnexpectedToken <: Token
  desc::String
end


# any number of space chars
abstract type SPC <: WhiteSpace end

# single space char
struct SPC1 <: SPC end
accept_one_more(::SPC1, c::Char) =
  if Base.Unicode.category_code(c) in (Base.Unicode.UTF8PROC_CATEGORY_ZS,
    Base.Unicode.UTF8PROC_CATEGORY_ZP, # TODO: right to accomodate paragraph sep like this?
  )
    ContinueToken(SPCN(2))
  else
    nothing
  end

# multiple space chars
struct SPCN <: SPC
  n::Int # invariant: n > 1
end
accept_one_more(t::SPCN, c::Char) =
  if Base.Unicode.category_code(c) in (Base.Unicode.UTF8PROC_CATEGORY_ZS,
    Base.Unicode.UTF8PROC_CATEGORY_ZP, # TODO: right to accomodate paragraph sep like this?
  )
    ContinueToken(SPCN(t.n + 1))
  else
    nothing
  end

# any number of tab chars
abstract type TAB <: WhiteSpace end

# single tab char
struct TAB1 <: TAB end
accept_one_more(::TAB1, c::Char) =
  if c == '\t'
    ContinueToken(TABN(2))
  else
    nothing
  end

# multiple tab chars
struct TABN <: TAB
  n::Int # invariant: n > 1
end
accept_one_more(t::TABN, c::Char) =
  if c == '\t'
    ContinueToken(TABN(t.n + 1))
  else
    nothing
  end

# any number of line breaks
abstract type LBR <: WhiteSpace end
# note: no `accept_one_more()` impl. for line breaks,
#       they are specially handled as the lexer runs line by line

# single line break char
struct LBR1 <: LBR end
# multiple line break chars
struct LBRN <: LBR
  n::Int # invariant: n > 1
end


# comments 
struct Comment <: WhiteSpace
  # block comments always have a tag starting with `=`, while a line comment can have empty tag if it is empty or have a white-space immediately following the starting `#`
  tag::String # word immediately following the starting `#` before any white-space
  # the line span is 0 for an inline block-comment, otherwise can only >= 1
  line_span::Int # number of lines the comment spans
  lines::Vector{String}
end


# string literal, quoted by single-quote, double-quote, or back-tick, or triple sequence of them
struct LitStr <: Phrase
  value::String
  quotation::String # the starting quotation mark, while ending mark should always be inferable
end


# bracketing tokens

# starts with `(` `[` `{`, followed by operator chars without intermediate whitespaces
struct Open <: Phrase
  bracket::Char
  mod::String
  function Open(bracket::AbstractChar, mod::AbstractString="")
    new(Char(bracket), string(mod))
  end
end
accept_one_more(b::Open, c::Char) =
  if is_bracket_modifier_char(Val(b.bracket), c)
    ContinueToken(Open(b.bracket, b.mod * c))
  else
    nothing
  end

# ends with `)` `]` `}`, following operator chars without intermediate whitespaces
struct Close <: Phrase
  bracket::Char
  mod::String
  function Close(bracket::AbstractChar, mod::AbstractString="")
    new(Char(bracket), string(mod))
  end
end
# note no `accept_one_more()` impl. as it's always done immediately


# sequence of alphanumeric chars
struct AlphaId <: Identifier
  id::String
end
accept_one_more(t::AlphaId, c::Char) =
  if is_name_part_char(c)
    ContinueToken(AlphaId(t.id * c))
  else
    nothing
  end

# sequence of "symbolic" operator chars
struct OperId <: Identifier
  id::String
end
accept_one_more(t::OperId, c::Char) =
  if is_operator_char(c)
    ContinueToken(OperId(t.id * c))
  elseif c in (')', ']', '}')
    if all(oc -> is_bracket_modifier_char(Val(c), oc), t.id)
      DoneToken(Close(c, t.id))
    else # not all op chars satisfying `is_bracket_modifier_char()`,
      # todo: give as much satisfying chars at rhs of `t.id` to `Close` token?
      #       remind: the start/end position of the split tokens need more nontrivial work to get correct
      #       anyway the end programmer should always make it match the opening bracket w.r.t. modifier,
      #       and be adviced to insert some space separater if it do follow some `OperId` token,
      #       we just make that separater space token mandatory, by far, as impl. here
      SplitToken(t, Close(c, ""))
    end
  elseif isdigit(c)
    if t.id == "-"
      ContinueToken((LitInt(true, string(c))))
    elseif t.id == "+"
      ContinueToken((LitInt(false, string(c))))
    else
      nothing
    end
  else
    nothing
  end


# integer literal
# note: decimal points not parsed as part, the parser is responsible to interpret the dot(.) operator identifier to construct fractional numbers
struct LitInt <: LitNumber
  negative::Bool
  digits::String
end
accept_one_more(t::LitInt, c::Char) =
  if Base.Unicode.isdigit(c)
    ContinueToken(LitInt(false, t.digits * c))
  elseif c in ('x', 'X') && !t.negative && t.digits === "0"
    ContinueToken(LitHex(""))
  elseif is_uom_char(c)
    ContinueToken(LitQty(t.negative, t.digits, string(c)))
  else
    nothing
  end

# hexadecimal literal
struct LitHex <: LitNumber
  digits::String
end
accept_one_more(t::LitHex, c::Char) =
  if Base.Unicode.isxdigit(c)
    ContinueToken(LitHex(t.digits * c))
  else
    nothing
  end

# quantity, i.e. natural number literal followed by UoM
struct LitQty <: LitNumber
  negative::Bool
  digits::String # invariant: only decimal digits
  uom::String # unit of measure
end
accept_one_more(t::LitQty, c::Char) =
  if is_uom_char(c)
    ContinueToken(LitQty(t.negative, t.digits, t.uom * c))
  else
    nothing
  end


# Lexer
struct LexIt
  lines::Any # anything to be iterated for lines of source code
end

lex(io::IO) = LexIt(eachline(io))
lex(str::AbstractString) = LexIt(eachline(IOBuffer(string(str))))

Base.IteratorSize(::Type{LexIt}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{LexIt}) = Base.HasEltype()
Base.eltype(::Type{LexIt}) = Tuple{Token,SrcRange}


# implementation details to be hidden deeper inside this Lexer module
module LexItDetails

using ....RT

using ..Lexer: is_operator_char, is_name_start_char, is_name_part_char, is_uom_char
using ..Lexer: DoneToken, ContinueToken, SplitToken, accept_one_more
using ..Lexer: SPC, SPC1, SPCN, TAB, TAB1, TABN, LBR, LBR1, LBRN, Comment
using ..Lexer: LitStr, Open, Close
using ..Lexer: AlphaId, OperId
using ..Lexer: LitInt, LitHex, LitQty
using ..Lexer: LexIt


mutable struct CommentWIP
  tag::String
  lines::Vector{String}
  nests::Int # nesting level, to handle block comments inside outer block comments
  start::SrcPosition
end

mutable struct StringWIP
  value::String
  quot::String
  start::SrcPosition
end

mutable struct TokenWIP
  token::Token
  start::SrcPosition
  end_::SrcPosition
end

mutable struct LexItState
  line_it::Any
  line_str::Union{String,Nothing}
  cuidx::Int # code unit index, it is utf8 byte idx as `line_str` is `String`
  line::Int
  char::Int
  wip::Union{CommentWIP,StringWIP,TokenWIP,Nothing}
  LexItState(line_it::Any, line_str::String) = new(
    line_it, line_str, firstindex(line_str), 1, 0, nothing)
end


"""    Don't use this elsewhere, it spits a hardcoded `lis::LexItState` reference, which you probably don't have.
"""
macro lexcall(fc)
  return quote
    r::Union{Tuple{Token,SrcRange},Nothing} = $(esc(fc))
    if r !== nothing
      return r, $(esc(:lis))
    end
  end
end


function Base.iterate(
  lexer::LexIt, lis_arg::Union{LexItState,Nothing}=nothing
)::Union{Tuple{Tuple{Token,SrcRange},LexItState},Nothing}
  local lis::LexItState = if lis_arg === nothing
    let first_line = Base.iterate(lexer.lines)
      if first_line === nothing
        return nothing
      end
      line_val, line_it = first_line
      LexItState(line_it, string(line_val))
    end
  else
    lis_arg
  end
  local wip = nothing # `wip` will repeatedly alias (possibly updated) `lis.wip` per loop iterations

  # some helper functions, they enclose `lis` and `wip` so have to live inside

  function start_comment()::Union{Tuple{Token,SrcRange},Nothing}
    curpos = SrcPosition(lis.line, lis.char)
    after_tag_idx = tag_start_idx = lis.cuidx
    tag_end_idx = tag_start_idx - 1
    while true # parse comment tag
      let next_tc = Base.iterate(lis.line_str, after_tag_idx)
        if next_tc === nothing
          break
        end
        local tc, next_cuidx = next_tc
        if isspace(tc)
          break
        end
        tag_end_idx = after_tag_idx
        after_tag_idx = next_cuidx
      end
    end
    tag = lis.line_str[tag_start_idx:tag_end_idx]
    if startswith(tag, "=") # starting a block comment
      # start search from right after the 1st (=) char
      cmt_end_idx = nextind(lis.line_str, tag_start_idx)
      nests = 1
      while true # seek end of block comment, recognize nested block comments meanwhile
        indent_rng = findnext("#=", lis.line_str, cmt_end_idx)
        @assert indent_rng === nothing || indent_rng isa UnitRange{Int}
        outdent_rng = findnext("=#", lis.line_str, cmt_end_idx)
        @assert outdent_rng === nothing || outdent_rng isa UnitRange{Int}
        if outdent_rng !== nothing && (
          indent_rng === nothing || # no nested block-cmt starting
          # another block-cmt starting, but after one level of nesting outdented
          indent_rng.start > outdent_rng.stop
        )
          nests -= 1 # outdent one level of nesting
          if nests < 1 # finished one piece of inline block comment
            cmt_end_idx = outdent_rng.stop
            break
          end
        end
        if indent_rng === nothing
          if outdent_rng === nothing
            # block comment does not end in this line
            @assert nests >= 1
            break
          else
            cmt_end_idx = outdent_rng.stop
          end
        else
          if outdent_rng === nothing
            # indent only
            nests += 1
            cmt_end_idx = indent_rng.stop
          elseif indent_rng.start > outdent_rng.stop
            # indent after a non-final-outdent
            nests += 1
            cmt_end_idx = indent_rng.stop
          else # bypass a complete piece of nested inline block-cmt
            cmt_end_idx = outdent_rng.stop
          end
        end
        # search from next char after end pos of last interesting marker
        cmt_end_idx = nextind(lis.line_str, cmt_end_idx)
      end
      if nests < 1 # got an inline block comment
        # to continue lexing current line after the inline comment
        lis.cuidx = nextind(lis.line_str, cmt_end_idx)
        # strip marker chars as well as white spaces from both ends of the inline comment
        cmt_rest = lis.line_str[after_tag_idx:cmt_end_idx]
        @assert endswith(cmt_rest, "=#")
        cmt_txt = strip(chop(cmt_rest; tail=2))
        cmt = Comment(string(tag), 0, String[string(cmt_txt)])
        endpos = SrcPosition(lis.line, lis.char + length(tag) + length(cmt_rest))
        if wip === nothing # at very beginning of the whole src 
          # yield the inline comment, w/o token wip
          return cmt, SrcRange(curpos, endpos)
        else
          @assert wip isa TokenWIP "non-token wip not expected here"
          # schedule the inline comment nextly,
          # then yield the last recognized token on this line
          lis.wip = TokenWIP(cmt, curpos, endpos)
          return wip.token, SrcRange(wip.start, wip.end_)
        end
      else # starting a multi-line block comment, wip
        cmt_rest = lis.line_str[after_tag_idx:end]
        # strip white spaces from both ends of the 1st comment line
        cmt_txt = strip(cmt_rest)
        lis.line_str = nothing # done with this line
        # schedule the multi-line block comment wip
        lis.wip = CommentWIP(string(tag), String[cmt_txt], nests, curpos)
        if wip === nothing # no pending token before the comment
          return nothing
        else
          @assert wip isa TokenWIP "non-token wip not expected here"
          # yield the last recognized token on this line
          return wip.token, SrcRange(wip.start, wip.end_)
        end
      end
    else # got a single-line comment
      cmt_rest = lis.line_str[after_tag_idx:end]
      cmt_txt = strip(cmt_rest)
      lis.line_str = nothing # done with this line
      cmt = Comment(string(tag), 1, String[cmt_txt])
      endpos = SrcPosition(lis.line, lis.char + length(tag) + length(cmt_rest))
      if wip === nothing # at very beginning of the whole src 
        # yield the single-line comment, w/o token wip
        return cmt, SrcRange(curpos, endpos)
      else
        @assert wip isa TokenWIP "non-token wip not expected here"
        # schedule the single-line comment nextly,
        # then yield the last recognized token on this line
        lis.wip = TokenWIP(cmt, curpos, endpos)
        return wip.token, SrcRange(wip.start, wip.end_)
      end
    end
  end

  function start_lit_str(quotc::Char)::Union{Tuple{Token,SrcRange},Nothing}
    curpos = SrcPosition(lis.line, lis.char)
    str_start_idx = lis.cuidx
    qc_repeat = 0
    while qc_repeat < 2
      next_qc = Base.iterate(lis.line_str, str_start_idx)
      if next_qc === nothing
        break
      end
      local qc, next_cuidx = next_qc
      if qc !== quotc
        break
      end
      str_start_idx = next_cuidx
      qc_repeat += 1
    end
    @assert qc_repeat <= 2
    if qc_repeat === 2 # triple quoting string literal
      lis.cuidx = str_start_idx
      quot = repeat(quotc, 3)
    else # single quoting string literal
      # lis.cuidx is already the right starting position of string contents
      quot = repeat(quotc, 1)
    end
    lis.wip = StringWIP("", quot, curpos)
    if wip === nothing # this str lit is the very 1st token in src
      return nothing
    else
      @assert wip isa TokenWIP "non-token wip not expected here"
      # yield the last recognized token on this line
      return wip.token, SrcRange(wip.start, wip.end_)
    end
  end

  function error_unexpected_char(c::Char)::Union{Tuple{Token,SrcRange},Nothing}
    curpos = SrcPosition(lis.line, lis.char)
    char_desc = let iob = IOBuffer()
      show(iob, "text/plain", c)
      String(take!(iob))
    end
    tk = UnexpectedToken("unexpected char - $char_desc")
    if wip === nothing
      return (tk, SrcRange(curpos, curpos)), lis
    else
      @assert wip isa TokenWIP "non-token wip not expected here"
      # schedule the warning token nextly,
      # then yield the last recognized token on this line
      lis.wip = TokenWIP(tk, curpos, curpos)
      return wip.token, SrcRange(wip.start, wip.end_)
    end
  end

  # the main loop
  while true
    if lis.line_str === nothing
      let next_line = Base.iterate(lexer.lines, lis.line_it)
        if next_line === nothing
          wip = lis.wip
          if wip !== nothing
            lis.wip = nothing
            curpos = SrcPosition(lis.line, lis.char)
            if wip isa CommentWIP
              return (Comment(wip.tag, length(wip.lines), wip.lines),
                SrcRange(wip.start, curpos)), lis
            elseif wip isa StringWIP
              return (LitStr(wip.value, wip.quot),
                SrcRange(wip.start, curpos)), lis
            elseif wip isa TokenWIP
              return (wip.token, SrcRange(wip.start, wip.end_)), lis
            else
              @assert false "non-exhaustive handling of wip types"
            end
          end
          return nothing
        end
        line_val, lis.line_it = next_line
        lis.line_str = string(line_val) # make sure be of Julia String, i.e. w/ utf8 storage
        lis.cuidx = firstindex(lis.line_str)
        lis.line += 1
        lis.char = 0
      end
    end
    wip = lis.wip
    if wip isa CommentWIP # in multi-line block comment
      @assert lis.char === 0 "here never supposed reachable from mid of a line"
      cmt_end_idx = firstindex(lis.line_str)
      while true # seek end of block comment, recognize nested block comments meanwhile
        indent_rng = findnext("#=", lis.line_str, cmt_end_idx)
        @assert indent_rng === nothing || indent_rng isa UnitRange{Int}
        outdent_rng = findnext("=#", lis.line_str, cmt_end_idx)
        @assert outdent_rng === nothing || outdent_rng isa UnitRange{Int}
        if outdent_rng !== nothing && (
          indent_rng === nothing || # no nested block-cmt starting
          # another block-cmt starting, but after one level of nesting outdented
          indent_rng.start > outdent_rng.stop
        )
          wip.nests -= 1 # outdent one level of nesting
          if wip.nests < 1 # finished one piece of inline block comment
            cmt_end_idx = outdent_rng.stop
            break
          end
        end
        if indent_rng === nothing
          if outdent_rng === nothing
            # block comment does not end in this line
            @assert wip.nests >= 1
            break
          else
            cmt_end_idx = outdent_rng.stop
          end
        else
          if outdent_rng === nothing
            # indent only
            wip.nests += 1
            cmt_end_idx = indent_rng.stop
          elseif indent_rng.start > outdent_rng.stop
            # indent after a non-final-outdent
            wip.nests += 1
            cmt_end_idx = indent_rng.stop
          else # bypass a complete piece of nested inline block-cmt
            cmt_end_idx = outdent_rng.stop
          end
        end
        # search from next char after end pos of last interesting marker
        cmt_end_idx = nextind(lis.line_str, cmt_end_idx)
      end
      if wip.nests < 1 # the block-cmt completes in this line
        # to continue lexing current line after the end of the block comment
        lis.cuidx = nextind(lis.line_str, cmt_end_idx)
        # strip marker chars as well as white spaces from right end of the last comment line
        cmt_rest = lis.line_str[begin:cmt_end_idx]
        @assert endswith(cmt_rest, "=#")
        cmt_txt = rstrip(chop(cmt_rest; tail=2))
        push!(wip.lines, string(cmt_txt))
        lis.char += length(cmt_rest)
        endpos = SrcPosition(lis.line, lis.char)
        lis.wip = nothing
        return (Comment(wip.tag, length(wip.lines), wip.lines), SrcRange(wip.start, endpos)), lis
      else # this whole line belong to the pending block comment
        # TODO: strip white spaces from any ends of the line here?
        push!(wip.lines, lis.line_str)
        lis.line_str = nothing # done with this line
      end
    elseif wip isa StringWIP # in literal string (always multi-line)
      fin_rng = findnext(wip.quot, lis.line_str, lis.cuidx)
      if fin_rng === nothing # string literal does not end
        wip.value *= lis.line_str[lis.cuidx:end] * "\n"
        lis.line_str = nothing # done with this line
      else
        @assert fin_rng isa UnitRange{Int}
        str_rest = lis.line_str[lis.cuidx:fin_rng.stop]
        @assert endswith(str_rest, wip.quot)
        wip.value *= chop(str_rest; tail=length(wip.quot))
        lis.cuidx = nextind(lis.line_str, fin_rng.stop)
        lis.char += length(str_rest)
        lis.wip = nothing
        return (LitStr(wip.value, wip.quot),
          SrcRange(wip.start, SrcPosition(lis.line, lis.char))), lis
      end
    else
      lis.char += 1 # even at end-of-line, a '\n' is assumed there, so always increase column count
      curpos = SrcPosition(lis.line, lis.char)
      next_char = Base.iterate(lis.line_str, lis.cuidx)
      # assuming '\n' (and '\r' as well?) has been stripped off by line iterator, so
      if next_char === nothing # this line exhausted
        # `lis.wip` will be cleared, but note that local `wip` has aliased it beforehand
        lis.line_str = nothing # done with this line
        if wip === nothing
          lis.wip = TokenWIP(LBR1(), curpos, curpos)
        elseif wip.token isa LBR1
          lis.wip = TokenWIP(LBRN(2), wip.start, curpos)
        elseif wip.token isa LBRN
          lis.wip = TokenWIP(LBRN(wip.token.n + 1), wip.start, curpos)
        else
          # schedule a single line-break nextly,
          # then yield the last recognized token on this line
          lis.wip = TokenWIP(LBR1(), curpos, curpos)
          return (wip.token, SrcRange(wip.start, wip.end_)), lis
        end
      else # got a char possibly following some token, that not a wild absorbing block,
        # i.e. not part of a string literal or comment
        c, lis.cuidx = next_char
        absorbed = false
        if lis.wip isa TokenWIP
          new_tk = accept_one_more(lis.wip.token, c)
          if new_tk isa DoneToken
            startpos = lis.wip.start
            lis.wip = nothing
            return (new_tk.token, SrcRange(startpos, curpos)), lis
          elseif new_tk isa ContinueToken
            lis.wip.end_ = curpos
            lis.wip.token = new_tk.token
            absorbed = true
          elseif new_tk isa SplitToken
            lis.wip = TokenWIP(new_tk.cont, curpos, curpos)
            return (new_tk.done, SrcRange(startpos, wip.end_)), lis
          else
            @assert new_tk === nothing
          end
        end
        if absorbed # by the pending token
          nothing # to do
        elseif c === '#' # starting a comment, either single-line or block
          @lexcall start_comment()
        elseif c in ('\'', '"', '`') # starting a literal string
          @lexcall start_lit_str(c)
        else
          function start_new_token(tk::Token)::Union{Tuple{Token,SrcRange},Nothing}
            lis.wip = TokenWIP(tk, curpos, curpos)
            if wip === nothing
              return nothing
            else
              @assert wip isa TokenWIP "non-token wip not expected here"
              return wip.token, SrcRange(wip.start, wip.end_)
            end
          end
          if c in ('(', '[', '{',)
            @lexcall start_new_token(Open(c, ""))
          elseif c in (')', ']', '}')
            @lexcall start_new_token(Close(c, ""))
          elseif is_operator_char(c)
            @lexcall start_new_token(OperId(string(c)))
          elseif is_name_start_char(c)
            @lexcall start_new_token(AlphaId(string(c)))
          elseif Base.Unicode.isdigit(c)
            @lexcall start_new_token(LitInt(false, string(c)))
          elseif c === '\t'
            @lexcall start_new_token(TAB1())
          elseif c === ' ' || Base.Unicode.category_code(c) in (
            Base.Unicode.UTF8PROC_CATEGORY_ZS, # single white space
            Base.Unicode.UTF8PROC_CATEGORY_ZP, # TODO: right to accomodate paragraph sep like this?
          )
            @lexcall start_new_token(SPC1())
          else
            @lexcall error_unexpected_char(c)
          end
        end
      end
    end
  end
end

end # module LexItDetails

end # module GoDuck.Femtoparsec.Lexer
