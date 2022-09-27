
# IDE oriented source locating data

struct SrcPosition
  line::Int
  char::Int
end
Base.show(io::IO, p::SrcPosition) = print(io, p.line, ':', p.char)

const NilPosition = SrcPosition(0, 0)

struct SrcRange
  start::SrcPosition
  end_::SrcPosition
end
Base.show(io::IO, r::SrcRange) = print(io, r.start, '-', r.end_)

const NilRange = SrcRange(NilPosition, NilPosition)

struct SrcLocation
  file::AbstractString
  span::SrcRange
end
Base.show(io::IO, l::SrcLocation) = print(io, l.file, ':', l.span)


# lexical analysis data

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


# diagnostics

abstract type Diagnosis end

function diagWhere(::Diagnosis)::SrcRange
  return NilRange
end

abstract type Error <: Diagnosis end

abstract type Warn <: Diagnosis end

abstract type Hint <: Diagnosis end
