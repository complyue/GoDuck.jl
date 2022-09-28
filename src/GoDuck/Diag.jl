
module Diag

export SrcPosition, SrcRange, SrcLocation, NilPosition, NilRange
export SyntaxError, SyntaxWarn
export Diagnosis, diagWhere, Error, Warn, Hint


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


# diagnostics

abstract type Diagnosis end

function diagWhere(::Diagnosis)::SrcRange
  return NilRange
end

abstract type Error <: Diagnosis end

abstract type Warn <: Diagnosis end

abstract type Hint <: Diagnosis end


struct SyntaxError <: Error
  span::SrcRange
  msg::String # TODO: i18n
end

diagWhere(err::SyntaxError) = err.span


struct SyntaxWarn <: Warn
  span::SrcRange
  msg::String # TODO: i18n
end

diagWhere(err::SyntaxWarn) = err.span


end # module GoDuck.Diag
