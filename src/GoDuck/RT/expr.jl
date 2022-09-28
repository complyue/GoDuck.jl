
abstract type GoDuckExpr end

function expr_src(x::Any)::String
  io = IOBuffer()
  expr_src(io, x)
  return String(take!(io))
end

# for other Julia values not able to be marked inheriting from GoDuckExpr
function expr_src(io::IO, x::Any)::Nothing
  show(io, x)
end

function expr_src(::IO, x::GoDuckExpr)::Nothing
  # fail loudly for any subtype not overriding its source form show
  error("uncovered expr type: $(typeof(x))")
end


struct WithSpan{T} <: GoDuckExpr where {T<:GoDuckExpr}
  expr::T
  span::SrcRange
end
