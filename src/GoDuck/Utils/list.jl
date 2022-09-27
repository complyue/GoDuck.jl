
# immutable homogeneous list

abstract type List{T} end

struct EmptyList{T} <: List{T} end
Base.length(e::EmptyList{T}) where {T} = 0

struct Cons{T} <: List{T}
  head::T
  tail::List{T}
  function Cons(h::T, t::List{T}=EmptyList{T}()) where {T}
    new{T}(h, t)
  end
end
Base.length(l::Cons{T}) where {T} = 1 + length(l.tail)

function Base.iterate(l::List{T}, it::Union{List{T},Nothing}=nothing) where {T}
  if it isa Nothing
    return l.head, l.tail
  elseif it isa Cons{T}
    return it.head, it.tail
  else
    return nothing
  end
end
