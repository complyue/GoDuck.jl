
# nil to bear deletion (deleted or to be deleted) semantics in place of an rhs value
struct DelAttr <: GoDuckExpr end
const nil = DelAttr()
expr_src(io::IO, ::DelAttr) = print(io, "nil")


# sortable to guaranteed order, thus to attempt locking in that order to prevent deadlock
struct SortableLock
  lock::ReentrantLock
  function SortableLock()
    new(ReentrantLock())
  end
end
Base.isless(x::SortableLock, y::SortableLock) = isless(objectid(x.lock), objectid(y.lock))


# insertion order preserved, lock guarded dictionary of attribute values
struct Entity
  store::Vector{Pair{AttrKey,Ref{Any}}} # note: `nil` (i.e. `DelAttr()`) means deleted
  index::Dict{AttrKey,Int}
  lock::SortableLock
  function Entity()
    new(Vector(), Dict(), SortableLock())
  end
end
Base.:(==)(x::Entity, y::Entity) = x.lock === y.lock
Base.hash(x::Entity) = hash(x.lock)
Base.hash(x::Entity, h) = hash(x.lock, h)
Base.show(io::IO, x::Entity) = print(io, "Entity#", objectid(x.lock))
