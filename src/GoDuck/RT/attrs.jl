
"""    universal attribute identifier
"""
struct UAID <: GoDuckExpr
  uuid::UUID
  alias::Symbol
end
Base.:(==)(x::UAID, y::UAID) = x.uuid == y.uuid
Base.hash(x::UAID) = hash(x.uuid)
Base.hash(x::UAID, h::UInt) = hash(x.uuid, h)
expr_src(io::IO, u::UAID) = print(io, '@', string(u.alias), " #=", string(u.uuid), "=#")


const AttrKey = Union{Symbol,UAID}
Base.:(==)(x::UAID, y::Symbol) = string(x.uuid) == string(y)
Base.:(==)(x::Symbol, y::UAID) = string(x) == string(y.uuid)

# make any `Symbol` behave like a `GoDuckExpr`, so `AttrKey` will too
expr_src(io::IO, a::Symbol) = print(io, string(a))


"""    immutable collection of values keyed by `AttrKey`,
with order of key/value pairs mattering in comparision, and reserved for iteration
"""
# todo: worth to avoid interning `uuid`s as symbols?
# TODO: move `keys` to type level? Then how to have `UUID`s at type level?
struct KwArgs <: GoDuckExpr
  keys::Tuple
  data::NamedTuple

  function KwArgs(ks::Tuple, data::NamedTuple)
    @assert length(ks) === length(data)
    new(ks, data)
  end

  function KwArgs(args...; kwargs...)
    idxs = IdDict{Symbol,Int}()
    ks, ns, vs = AttrKey[], Symbol[], Any[]
    function ingest(k::Union{Symbol,AttrKey}, v::Any)::Nothing
      if k isa Symbol
        key = name = k
      elseif k isa UAID
        key = k
        name = Symbol(k.uuid)
      else
        @assert false
      end
      key::AttrKey
      name::Symbol
      idx = get(idxs, name, nothing)
      if idx isa Nothing
        push!(ks, key)
        push!(ns, name)
        push!(vs, v)
        idxs[name] = length(ks)
      else
        ks[idx] = key
        ns[idx] = name
        vs[idx] = v
      end
      return
    end
    for arg in args
      if arg isa Pair
        k, v = arg
        ingest(k, v)
      else
        @assert false "not supported as kwarg: $arg of type $(typeof(arg))"
      end
    end
    for (k, v) in kwargs
      ingest(k, v)
    end
    new(tuple(ks...), NamedTuple{tuple(ns...)}(tuple(vs...)))
  end
end

Base.:(==)(x::KwArgs, y::KwArgs)::Bool = x.data == y.data

Base.length(kwargs::KwArgs) = length(kwargs.keys)
function Base.iterate(kwargs::KwArgs, i::Integer=1)
  i > length(kwargs.keys) && return nothing
  return (kwargs.keys[i] => kwargs.data[i]), i + 1
end
Base.get(kwargs::KwArgs, k, d) =
  if k isa Symbol
    get(kwargs.data, k, d)
  elseif k isa UAID
    get(kwargs.data, Symbol(k.uuid), d)
  else
    d
  end
Base.getindex(kwargs::KwArgs, k) =
  if k isa Symbol
    getindex(kwargs.data, k)
  elseif k isa UAID
    getindex(kwargs.data, Symbol(k.uuid))
  else
    throw(MethodError(kwargs, k))
  end

function expr_src(io::IO, kwargs::KwArgs)::Nothing
  for (k, v) in zip(kwargs.keys, values(kwargs.data))
    expr_src(io, k)
    print(io, " = ")
    show(io, v)
    print(io, ", ")
  end
end

function take(kwargs::KwArgs, key::AttrKey, default::T)::Tuple{KwArgs,T} where {T}
  for (i, k) in enumerate(kwargs.keys)
    # TODO: optimize perf by looking up the field index from kwargs.data (NamedTuple), how?
    if k == key
      ks = kwargs.keys
      ns, vs = keys(kwargs.data), values(kwargs.data)
      return KwArgs(
        tuple(ks[1:i-1]..., ks[i+1:end]...),
        NamedTuple{tuple(ns[1:i-1]..., ns[i+1:end]...)}(
          tuple(vs[1:i-1]..., vs[i+1:end]...)
        )
      ), vs[i]
    end
  end
  return kwargs, default
end


"""    arguments pack
"""
struct ArgsPack <: GoDuckExpr
  args::Tuple
  kwargs::KwArgs

  function ArgsPack(args::Tuple, kwargs::KwArgs)
    new(args, kwargs)
  end

  function ArgsPack(args...; kwargs...)
    posargs, kvpairs = parse(args...)
    new(tuple(posargs...), KwArgs(kvpairs...; kwargs...))
  end

  function parse(args...)::Tuple{Vector{Any},Vector{Pair}}
    posargs, kvpairs = [], Pair[]
    for arg in args
      if arg isa Pair
        push!(kvpairs, arg)
      else
        push!(posargs, arg)
      end
    end
    posargs, kvpairs
  end
end

Base.length(apk::ArgsPack) = length(apk.args) + length(apk.kwargs)
function Base.iterate(apk::ArgsPack, i::Integer=1)
  ik = i - length(apk.args)
  ik > length(apk.kwargs.keys) && return nothing
  if ik > 0
    return (apk.kwargs.keys[ik] => apk.kwargs.data[ik]), i + 1
  end
  return (i => apk.args[i]), i + 1
end
Base.get(apk::ArgsPack, k, d) = get(apk.kwargs, k, d)
function Base.getindex(apk::ArgsPack, k)
  k isa Integer && return apk.args[k]
  return getindex(apk.kwargs, k)
end

function expr_src(io::IO, apk::ArgsPack)::Nothing
  print(io, "( ")
  for arg in apk.args
    expr_src(io, arg)
    print(io, ", ")
  end
  expr_src(io, apk.kwargs)
  print(io, ")")
end

function take(apk::ArgsPack, key::AttrKey, default::T)::Tuple{ArgsPack,T} where {T}
  kwargs, v = take(apk.kwargs, key, default)
  return ArgsPack(apk.args, kwargs), v
end


"""    Syntactic sugar to define a unique attribute identifier
"""
macro uaid(alias::Symbol, uid::Union{Nothing,UUID,AbstractString}=nothing)
  uuid::UUID = if uid isa AbstractString
    UUID(string(uid))
  elseif uid isa UUID # todo: can a macro really take a UUID arg?
    uid
  else
    uuid1()
  end
  id = UAID(uuid, alias)
  return esc(quote
    macro $alias()
      return $id
    end
  end)
end
