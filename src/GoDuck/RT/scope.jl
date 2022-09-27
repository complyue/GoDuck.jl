
abstract type ScopeDefiner end
show_loc(io::IO, ::ScopeDefiner)::Nothing = print(io, "<unknown-scope>")


struct WorldRoot <: ScopeDefiner end
show_loc(io::IO, ::WorldRoot)::Nothing = print(io, "<world-root>")


# instantiated container of attributes according to lexical scope
struct Scope <: ScopeDefiner
  vocabulary::Ref{Vocabulary} # all operators declared, affect parsing of source text in this scope
  # this field shall behave copy-on-write, see `writable(r::Ref{Vocabulary})::Vocabulary`
  workset::Entity # lexically scoped artifacts in this scope, working memory of a procedure or adhoc scoped-block
  effects::Entity # dynamically scoped (i.e. effectful) artifacts in this scope
  # who instantiated this scope, possible values:
  #  * the instantiating procedure (definition of it), usual case
  #  * a builtin host module, or a script module from source file on disk, usual case
  #  * the lexical outer scope, in case this is backing an adhoc (nested) scoped-block
  #  * root of the runtime system, defines this as the root/top-level scope
  # note: the lexical outer scope can be obtained from the instantiating procedure in that case
  creator::ScopeDefiner
  inst_loc::SrcLocation # src location where instantiation of this scope is triggered
end

Base.:(==)(x::Scope, y::Scope) = x.entity == y.entity
Base.hash(x::Scope) = hash(x.entity)
Base.hash(x::Scope, h) = hash(x.entity, h)

show_loc(io::IO, x::Scope)::Nothing = begin
  print(io, "<*")
  show_loc(io, x.creator)
end

function Base.show(io::IO, x::Scope)
  print(io, "Scope#", objectid(x.entity.lock))

  print(io, "@")
  show_loc(io, x.creator)

  print(io, "^")
  show(io, x.inst_loc)
end
