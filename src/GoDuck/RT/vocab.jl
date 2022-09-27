
# TODO: a value of this enum may be made available to a called procedure,
#       reflecting how it's called syntatically, via some implicit or explicit argument?
#       how useful is it? either to implement it or abandon the idea!
@enum Fixity Niladic Prefix Infix Postfix

const Precedence = Int
@enum Associativity LeftAsso = -1 NoAsso = 0 RightAsso = 1

abstract type OpDecl end

# niladic/nullary operation, i.e. no argument at all
# providing some constant value (e.g. `nil`) or side-effect (e.g. `chan` construction)
struct NiladicDecl <: OpDecl
  sym::Symbol
end

# take a single argument (use ArgPack for flexibility of more args) after the identifier
# note: usual functions are syntatically prefix operations,
#       but undeclared operators are assumed `prefix 100`,
#       so you don't declare a usual (alphanumerically named) function identifier as prefix op,
#       unless a different precedence is more proper. though any good example?
struct PrefixDecl <: OpDecl
  prec::Precedence
  sym::Symbol
end

# operator taking 2 or more chained arguments around it
# infix, infixl, infixr are pretty much the same as in Haskell (and StandardML?)
struct InfixDecl <: OpDecl
  asso::Associativity
  prec::Precedence
  sym::Symbol
end

# a group of operators those can be chained together,
# each one standalone (i.e. no chaining) is allowed but always be non-associative (i.e. considered infix/NoAsso)
# infixc is to support chained comparisons as in Julia (and Python though harder-coded)
struct InfixChainDecl <: OpDecl
  prec::Precedence
  syms::Vector{Symbol}
end

# take a single argument before the identifier
# such operators are unusual in mainstream programming languages,
# widely seen ones are limited to only (++) and (--), as in C/C++ plus the likes,
# also some of StandardML's type constructors, e.g. `list` `ref` are syntatically postfix ops
struct PostfixDecl <: OpDecl
  prec::Precedence
  sym::Symbol
end

# list of all declared operators per a specialized type
struct OpDict{T<:OpDecl}
  ops::IdDict{Symbol,T}
  function OpDict() where {T}
    new{T}(IdDict{Symbol,T}())
  end
  # copy constructor, usually for COW
  function OpDict(other::OpDict{T}) where {T<:OpDecl}
    new{T}(copy(other.ops))
  end
end

# all declared operators per a specific lexical scope
#
# a single identifier symbol can be declared as prefix/infix/postfix ops separately at the same time
# TODO: precedence can conflict in those cases? to detect and reject such declarations?
#
# note: undeclared identifiers shall be treated as `prefix 100`, simulating
#       that function application binds the tightest as in Haskell
struct Vocabulary
  write_protected::Bool # to implement copy-on-write,
  # a copy with this flag set to false must be made, with all op list copied as well,
  # for any update to the op lists

  niladic::OpDict{NiladicDecl}
  prefix::OpDict{PrefixDecl}
  infix::OpDict{Union{InfixDecl,InfixChainDecl}}
  postfix::OpDict{PostfixDecl}
end

function writable(r::Ref{Vocabulary})::Vocabulary
  v = r[]
  if v.write_protected
    r[] = v = Vocabulary(false,
      OpDict(v.niladic), OpDict(v.prefix), OpDict(v.infix), OpDict(v.postfix))
  end
  v
end

# note: it's assumed that silently overwriting of op declarations, as in StandardML,
#       so we always straightly declare into the op list in context

function declare_niladic!(vr::Ref{Vocabulary}, sym::Symbol)::NiladicDecl
  voc = writable(vr)
  decl = NiladicDecl(sym)
  voc.niladic.ops[sym] = decl
  return decl
end

function declare_prefix!(vr::Ref{Vocabulary}, prec::Precedence, sym::Symbol)::PrefixDecl
  voc = writable(vr)
  decl = PrefixDecl(prec, sym)
  voc.prefix.ops[sym] = decl
  return decl
end

function declare_infixl!(vr::Ref{Vocabulary}, prec::Precedence, sym::Symbol)::InfixDecl
  voc = writable(vr)
  decl = InfixDecl(LeftAsso, prec, sym)
  voc.infix.ops[sym] = decl
  return decl
end

function declare_infix!(vr::Ref{Vocabulary}, prec::Precedence, sym::Symbol)::InfixDecl
  voc = writable(vr)
  decl = InfixDecl(NoAsso, prec, sym)
  voc.infix.ops[sym] = decl
  return decl
end

function declare_infixc!(vr::Ref{Vocabulary}, chain_to::Symbol, sym::Symbol)::InfixChainDecl
  voc = writable(vr)
  decl = get(voc.infix.ops, chain_to, nothing)
  if decl === nothing
    error("chain operator $chain_to has not been defined yet")
  elseif decl isa InfixChainDecl
    push!(decl.syms, sym)
  else
    @assert decl isa InfixDecl "non-exhaustive infix type handling ?!"
    if decl.asso !== NoAsso
      error("you don't chain an associative operator: $chain_to")
    end
    decl = InfixChainDecl(decl.prec, Symbol[decl.sym, sym])
    voc.infix.ops[chain_to] = decl
  end
  voc.infix.ops[sym] = decl
  return decl
end

function declare_infixr!(vr::Ref{Vocabulary}, prec::Precedence, sym::Symbol)::InfixDecl
  voc = writable(vr)
  decl = InfixDecl(RightAsso, prec, sym)
  voc.infix.ops[sym] = decl
  return decl
end

function declare_postfix!(vr::Ref{Vocabulary}, prec::Precedence, sym::Symbol)::PostfixDecl
  voc = writable(vr)
  decl = PostfixDecl(prec, sym)
  voc.postfix.ops[sym] = decl
  return decl
end
