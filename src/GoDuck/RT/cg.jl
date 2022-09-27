
#= Categorial Grammar

  https://en.wikipedia.org/wiki/Categorial_grammar
  https://en.wikipedia.org/wiki/Combinatory_categorial_grammar

=#

# abstract type for CG
macro cg!type(name::Union{Symbol,QuoteNode}, super::Union{Symbol,Nothing}=nothing)
  if name isa QuoteNode
    name = name.value
  end
  @assert name isa Symbol
  return esc(quote
    abstract type $name <: $super end
  end)
end

# ADT triggering atom for CG
macro cg!term(sym::Union{Symbol,QuoteNode}, sig)
  if sym isa QuoteNode
    sym = sym.value
  end
  @assert sym isa Symbol
  return esc(quote
    struct $sym

    end
  end)
end

# ADT for CG, optionally with a constructing signature
macro cg!data(sig)
  return esc(quote
    struct $name

    end
  end)
end


#= standalone expression, possibly with nested expressions

nested expressions can semantically be sequential or parallel to eachothers

the whole expression evaluates to the last nested expresssion with sequential semantics, or a control item managing synchronization with parallel semantics
=#
@cg!type Paragraph

#= single standalone expression

single sentence paragraph is a special case of multiple-sentence-possible paragraph
=#
@cg!type Sentence Paragraph

#= general expression to be joined with other expressions or standalone

a phrase is a sentence only if standalone, i.e. not joined with any other phrase
=#
@cg!type Phrase Sentence

#= boolean result phrase
=#
@cg!type Dictate Phrase


@cg!data OneShot(cond::Dictate)


# <consequence> if <pred>
@cg!data OrNot(cond, act) \ act::Paragraph / OneShot(cond)

@cg!data Dual(cond::Dictate, act::Paragraph, alt::Paragraph)

# if <pred> then <consequence> [ else <alternative> ]
@cg!term :if OneShot(cond) / cond::Dictate
@cg!term :then OrNot(cond, act) \ OneShot(cond) / act::Paragraph
@cg!term :else Dual(cond, act, alt) \ OrNot(cond, act) / alt::Paragraph

# <pred> ? <consequence> : <alternative>
@cg!term :? OneShot(cond) \ cond::Dictate
@cg!term :(:) Dual(cond, act, alt) \ OneShot(cond) \ act::Paragraph / alt::Paragraph


@cg!data WhileHead(cond::Dictate)
@cg!term :while WhileHead(cond) / cond::Dictate

# <repeat-act> while <pred>
@cg!data WhileLoopL(body, cond) \ body::Paragraph / WhileHead(cond)

# while <pred> <repeat-act>
@cg!data WhileLoopR(cond, body) / WhileHead(cond) / body::Paragraph


@cg!type RecvArgs Phrase
@cg!type IterSrc Phrase

# <item> in <coll>
@cg!term :in ForLoopHead(args, itsrc) \ args::RecvArgs / itsrc::IterSrc
# <item> = <coll>
@cg!term :(=) ForLoopHead(args, itsrc) \ args::RecvArgs / itsrc::IterSrc


# for i in 3..5 print i
@cg!term :for ForLoop(vars, itsrc, body) / ForLoopHead(vars, itsrc) / body::Paragraph

# print i for i in 3..5
@cg!term :for ForLoop(vars, itsrc, body) \ body::Paragraph / ForLoopHead(vars, itsrc)


# 

struct CateType
  name::Symbol
end

struct CateData
  name::Symbol
  fields::Vector{Symbol,CateType}
end

struct CateParam
  prec::Int
  name::Union{Symbol,Nothing}
  type::CateType
end

struct CateSig
  final::CateData
  lhs!args::Vector{CateParam}
  rhs!args::Vector{CateParam}
end

struct Lexicon
  patterns::Vector{CateSig}
  terms::IdDict{Symbol,Vector{CateSig}}
end
