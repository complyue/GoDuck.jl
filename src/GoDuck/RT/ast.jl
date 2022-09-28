
# literal value interpolated into AST
struct ValueExpr <: GoDuckExpr
  value::Any
  # loc::SrcLocation
end


# literal quantity expression
struct QtyExpr <: GoDuckExpr
  num::Any
  uom::Symbol
end


# attribute addressing
abstract type AddrExpr <: GoDuckExpr end

# address an attribute directly in-scope
struct AttrExpr <: AddrExpr
  key::AttrKey
  loc::SrcLocation
end

#= indirectly address an attribute from some object

this is essentially the traditional dot-notation, if `.` declared as a left-associative, high-precedence infix operator
=#
struct DotExpr <: AddrExpr
  target::AddrExpr
  attr::AttrExpr
end


# AddrExpr/AttrExpr/DotExpr needed there, so included here
include("./args.jl")


# abstract block
abstract type BlockExpr <: GoDuckExpr end

#= sequential steps

semicolon is usally used as the separator, to form one-liners, where blank lines have to be avoided
=#
struct Sequential <: BlockExpr
  steps::Vector{GoDuckExpr}
  span::SrcLocation
end

# sequential steps within a nested local scope
struct ScopedSeq <: BlockExpr
  steps::Vector{GoDuckExpr}
  span::SrcLocation
end


#= parenthese bracketed single expression

note: a leading/trailing comma is usually used to disambiguate tuple expression from this form
=#
struct ParenExpr <: GoDuckExpr
  content::GoDuckExpr
  span::SrcLocation
end


#= literal argument pack expression

note: tuple expression is a special form of this, with only positional args sent
=#
struct ApkExpr <: GoDuckExpr
  args::ArgsSender
  span::SrcLocation
end


# literal array expression
struct ArrayExpr <: GoDuckExpr
  items::Vector{GoDuckExpr}
  span::SrcLocation
end


# literal dict expression
struct DictExpr <: GoDuckExpr
  entries::Vector{Tuple{GoDuckExpr,GoDuckExpr}}
  span::SrcLocation
end


# indexing expression
struct IndexExpr <: GoDuckExpr
  target::GoDuckExpr
  index::GoDuckExpr
end


# function-application / procedure-call expression
struct CallExpr <: GoDuckExpr
  target::GoDuckExpr
  args::ArgsSender
end


# procedure/function definition expression
struct ProcExpr <: GoDuckExpr
  name::Union{AttrExpr,Nothing}
  args::ArgsReceiver
  body::GoDuckExpr
end


# conditional expression
struct IfExpr <: GoDuckExpr
  cond::GoDuckExpr
  t::GoDuckExpr
  f::GoDuckExpr
end


# do-while loop expression
struct DoLoopExpr <: GoDuckExpr
  body::GoDuckExpr
  cond::GoDuckExpr
end


# while loop expression
struct WhileLoopExpr <: GoDuckExpr
  cond::GoDuckExpr
  body::GoDuckExpr
end


# for loop expression
struct ForLoopExpr <: GoDuckExpr
  vars::ArgsReceiver
  iter::GoDuckExpr
  body::GoDuckExpr
end


# condition testing branch expression
struct BranchExpr <: GoDuckExpr
  cond::GoDuckExpr
  branch::GoDuckExpr
end


# pattern matching branch expression
struct PatternExpr <: GoDuckExpr
  pattern::GoDuckExpr
  cond::Union{GoDuckExpr,Nothing}
  branch::GoDuckExpr
end


# yield expression
struct YieldExpr <: GoDuckExpr
  value::GoDuckExpr
end


# return expression
struct ReturnExpr <: GoDuckExpr
  value::GoDuckExpr
end


# break expression
struct BreakExpr <: GoDuckExpr end


# continue expression
struct ContinueExpr <: GoDuckExpr end


# fallthrough expression
struct FallthroughExpr <: GoDuckExpr end


# throw expression
struct ThrowExpr <: GoDuckExpr
  value::GoDuckExpr
end


# rethrow expression
struct RehrowExpr <: GoDuckExpr end



# Concrete Syntax Tree
abstract type ConcreteExpr <: GoDuckExpr end

# note: whitespace tokens are stripped off from concrete syntax nodes


#= sequence of blank line separated sentences

technically, such separators are `LBRN(n) where n >= 2` (i.e. 2 or more consecutive line breaks), those happen to visually appear as 1+ blank lines.

note not-so-blank lines those with only whitepace chars should have been trimmed by the code formatter, thus should not appear in well-formed Julie source.
=#
struct ConcreteBlock <: ConcreteExpr
  sentences::Vector{GoDuckExpr}
end

# bracketed block of concrete sentences
struct Bracketed <: ConcreteExpr
  quotation::String
  sentences::Vector{GoDuckExpr}
end

#= apply niladic operation

note: will only parse so when the identifier is declared as a niladic operator
=#
struct NiladicAp <: ConcreteExpr
  opsym::String
end

#= apply unary prefix operation

for a leading identifier not declared as any sort of operator, unless it's followed by a rhs arg-taking operator, it'll parse as a prefix operation.

this may create the intuition that whitespace serves as a high-precedence right-associative operator doing function-application (contrary to Haskell's left-associative rule), e.g.:

    print typeof x

will parse as `print(typeof(x))` in Julie, while as `(print(typeof))(x)` in Haskell.

such syntax is closer to natural language rather than programming language, while Julie intends to be so. in Haskell you insert the low-precedence `$` operator to achieve the right-associative feel, but being Julia's daughter, Julie wants `$` to serve interpolation purpose naturally. also given we don't have intrinsic currying idioms to optimize, all consistently appear ideal here.

so non-operator function-applications / procedure-calls fall into this form (in the concrete parsing pass), those can be major cases across everyday codebases, keep in mind such intensity.
=#
struct PrefixAp <: ConcreteExpr
  opsym::String
  arg::GoDuckExpr
end

#= apply binary/singly-chained infix operation

e.g. `a + b + c` will parse as `InfixAp("+", [a, b, c])`, as in Julia

note:
    if the operator is declared as `infixc`, it'll parse as `InfixChainAp` instead of `InfixAp` even if chained with itself.
=#
struct InfixAp <: ConcreteExpr
  opsym::String
  #= TODO: 
    design decision - length can be 0 or 1 ?
      in case the `opsym` never declared prefix/postfix, and lacking more operands

    in this way, leading/trailing comma/semicolon is syntatically allowed, and ideally ignored, only if `,` and `;` are declared as low-precedence infix operators

    this seems desirable, but any gotcha?
  =#
  args::Vector{GoDuckExpr}
end

#= apply a group of multiple-chained infix operations

invariant: 2 <= length(opsyms) == length(args)-1

e.g. `x < y == c <= z` will parse as `InfixChainAp(["<", "==", "<="], [x, y, c, z])`

note: this is more general than Julia's specialized parsing of chained comparison

Expr> x < y == c <= z
Expr
  head: Symbol comparison
  args: Array{Any}((7,))
    1: Symbol x
    2: Symbol <
    3: Symbol y
    4: Symbol ==
    5: Symbol c
    6: Symbol <=
    7: Symbol z
quote
    x < y == c <= z
end

but any case other than chained comparison that to leverage this generality?
=#
struct InfixChainAp <: ConcreteExpr
  opsyms::Vector{String}
  args::Vector{GoDuckExpr}
end

#= apply unary postfix operation

note: will only parse so when the identifier is declared as a postfix operator
=#
struct PostfixAp <: ConcreteExpr
  arg::GoDuckExpr
  opsym::String
end
