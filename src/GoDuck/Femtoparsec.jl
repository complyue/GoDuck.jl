
module Femtoparsec
"""    A monadic parser combinator lib inspired greatly by Megaparsec (https://hackage.haskell.org/package/megaparsec)

One greatest deviation from Megaparsec is that, parsing errors/failures are directed to a separate diagnostic channel (i.e. `problems`, via `@diagWith`), by any Femtoparsec parser (user written effectful parsing procedure), deciding the way to proceed (or not), on its own discretion. This makes it straight forward to implement fault tolerant parsers as a reusable part of modern code analysis toolchains (compilers/linters/interpreters gathered into IDEs).

There's also a great lack of error bundle and pretty show of errors toward text console UI, compared to Megaparsec, because the lack of demand, as we are toward IDE based paradigm.

Performance is not a focus by far, but with Julia under the hood, there seems even more optimization space than Haskell (GHC) has to offer.

"""

export LexInProgress, femtoParse, @parse, @parser, @lexeme, @sc
export @lastTok, @lastToken, @tokAhead, @nextTok, @tokenAhead, @nextToken, @diagWith
export @expectTokenOf, @expectToken
export @expect, @maybe, @expectZeroOrMore, @expectOneOrMore, @choiceFor
export Unmet, Incomplete, ExpectTerms, ExpectBracketOpen, ExpectBracketClose


export Diagnosis, diagWhere, Error, Warn, Hint
using ...RT


include("./Femtoparsec/parsing.jl")


#=note
  following submodules (i.e. Lexer and Combinators) can actually standalone, they are put here merely for namespacing purpose.

  alternative lexer and combinator-collection modules can be authored by 3rd party likewise.
=#


export lex
include("./Femtoparsec/Lexer.jl")
using .Lexer


export collectBracketed
include("./Femtoparsec/Combinators.jl")
using .Combinators


end # module GoDuck.Femtoparsec
