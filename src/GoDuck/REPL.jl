
module REPL

using REPL
using REPL.LineEdit

using ReplMaker

using ..Femtoparsec
using ..RT

using ..RT.Parser: cognProc # TODO: temp test purpose


function __init__()

  if !isdefined(Base, :active_repl)
    return
  end

  function parseGoDuckTop(src::AbstractString)::Tuple{Union{Any,Unmet},Vector{Diagnosis}}
    parseGoDuck(src, cognProc) # TODO: use proper @parser function
  end

  function evalGoDuck(s)
    result, problems = parseGoDuckTop(s)
    if !isempty(problems)
      io = IOContext(stderr, :compact => true)
      for p in problems
        print(io, p)
        print(io, "\n---\n")
      end
    end
    print(IOContext(stdout, :compact => true), result)
  end

  function isCompleteGoDuck(s)
    input = String(take!(copy(LineEdit.buffer(s))))
    _result, problems = parseGoDuckTop(input)
    return !any(problem -> problem isa Incomplete, problems)
  end

  initrepl(evalGoDuck,
    prompt_text="goduck> ",
    prompt_color=49,
    start_key='}',
    mode_name="GoDuck",
    valid_input_checker=isCompleteGoDuck,
  )

end


end # module GoDuck.REPL 
