
module REPL

using REPL
using REPL.LineEdit

using ReplMaker

using ..Femtoparsec
using ..Compiler

using ..Compiler.Parser: cognProc # TODO: temp test purpose


function __init__()

  if !isdefined(Base, :active_repl)
    return
  end

  function parse_julie(src::AbstractString)::Tuple{Union{Any,Unmet},Vector{Diagnosis}}
    parseGoDuck(src, cognProc) # TODO: use proper @parser function
  end

  function eval_julie(s)
    result, problems = parse_julie(s)
    if !isempty(problems)
      io = IOContext(stderr, :compact => true)
      for p in problems
        print(io, p)
        print(io, "\n---\n")
      end
    end
    print(IOContext(stdout, :compact => true), result)
  end

  function is_complete_julie(s)
    input = String(take!(copy(LineEdit.buffer(s))))
    _result, problems = parse_julie(input)
    return !any(problem -> problem isa Incomplete, problems)
  end

  initrepl(eval_julie,
    prompt_text="julie> ",
    prompt_color=49,
    start_key='}',
    mode_name="GoDuck",
    valid_input_checker=is_complete_julie,
  )

end


end # module GoDuck.REPL 
