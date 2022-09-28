
#* arg receiving

abstract type ArgReceiver <: GoDuckExpr end
# fail loudly for any subtype not overriding its show
Base.show(::IO, ::T) where {T<:ArgReceiver} = error("uncovered ArgReceiver type: $T")


struct RecvOneArg <: ArgReceiver
  name::AttrExpr
  retarget::Union{AddrExpr,Nothing}
  default::Union{GoDuckExpr,Nothing}
end
function expr_src(io::IO, r::RecvOneArg)
  print(io, r.name)
  if r.retarget !== nothing
    print(io, " as ")
    expr_src(io, r.retarget)
  end
  if r.default !== nothing
    print(io, " = ")
    expr_src(io, r.default)
  end
end


struct RecvRestPosArg <: ArgReceiver
  name::AttrKey
  retarget::Union{AddrExpr,Nothing}
end
function expr_src(io::IO, r::RecvRestPosArg)
  print(io, "*", r.name)
  if r.retarget !== nothing
    print(io, " as ")
    expr_src(io, r.retarget)
  end
end


struct RecvRestKwArg <: ArgReceiver
  name::AttrKey
  retarget::Union{AddrExpr,Nothing}
end
function expr_src(io::IO, r::RecvRestKwArg)
  print(io, "**", r.name)
  if r.retarget !== nothing
    print(io, " as ")
    expr_src(io, r.retarget)
  end
end


struct RecvRestPkArg <: ArgReceiver
  name::AttrKey
  retarget::Union{AddrExpr,Nothing}
end
function expr_src(io::IO, r::RecvRestPkArg)
  print(io, "***", r.name)
  if r.retarget !== nothing
    print(io, " as ")
    expr_src(io, r.retarget)
  end
end


const ArgsReceiver = Vector{ArgReceiver}
function expr_src(io::IO, ars::ArgsReceiver)
  print(io, "( ")
  for ar::ArgReceiver in ars
    expr_src(io, ar)
    print(io, ", ")
  end
  print(io, ")")
end


#* arg sending

abstract type ArgSender <: GoDuckExpr end
# fail loudly for any subtype not overriding its show
Base.show(::IO, ::T) where {T<:ArgSender} = error("uncovered ArgSender type: $T")


struct SendOneArg <: ArgSender
  name::Union{AttrExpr,Nothing} # named or not
  expr::GoDuckExpr
end
expr_src(io::IO, s::SendOneArg) = expr_src(io, s.expr)


struct UnpackPosArgs <: ArgSender
  expr::GoDuckExpr
end
function expr_src(io::IO, s::UnpackPosArgs)
  if s.expr isa AttrKey
    print(io, '*')
    expr_src(io, s.expr)
  else
    print(io, "*(")
    expr_src(io, s.expr)
    print(io, ")")
  end
end


struct UnpackKwArgs <: ArgSender
  expr::GoDuckExpr
end
function expr_src(io::IO, s::UnpackKwArgs)
  if s.expr isa AttrKey
    print(io, "**")
    expr_src(io, s.expr)
  else
    print(io, "**(")
    expr_src(io, s.expr)
    print(io, ")")
  end
end


struct UnpackPkArgs <: ArgSender
  expr::GoDuckExpr
end
function expr_src(io::IO, s::UnpackPkArgs)
  if s.expr isa AttrKey
    print(io, "***")
    expr_src(io, s.expr)
  else
    print(io, "***(")
    expr_src(io, s.expr)
    print(io, ")")
  end
end


const ArgsSender = Vector{ArgSender}
function expr_src(io::IO, ass::ArgsSender)
  print(io, "( ")
  for s::ArgSender in ass
    expr_src(io, s)
    print(io, ", ")
  end
  print(io, ")")
end
