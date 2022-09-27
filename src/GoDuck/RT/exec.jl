
mutable struct ExecLoc
  file::Symbol
  span::SrcRange
end

const CallStack = Vector{Scope}

struct ThreadState
  exec_loc::ExecLoc
  tip_frame::Scope
  caller_frames::CallStack
end
