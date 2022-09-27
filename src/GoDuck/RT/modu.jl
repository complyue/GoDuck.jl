
struct HostModule <: ScopeDefiner
  name::Symbol # full import path
  scope::Scope # scope of this module
  outer::Scope # the lexical outer scope, should be root scope of the runtime world
end
show_loc(io::IO, m::HostModule)::Nothing = print(io, "<host-module>:", m.name)


struct ScriptModule <: ScopeDefiner
  name::Symbol # full import path, also the relative path to some resolution root directory,
  # but without magic names as with source file (e.g. `/__init__`) nor extension names (e.g. `.jle`)
  path::Symbol # full path to containing directory of the module source file,
  # used to resolve relative imports from within this module
  file::Symbol # absolute path of the module source file, uniquely identify this module
  scope::Scope # scope of this module
  outer::Scope # the lexical outer scope, should be root scope of the runtime world
end
show_loc(io::IO, m::ScriptModule)::Nothing = print(io, "<module>:", m.name, " @ ", m.file)
