using Pkg

python = Sys.which(get(ENV, "FEAMMATCH2_PYTHON", "python3"))
isnothing(python) && error("Install Python 3 or set FEAMMATCH2_PYTHON.")
ENV["PYTHON"] = python
registry_uuid = Base.UUID("fb91e516-02d6-4456-8893-ab966867df81")
if !any(registry -> registry.uuid == registry_uuid, Pkg.Registry.reachable_registries())
    Pkg.Registry.add(Pkg.RegistrySpec(
        url="https://code.ihep.ac.cn/IHEP-Multiloop/JuliaRegistry.git"))
end
Pkg.instantiate()
pycall = Base.locate_package(Base.PkgId(Base.UUID("438e738f-606a-5dbb-bf0a-cddfbfd45ab0"), "PyCall"))
if !isnothing(pycall) && !isfile(joinpath(dirname(dirname(pycall)), "deps", "deps.jl"))
    Pkg.build("PyCall")
end
using FeAmGen, JLD2, YAML
