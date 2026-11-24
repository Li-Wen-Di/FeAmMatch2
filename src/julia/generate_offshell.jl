# Dedicated scalar matching adapter. Overrides are confined to this Julia process.
# Upstream FeAmGen installation is never changed.
using FeAmGen
using YAML
using SHA
include("jsonutil.jl")
length(ARGS) >= 2 || error("usage: generate_offshell.jl process.json model_search_path")
@eval FeAmGen begin
    const FM2_SCALAR_ZERO_EXTERNAL = Dict{Basic,Basic}()
    function generate_kin_relation(graph_list::Vector{Graph})::Dict{Basic,Basic}
        # Keep SP and tree Den objects symbolic. Momentum conservation is
        # separately implemented by generate_amplitude/factor_out_loop_den.
        return Dict{Basic,Basic}()
    end

    # A neutral self-loop occupies two ordered rays of the same vertex. The
    # upstream incoming-edge membership test gives both rays the same sign;
    # distinguish the occurrences, as get_link_lorentz already does. Scalar
    # potential vertices were insensitive to this; derivative EFT vertices are
    # not. Keep this correction local to the matching adapter process.
    function get_link_momentum(g::Graph, vert_id::Int64, link_index::Int64)::Basic
        @assert link_index > 0
        in_edges = get_in_edge_list(g, vert_id)
        out_edges = get_out_edge_list(g, vert_id)
        edge = edge_from_link_index(g, vert_id, link_index)
        direction = edge in in_edges ? -1 : 1
        if edge in in_edges && edge in out_edges && is_neutral(edge.property[:particle])
            indices = g.node_list[vert_id].property[:propagator_index_list]
            first_ray = findfirst(==(indices[link_index]), indices)
            direction = first_ray == link_index ? -1 : 1
        end
        return direction * edge.property[:momentum]
    end

    # Adapted from FeAmGen's MIT-licensed factor_out_loop_den. Cancel the
    # original propagator product first, then perform exact zero-kinematic
    # substitutions BEFORE expanding scalar derivative numerators. The
    # unmodified ordering expands a product of four large off-shell vertices
    # even when the requested Taylor coefficient has all external momenta zero.
    function factor_out_loop_den(g::Graph, lorentz_list::Vector{Basic},
                                 mom_conserv::Pair{Basic,Basic})::Tuple{Vector{Basic},Vector{Basic},Vector{Int64}}
        @funs Den
        den_prod = one(Basic)
        width_den_prod = one(Basic)
        for edge in filter(x -> x.property[:style] == "Loop", g.edge_list)
            mom = edge.property[:momentum]
            particle = edge.property[:particle]
            den_prod *= Den(normalize_loop_mom_single(subs(mom, mom_conserv)), particle.mass, 0)
            width_den_prod *= Den(mom, particle.mass, particle.width)
        end
        new_lorentz_list = map(lorentz_list) do expression
            numerator = expression / width_den_prod
            if !isempty(FM2_SCALAR_ZERO_EXTERNAL)
                numerator = subs(numerator, FM2_SCALAR_ZERO_EXTERNAL)
                scalar_zeros = Dict{Basic,Basic}()
                pending = Basic[numerator]
                seen = Set{Basic}()
                while !isempty(pending)
                    node = pop!(pending)
                    node in seen && continue
                    push!(seen, node)
                    args = get_args(node)
                    if is_FunctionSymbol(node)
                        name = get_name(node)
                        if (name == "FV" && !isempty(args) && first(args) == 0) ||
                           (name == "SP" && any(iszero, args))
                            scalar_zeros[node] = zero(Basic)
                            continue
                        end
                    end
                    append!(pending, args)
                end
                numerator = subs(numerator, scalar_zeros)
            end
            expand(numerator)
        end
        if !isempty(FM2_SCALAR_ZERO_EXTERNAL)
            den_prod = subs(den_prod, FM2_SCALAR_ZERO_EXTERNAL)
        end
        is_den(x) = is_FunctionSymbol(x) && get_name(x) == "Den"
        factors = den_prod == 1 ? Basic[] :
                  (is_den(den_prod) || is_class(:Pow, den_prod) ? Basic[den_prod] : get_args(den_prod))
        loop_den_list = Basic[]
        loop_den_xpt_list = Int64[]
        for factor in factors
            if is_den(factor)
                push!(loop_den_list, factor); push!(loop_den_xpt_list, 1)
            elseif is_class(:Pow, factor) && is_den(first(get_args(factor)))
                push!(loop_den_list, first(get_args(factor)))
                push!(loop_den_xpt_list, convert(Int64, get_args(factor)[2]))
            else
                error("Unexpected scalar loop denominator factor: $factor")
            end
        end
        return new_lorentz_list, loop_den_list, loop_den_xpt_list
    end
end
path = abspath(ARGS[1])
input = YAML.load_file(path)
if get(get(input, "fm2_graph_policy", Dict()), "zero_external_momenta", false)
    n_external = length(input["incoming"]) + length(input["outgoing"])
    # FeAmGen uses lowercase k for massless external auxiliary particles.
    # Both conventions represent external momenta and are zero in this mode.
    for i in 1:n_external, prefix in ("K", "k")
        FeAmGen.FM2_SCALAR_ZERO_EXTERNAL[FeAmGen.Basic("$prefix$i")] = FeAmGen.Basic(0)
    end
end
dir = dirname(path)
model = FeAmGen.readin_model(input; model_paths=abspath.(ARGS[2:end]))
FeAmGen.generate_QGRAF_model(model; dir=dir)
# FeAmGen's writer omits <=2-link vertices. Keep the perturbative contacts
# already read from the validated UFO, without modifying upstream FeAmGen.
if get(input, "fm2_include_local_contacts", false)
    contacts = Any[]
    open(joinpath(dir, "model.qgraf"), "a") do file
        for interaction in model.interaction_list
            n = length(interaction.link_list)
            1 <= n <= 2 || continue
            if n <= 2
                # QGRAF 3.6 does not accept degree-one interactions. The
                # Mathematica insertion driver attaches the corresponding
                # source to a zero-momentum heavy external leg instead.
                push!(contacts, Dict("fields"=>[p.name for p in interaction.link_list],
                                     "deferred"=>n == 1 ? "zero-momentum-source" : "quadratic-spurion"))
                continue
            end
            links = FeAmGen.generate_ordered_link_list(interaction.link_list)
            names = [particle.name for particle in links]
            println(file, "[", join(names, ","), "; epow='", interaction.QED_order,
                    "',gspow='", interaction.QCD_order, "',spc='", interaction.SPC_order,
                    "',qcdct='0']")
            push!(contacts, Dict("fields"=>names, "QED_order"=>interaction.QED_order,
                                 "QCD_order"=>interaction.QCD_order))
        end
    end
    write(joinpath(dir,"local-contacts.json"), fm2json(contacts))
    println("FM2_LOCAL_CONTACTS: ",length(contacts))
end
FeAmGen.logging_model(model; dir=dir)
FeAmGen.generate_Feynman_diagram(model, input; dir=dir)
cp(joinpath(dir,"qgraf_out.dat"),joinpath(dir,"graphs.yaml");force=true)
# Selection rules are produced by Mathematica and executed by one audited worker
# for both compressed and uncompressed tasks. No local fallback policy exists.
python = ENV["FEAMMATCH2_PYTHON"]
helper = ENV["FEAMMATCH2_GRAPH_HELPER"]
source = joinpath(dir,"graphs.yaml")
request = Dict("schema"=>"feammatch2.helper-request.v1", "task_id"=>input["fm2_helper_task_id"],
 "action"=>"select-graphs", "policy"=>input["fm2_graph_policy"], "source"=>source,
 "output_directory"=>dir, "response_file"=>joinpath(dir,"graph-selection.response.json"),
 "inputs"=>[Dict("path"=>source,"sha256"=>bytes2hex(sha256(read(source))))])
request_path = joinpath(dir,"graph-selection.request.json")
write(request_path, fm2json(request))
run(`$python $helper --request $request_path`)
receipt = YAML.load_file(request["response_file"])
receipt["schema"] == "feammatch2.helper-response.v1" || error("Wrong helper response schema")
receipt["task_id"] == request["task_id"] || error("Wrong helper task identity")
receipt["status"] == "ok" || error("Graph helper failed")
selected = YAML.load_file(joinpath(dir,"graphs-selected.json"))
graphs = Dict{Any,Any}("FeynmanDiagrams" => selected)
graphs["FeynmanDiagrams"] = selected
YAML.write_file(joinpath(dir,"qgraf_out.dat"), graphs)
println("FM2_AMPLITUDE_REPRESENTATIVES: $(length(selected))")
if isempty(selected)
    println("FM2_OFFSHELL_GENERATION_COMPLETE_EMPTY")
    exit(0)
end
FeAmGen.generate_amplitude(model, input; dir=dir)
println("FM2_OFFSHELL_GENERATION_COMPLETE")
