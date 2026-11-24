# Use the SAME Julia project/depot that generated the amplitudes.
using JLD2, SHA

# A JSON writer for JLD2's string/number/vector/dictionary metadata; no extra deps.
function json(x)
    if x isa AbstractString
        return "\"" * join((c == '"' ? "\\\"" : c == '\\' ? "\\\\" :
            c == '\n' ? "\\n" : c == '\r' ? "\\r" : c == '\t' ? "\\t" :
            Int(c) < 32 ? "\\u" * string(Int(c), base=16, pad=4) : string(c)
            for c in x)) * "\""
    elseif x isa AbstractDict
        return "{" * join((json(string(k))*":"*json(x[k]) for k in sort(collect(keys(x)),by=string)),",") * "}"
    elseif x isa AbstractVector
        return "[" * join(json.(x),",") * "]"
    elseif x isa Bool
        return x ? "true" : "false"
    elseif x isa Integer
        return string(x)
    else
        error("Unsupported metadata type: $(typeof(x))")
    end
end

length(ARGS) == 2 || error("usage: export.jl amplitude_directory output.json")
indir, outfile = abspath.(ARGS)
files = filter(f -> occursin(r"^amp\d+\.jld2$",basename(f)), readdir(indir;join=true))
sort!(files, by=f->parse(Int,match(r"amp(\d+)",basename(f))[1]))
isempty(files) && error("No amp<number>.jld2 files in $indir")
records = Any[]
for f in files
    d = JLD2.load(f)
    d["source_file"] = basename(f)
    d["source_sha256"] = bytes2hex(sha256(read(f)))
    push!(records,d)
end
open(outfile,"w") do io
    print(io,json(Dict("schema"=>"feammatch2.feamgen.v1","amplitudes"=>records)))
end
println("Exported $(length(records)) amplitudes to $outfile")
