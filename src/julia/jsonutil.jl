# JSON for scalar graph/amplitude metadata without extra Julia dependencies.
function fm2json(x)
    if x === nothing
        return "null"
    elseif x isa AbstractString
        return "\"" * join((c == '"' ? "\\\"" : c == '\\' ? "\\\\" :
            c == '\n' ? "\\n" : c == '\r' ? "\\r" : c == '\t' ? "\\t" :
            Int(c) < 32 ? "\\u" * string(Int(c), base=16, pad=4) : string(c)
            for c in x)) * "\""
    elseif x isa AbstractDict
        return "{" * join((fm2json(string(k))*":"*fm2json(x[k]) for k in sort(collect(keys(x)),by=string)),",") * "}"
    elseif x isa AbstractVector
        return "[" * join(fm2json.(x),",") * "]"
    elseif x isa Bool
        return x ? "true" : "false"
    elseif x isa Integer || x isa AbstractFloat
        return string(x)
    else
        error("Unsupported graph metadata type: $(typeof(x))")
    end
end
