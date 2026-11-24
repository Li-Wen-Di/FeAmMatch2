"""Exact colored-graph grouping for scalar zero-external-momentum amplitudes.

External labels may be forgotten only at p=0. Vertex interaction types and
internal field colors remain fixed; exact canonical labeling, rather than a
graph hash alone, establishes equivalence. QGRAF symmetry factors are summed.
"""
import argparse
import copy
import hashlib
import json
from collections import defaultdict
from itertools import permutations,product
from pathlib import Path
from fractions import Fraction


def canonical_key(graph):
    vertices=sorted(graph['vertices'],key=lambda v:v['vertex_index'])
    ids=[v['vertex_index'] for v in vertices]
    external=defaultdict(list)
    for key in ('incoming_propagators','outgoing_propagators'):
        for edge in graph.get(key) or []:
            external[edge['vertex_index']].append(edge['field'])
    labels={v['vertex_index']:(tuple(sorted(v['fields'])),tuple(sorted(external[v['vertex_index']])))
            for v in vertices}
    adjacency=defaultdict(list);incident=defaultdict(list)
    for edge in graph.get('remnant_propagators') or []:
        a,b=edge['birth_index'],edge['death_index'];field=edge['field']
        adjacency[tuple(sorted((a,b)))].append(field)
        incident[a].append((field,b));incident[b].append((field,a))
    palette={v:i for i,v in enumerate(sorted(set(labels.values())))}
    colors={v:palette[labels[v]] for v in ids}
    while True:
        signatures={v:(colors[v],tuple(sorted((f,colors[w]) for f,w in incident[v]))) for v in ids}
        palette={v:i for i,v in enumerate(sorted(set(signatures.values())))}
        refined={v:palette[signatures[v]] for v in ids}
        stable=len(set(refined.values()))==len(set(colors.values()))
        colors=refined
        if stable:
            break
    classes=[[v for v in ids if colors[v]==c] for c in sorted(set(colors.values()))]
    best=None
    for choices in product(*(permutations(c) for c in classes)):
        order=sum((tuple(c) for c in choices),())
        key=(tuple(labels[v] for v in order),tuple(tuple(sorted(adjacency[tuple(sorted((a,b)))]))
             for i,a in enumerate(order) for b in order[i:]))
        if best is None or key<best:
            best=key
    return best


def compress(graphs):
    groups={};cache={};labeled_count=0
    for graph in graphs:
        labeled_count+=1
        # QGRAF often changes only external labels. Cache the labeled internal
        # graph before doing the small exact canonical-labeling search.
        vertices=tuple((v['vertex_index'],tuple(sorted(v['fields']))) for v in graph['vertices'])
        ext=tuple(sorted((e['vertex_index'],e['field'])
             for k in ('incoming_propagators','outgoing_propagators') for e in graph.get(k) or []))
        edges=tuple(sorted((min(e['birth_index'],e['death_index']),max(e['birth_index'],e['death_index']),e['field'])
                           for e in graph.get('remnant_propagators') or []))
        labeled=(vertices,ext,edges)
        if labeled not in cache:
            cache[labeled]=canonical_key(graph)
        key=cache[labeled]
        weight=Fraction(graph.get('sign',1))*Fraction(str(graph['symmetry_factor']))
        if key not in groups:
            groups[key]={'graph':copy.deepcopy(graph),'weight':Fraction(0),'members':[]}
        group=groups[key];group['weight']+=weight
        group['members'].append({'diagram_index':graph['diagram_index'],'signed_symmetry_factor':str(weight)})
    representatives=[];records=[]
    for key,group in groups.items():
        graph=group['graph'];graph['sign']=1;graph['symmetry_factor']=str(group['weight'])
        if group['weight']!=0:
            representatives.append(graph)
        records.append({'canonical_sha256':hashlib.sha256(repr(key).encode()).hexdigest(),
                        'representative_diagram':graph['diagram_index'],
                        'summed_symmetry_factor':str(group['weight']),'emitted':group['weight']!=0,'members':group['members']})
    return representatives,{'schema':'feammatch2.zero-momentum-groups.v1',
        'condition':'nonderivative real scalars; all external momenta zero',
        'labeled_diagrams':labeled_count,'representatives':len(representatives),'groups':records}


def main():
    ap=argparse.ArgumentParser(description=__doc__)
    ap.add_argument('source');ap.add_argument('output');ap.add_argument('report')
    args=ap.parse_args()
    reps,report=compress(json.loads(Path(args.source).read_text()))
    Path(args.output).write_text(json.dumps(reps,separators=(',',':'))+'\n')
    Path(args.report).write_text(json.dumps(report,separators=(',',':'))+'\n')
    print('FM2_ZERO_MOMENTUM_GROUPS:',report['labeled_diagrams'],'->',report['representatives'],flush=True)


if __name__=='__main__':
    main()
