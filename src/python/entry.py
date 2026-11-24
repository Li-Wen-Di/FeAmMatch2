"""Internal, versioned file/graph worker. Python >=3.9; standard library only."""
import argparse
import ast
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import traceback
from fractions import Fraction

from graph_compression import compress

REQUEST = 'feammatch2.helper-request.v1'
RESPONSE = 'feammatch2.helper-response.v1'


def sha(path):
    h = hashlib.sha256()
    with Path(path).open('rb') as stream:
        for block in iter(lambda: stream.read(1048576), b''):
            h.update(block)
    return h.hexdigest()


def artifact(path):
    return {'path': str(Path(path).resolve()), 'sha256': sha(path)}


def graph_reason(graph, policy):
    vertices = {v['vertex_index'] for v in graph['vertices']}
    if not vertices:
        raise ValueError('Graph has no vertices')
    if len(vertices) != len(graph['vertices']):
        raise ValueError('Duplicate vertex identity')
    edges = graph.get('remnant_propagators') or []
    external_lines = [e for key in ('incoming_propagators', 'outgoing_propagators')
                      for e in graph.get(key) or []]
    auxiliary = set(policy.get('auxiliary_external_fields', []))
    external = {e['vertex_index'] for e in external_lines if e['field'] not in auxiliary}
    if not {e['vertex_index'] for e in external_lines} <= vertices:
        raise ValueError('External line references an undeclared vertex')
    reached = {min(vertices)}
    for _ in vertices:
        for edge in edges:
            ends = {edge['birth_index'], edge['death_index']}
            if not ends <= vertices:
                raise ValueError('Propagator references an undeclared vertex')
            if ends & reached:
                reached |= ends
    if reached != vertices:
        raise ValueError('Disconnected graph')
    if any(e['field'] in policy.get('external_only_fields', []) for e in edges):
        return 'excluded_auxiliary_internal_line'
    zero = {tuple(sorted(v)) for v in policy['zero_boundary_vertices']}
    if sum(tuple(sorted(v['fields'])) in zero for v in graph['vertices']) > policy['max_zero_vertices']:
        return 'excluded_boundary_vertex_order'
    for cut, edge in enumerate(edges):
        if edge['birth_index'] not in vertices or edge['death_index'] not in vertices:
            raise ValueError('Propagator references an undeclared vertex')
        reached = {min(vertices)}
        changed = True
        while changed:
            changed = False
            for j, e in enumerate(edges):
                if j == cut:
                    continue
                ends = {e['birth_index'], e['death_index']}
                if ends & reached and not ends <= reached:
                    reached |= ends
                    changed = True
        if reached != vertices:
            if policy['matching_object'] == '1PI':
                return 'excluded_propagator_bridge'
            if edge['field'] in policy['light_fields']:
                return 'excluded_light_bridge'
            if policy['tadpole_free'] and (not reached & external or not (vertices-reached) & external):
                return 'excluded_heavy_tadpole_bridge'
    return 'retained_' + policy['matching_object']


def validate_policy(policy):
    required = {'schema', 'matching_object', 'light_fields', 'tadpole_free',
                'zero_boundary_vertices', 'max_zero_vertices', 'compress_zero_momentum',
                'zero_external_momenta', 'derivative_order'}
    if not isinstance(policy, dict) or not required <= policy.keys():
        raise ValueError('Incomplete explicit graph policy')
    if policy['schema'] != 'feammatch2.graph-policy.v1' or policy['matching_object'] not in ('1PI', '1LPI'):
        raise ValueError('Unsupported graph policy or matching object')
    if not isinstance(policy['light_fields'], list) or not policy['light_fields']:
        raise ValueError('Declare light fields')
    if not isinstance(policy.get('auxiliary_external_fields', []), list) or any(
            not isinstance(x, str) for x in policy.get('auxiliary_external_fields', [])):
        raise ValueError('Auxiliary external field identities must be strings')
    if not isinstance(policy.get('external_only_fields', []), list) or any(
            not isinstance(x, str) for x in policy.get('external_only_fields', [])):
        raise ValueError('External-only field identities must be strings')
    if any(type(policy[k]) is not bool for k in ('tadpole_free', 'compress_zero_momentum', 'zero_external_momenta')):
        raise ValueError('Graph policy switches must be booleans')
    if type(policy['max_zero_vertices']) is not int or policy['max_zero_vertices'] < 0:
        raise ValueError('Invalid boundary order')
    if type(policy['derivative_order']) is not int or policy['derivative_order'] < 0:
        raise ValueError('Invalid derivative order')
    if policy['compress_zero_momentum'] and (not policy['zero_external_momenta'] or policy['derivative_order'] != 0):
        raise ValueError('Compression requires zero external momenta and no derivative projection')


def stream_graphs(path):
    graph = section = item = None
    def value(text):
        text = text.strip()
        if re.fullmatch(r'[+-]?\d+', text):
            return int(text)
        if re.fullmatch(r'[+-]?\d+/\d+', text):
            return text
        if text.startswith(('"', '[')):
            return json.loads(text)
        raise ValueError('Unexpected QGRAF value: ' + text[:100])
    with Path(path).open() as stream:
        for number, line in enumerate(stream, 1):
            if not line.strip() or line.lstrip().startswith('#') or line.strip() == 'FeynmanDiagrams:':
                continue
            if line.startswith('- # Diagram'):
                if graph is not None:
                    yield graph
                graph = {}; section = item = None
                continue
            if graph is None:
                raise ValueError('Unexpected graph header at line %d' % number)
            if line.startswith('    - # '):
                if section is None:
                    raise ValueError('Missing graph section')
                item = {}; graph[section].append(item)
                continue
            indent = len(line)-len(line.lstrip())
            if indent not in (2, 6) or ':' not in line:
                raise ValueError('Unexpected QGRAF line %d' % number)
            key, text = line.strip().split(':', 1)
            if indent == 2:
                if not text.strip():
                    section = key; graph[key] = []; item = None
                else:
                    graph[key] = value(text)
            else:
                if item is None:
                    raise ValueError('Missing graph list item')
                item[key] = value(text)
    if graph is not None:
        yield graph


def select_graphs(request):
    policy = request['policy']; validate_policy(policy)
    directory = Path(request['output_directory']); source = Path(request['source'])
    counts = {}; total = selected_count = 0
    ledger = directory / 'graph-decisions.jsonl'
    with ledger.open('w') as log:
        def selected():
            nonlocal total, selected_count
            for graph in stream_graphs(source):
                total += 1
                reason = graph_reason(graph, policy)
                counts[reason] = counts.get(reason, 0) + 1
                weight = Fraction(graph.get('sign', 1))*Fraction(str(graph['symmetry_factor']))
                log.write(json.dumps({'diagram_index': graph['diagram_index'], 'decision': reason,
                                      'signed_symmetry_factor': str(weight)}, separators=(',', ':'))+'\n')
                if reason == 'retained_' + policy['matching_object']:
                    selected_count += 1
                    yield graph
        target = directory / 'graphs-selected.json'
        if policy['compress_zero_momentum']:
            reps, grouping = compress(selected())
            target.write_text(json.dumps(reps, separators=(',', ':'))+'\n')
            representatives = len(reps)
        else:
            representatives = 0
            with target.open('w') as output:
                output.write('[')
                for graph in selected():
                    if representatives:
                        output.write(',')
                    output.write(json.dumps(graph, separators=(',', ':')))
                    representatives += 1
                output.write(']\n')
            grouping = {'schema': 'feammatch2.zero-momentum-groups.v1', 'condition': 'compression_disabled',
                        'labeled_diagrams': selected_count, 'representatives': representatives, 'groups': []}
    grouping.update(source_sha256=sha(source), all_qgraf_diagrams=total,
                    policy=policy, decisions=artifact(ledger), counts=counts)
    report = directory / 'zero-momentum-groups.json'
    report.write_text(json.dumps(grouping, separators=(',', ':'))+'\n')
    return {'artifacts': [artifact(p) for p in (target, report, ledger)],
            'statistics': {'all': total, 'selected': selected_count, 'representatives': representatives}}


def normalize_ufo(request):
    directory = Path(request['directory']); files = sorted(directory.glob('*.py'))
    if not files:
        raise ValueError('No UFO Python sources')
    local = {p.stem for p in files} - {'__init__'}
    originals = directory.parent / (directory.name + '-original')
    originals.mkdir(exist_ok=True)
    bundle_hash = hashlib.sha256(json.dumps(
        {p.name: sha(p) for p in files}, sort_keys=True).encode()).hexdigest()
    snapshot = originals / bundle_hash
    snapshot.mkdir(exist_ok=True)
    transformed = {}; changes = []
    for path in files:
        text = path.read_text()
        # The two Python-2 raise statements in FeynRules' object_library template
        # are the only legacy syntax accepted. Other unsupported syntax fails AST parsing.
        text = re.sub(r'(?m)^(\s*)raise ([A-Za-z_]\w*), (.+)$', r'\1raise \2(\3)', text)
        text = re.sub(r'(?m)^from (\w+) import ',
                      lambda m: 'from ' + ('.' if m[1] in local else '') + m[1] + ' import ', text)
        text = re.sub(r'(?m)^import (\w+)\s*$',
                      lambda m: 'from . import '+m[1] if m[1] in local else m[0], text)
        text = re.sub(r'(?m)^import (\w+) as (\w+)\s*$',
                      lambda m: 'from . import '+m[1]+' as '+m[2] if m[1] in local else m[0], text)
        tree = ast.parse(text, filename=path.name)
        transformed[path] = (text, tree)
    def calls(filename, kind):
        return [n.value for n in transformed[directory/filename][1].body
                if isinstance(n, ast.Assign) and isinstance(n.value, ast.Call)
                and isinstance(n.value.func, ast.Name) and n.value.func.id == kind]
    merged_groups = []
    if request.get('merge_scalar_vertices', False):
        vertex_path = directory/'vertices.py'
        _, vertex_tree = transformed[vertex_path]
        groups = {}
        remove = set()
        for statement in vertex_tree.body:
            if not (isinstance(statement, ast.Assign) and isinstance(statement.value, ast.Call)
                    and isinstance(statement.value.func, ast.Name)
                    and statement.value.func.id == 'Vertex'):
                continue
            vertex = statement.value
            kw = {k.arg: k.value for k in vertex.keywords}
            fields = tuple(x.attr for x in kw['particles'].elts)
            key = tuple(sorted(fields))
            if key not in groups:
                groups[key] = (fields, vertex, kw, 1)
                continue
            old_fields, first_vertex, first, count = groups[key]
            if fields != old_fields or ast.dump(kw['color']) != ast.dump(first['color']):
                raise ValueError('Repeated scalar UFO channel needs consistent particle and color ordering')
            offset = len(first['lorentz'].elts)
            first['lorentz'].elts.extend(kw['lorentz'].elts)
            for indices, coupling in zip(kw['couplings'].keys, kw['couplings'].values):
                ci, li = ast.literal_eval(indices)
                first['couplings'].keys.append(ast.Tuple(
                    elts=[ast.Constant(ci), ast.Constant(li+offset)], ctx=ast.Load()))
                first['couplings'].values.append(coupling)
            groups[key] = (old_fields, first_vertex, first, count+1)
            remove.add(id(statement))
        if remove:
            vertex_tree.body = [x for x in vertex_tree.body if id(x) not in remove]
            ast.fix_missing_locations(vertex_tree)
            transformed[vertex_path] = (ast.unparse(vertex_tree)+'\n', vertex_tree)
            merged_groups = [{'fields': list(key), 'original_vertices': value[3]}
                             for key, value in groups.items() if value[3] > 1]
    metadata = {'masses': {}, 'vertices': [], 'couplings': {}, 'lorentz': {}}
    metadata['merged_vertex_groups'] = merged_groups
    for n in calls('particles.py', 'Particle'):
        kw = {k.arg: k.value for k in n.keywords}
        metadata['masses'][ast.literal_eval(kw['name'])] = kw['mass'].attr
    for n in calls('couplings.py', 'Coupling'):
        kw = {k.arg: k.value for k in n.keywords}
        metadata['couplings'][ast.literal_eval(kw['name'])] = ast.literal_eval(kw['value']).replace('complex(0,1)', 'I')
        if request.get('vertex_order_unit', False):
            for keyword in n.keywords:
                if keyword.arg == 'order':
                    keyword.value = ast.parse("{'QED': 1}", mode='eval').body
    for n in calls('lorentz.py', 'Lorentz'):
        kw = {k.arg: k.value for k in n.keywords}
        metadata['lorentz'][ast.literal_eval(kw['name'])] = {
            'spins': ast.literal_eval(kw['spins']),
            'structure': ast.literal_eval(kw['structure'])}
    for n in calls('vertices.py', 'Vertex'):
        kw = {k.arg: k.value for k in n.keywords}
        fields = [x.attr for x in kw['particles'].elts]
        lorentz = [x.attr for x in kw['lorentz'].elts]
        colors = ast.literal_eval(kw['color'])
        terms = []
        for key, value in zip(kw['couplings'].keys, kw['couplings'].values):
            ci, li = ast.literal_eval(key)
            if not (0 <= ci < len(colors) and 0 <= li < len(lorentz)):
                raise ValueError('UFO coupling refers to an absent tensor structure')
            terms.append({'color_index': ci, 'lorentz_index': li,
                          'coupling': value.attr})
        vertex = {'fields': sorted(fields), 'ordered_fields': fields,
                  'lorentz': lorentz, 'color': colors, 'terms': terms}
        if len(terms) == 1:
            vertex['coupling'] = terms[0]['coupling']
        metadata['vertices'].append(vertex)
    if not metadata['masses']:
        raise ValueError('UFO has no declared particles')
    if request.get('vertex_order_unit', False):
        path = directory/'couplings.py'
        _, tree = transformed[path]
        ast.fix_missing_locations(tree)
        transformed[path] = (ast.unparse(tree)+'\n', tree)
        metadata['vertex_order_convention'] = 'one QED unit per local vertex; EFT/CT orders tracked by caller'
    for path, (text, _) in transformed.items():
        backup = originals / path.name
        if not backup.exists():
            shutil.copy2(path, backup)
        current_backup = snapshot / path.name
        if not current_backup.exists():
            shutil.copy2(path, current_backup)
        before = sha(path); path.write_text(text)
        changes.append({'file': path.name, 'before_sha256': before, 'after_sha256': sha(path),
                        'original': artifact(current_backup)})
    target = directory.parent / 'ufo-metadata.json'
    target.write_text(json.dumps(metadata, indent=2)+'\n')
    receipt = directory.parent / 'ufo-normalization.json'
    receipt.write_text(json.dumps({'schema': 'feammatch2.ufo-normalization.v1',
                                   'parser': 'restricted FeynRules scalar UFO AST; no lib2to3',
                                   'changes': changes}, indent=2)+'\n')
    return {'artifacts': [artifact(target), artifact(receipt)],
            'statistics': {'particles': len(metadata['masses']), 'vertices': len(metadata['vertices'])}}


def qgraf_run(request):
    directory = Path(request['directory']); path = directory / 'model.qgraf'
    text = path.read_text(); lines = text.splitlines(keepends=True); vertices = []
    for line in lines:
        if line.lstrip().startswith('[') and ';' in line:
            vertices.extend(w.strip() for w in line.split(';', 1)[0].strip()[1:].split(','))
    used = set(vertices); out = []; removed = []
    for line in lines:
        match = re.fullmatch(r'\s*\[\s*(\w+)\s*,\s*(\w+)\s*,\s*[+-]\s*\]\s*', line)
        if match and not (set(match.groups()) & used):
            removed.append(match.groups())
        else:
            out.append(line)
    before = sha(path)
    if removed:
        shutil.copy2(path, directory / 'model.qgraf.before-prune')
        path.write_text(''.join(out))
    process = subprocess.run([request['executable'], 'qgraf.dat'], cwd=directory, capture_output=True, text=True)
    (directory/'qgraf.stdout.log').write_text(process.stdout)
    (directory/'qgraf.stderr.log').write_text(process.stderr)
    if process.returncode:
        raise RuntimeError('QGRAF failed: '+process.stderr[-2000:])
    output = directory/'qgraf_out.dat'
    if not output.is_file():
        raise ValueError('QGRAF did not produce qgraf_out.dat')
    return {'artifacts': [artifact(path)],
            'statistics': {'output_sha256': sha(output), 'removed_unused_fields': removed, 'before_sha256': before, 'after_sha256': sha(path)}}


def execute(request):
    if not isinstance(request, dict) or request.get('schema') != REQUEST:
        raise ValueError('Unsupported request schema')
    if not isinstance(request.get('task_id'), str) or not request['task_id']:
        raise ValueError('Missing task identity')
    if request.get('action') not in ('normalize-ufo', 'select-graphs', 'qgraf-run', 'capabilities'):
        raise ValueError('Unsupported action')
    inputs = request.get('inputs')
    if not isinstance(inputs, list):
        raise ValueError('Declare input hashes')
    for item in inputs:
        if sha(item['path']) != item['sha256']:
            raise ValueError('Input hash mismatch: '+item['path'])
    if request['action'] == 'capabilities':
        return {'artifacts': [], 'statistics': {'python': sys.version,
                'version_info': list(sys.version_info[:3]), 'stdlib_only': True, 'lib2to3_required': False}}
    return {'normalize-ufo': normalize_ufo, 'select-graphs': select_graphs, 'qgraf-run': qgraf_run}[request['action']](request)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--request', required=True)
    args = parser.parse_args(); request = {}; response = {}
    try:
        if sys.version_info < (3, 9):
            raise RuntimeError('Python >=3.9 required')
        request = json.loads(Path(args.request).read_text())
        response = execute(request)
        response.update(status='ok')
    except Exception as error:
        traceback.print_exc(file=sys.stderr)
        response = {'status': 'failed', 'artifacts': [],
                    'error': {'category': type(error).__name__, 'message': str(error)}}
    response.update(schema=RESPONSE, task_id=request.get('task_id'), action=request.get('action'),
                    request_sha256=sha(args.request), python=sys.version,
                    helper_sources={p.name: sha(p) for p in sorted(Path(__file__).parent.glob('*.py'))})
    text = json.dumps(response, separators=(',', ':'))+'\n'
    response_path = request.get('response_file')
    if response_path:
        Path(response_path).write_text(text)
    print(text, end='')
    return 0 if response['status'] == 'ok' else 2


if __name__ == '__main__':
    raise SystemExit(main())
