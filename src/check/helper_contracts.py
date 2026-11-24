"""Independent fixtures for the production helper protocol and graph policies."""
import argparse
import copy
import json
from pathlib import Path
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT/'src/python'))
import entry


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('-o', '--output', type=Path)
    args = parser.parse_args(); checks = {}
    policy = {'schema': 'feammatch2.graph-policy.v1', 'matching_object': '1LPI',
              'light_fields': ['phi'], 'tadpole_free': False, 'zero_boundary_vertices': [],
              'max_zero_vertices': 1, 'compress_zero_momentum': False,
              'zero_external_momenta': True, 'derivative_order': 0}
    graph = {'diagram_index': 1, 'sign': 1, 'symmetry_factor': '1/3',
             'vertices': [{'vertex_index': 1, 'fields': ['H','phi','phi']},
                          {'vertex_index': 2, 'fields': ['H','phi','phi']}],
             'incoming_propagators': [{'vertex_index': 1, 'field': 'phi'}],
             'outgoing_propagators': [{'vertex_index': 2, 'field': 'phi'}],
             'remnant_propagators': [{'birth_index': 1, 'death_index': 2, 'field': 'H'}]}
    def rejected(call):
        try:
            call()
            return False
        except (ValueError, KeyError, SyntaxError):
            return True
    checks['heavy_bridge_retained'] = entry.graph_reason(graph, policy) == 'retained_1LPI'
    one_pi = dict(policy, matching_object='1PI')
    entry.validate_policy(one_pi)
    checks['one_pi_excludes_heavy_bridge'] = entry.graph_reason(graph, one_pi) == 'excluded_propagator_bridge'
    light = copy.deepcopy(graph); light['remnant_propagators'][0]['field'] = 'phi'
    checks['light_bridge_excluded'] = entry.graph_reason(light, policy) == 'excluded_light_bridge'
    checks['bookkeeping_source_never_propagates'] = entry.graph_reason(
        light, dict(policy, external_only_fields=['phi'])) == 'excluded_auxiliary_internal_line'
    tadpole = copy.deepcopy(graph); tadpole['outgoing_propagators'][0]['vertex_index'] = 1
    checks['MSbar_tadpole_retained'] = entry.graph_reason(tadpole, policy) == 'retained_1LPI'
    checks['tadpole_free_bridge_excluded'] = entry.graph_reason(tadpole, dict(policy, tadpole_free=True)) == 'excluded_heavy_tadpole_bridge'
    auxiliary = copy.deepcopy(graph)
    auxiliary['outgoing_propagators'][0]['field'] = 'source'
    checks['auxiliary_source_does_not_hide_tadpole'] = entry.graph_reason(
        auxiliary, dict(policy, tadpole_free=True, auxiliary_external_fields=['source'])) == 'excluded_heavy_tadpole_bridge'
    auxiliary['outgoing_propagators'][0]['vertex_index'] = 3
    checks['auxiliary_external_references_still_validated'] = rejected(lambda: entry.graph_reason(
        auxiliary, dict(policy, auxiliary_external_fields=['source'])))
    checks['boundary_vertex_order'] = entry.graph_reason(graph, dict(policy, zero_boundary_vertices=[['H','phi','phi']])) == 'excluded_boundary_vertex_order'
    bad = copy.deepcopy(graph); bad['remnant_propagators'][0]['death_index'] = 3
    checks['bad_vertex_reference_rejected'] = rejected(lambda: entry.graph_reason(bad, policy))
    bad = copy.deepcopy(graph); bad['remnant_propagators'] = []
    checks['disconnected_graph_rejected'] = rejected(lambda: entry.graph_reason(bad, policy))
    checks['derivative_compression_rejected'] = rejected(lambda: entry.validate_policy(dict(policy, compress_zero_momentum=True, derivative_order=2)))
    checks['nonzero_external_compression_rejected'] = rejected(lambda: entry.validate_policy(dict(policy, compress_zero_momentum=True, zero_external_momenta=False)))
    checks['incomplete_policy_rejected'] = rejected(lambda: entry.validate_policy({'schema':policy['schema']}))
    second = copy.deepcopy(graph); second['diagram_index'] = 2
    reps, report = entry.compress([graph, second])
    checks['exact_rational_weights'] = len(reps) == 1 and reps[0]['symmetry_factor'] == '2/3'
    second['sign'] = -1
    reps, report = entry.compress([graph, second])
    checks['cancelled_groups_have_witness'] = not reps and report['groups'][0]['summed_symmetry_factor'] == '0' and len(report['groups'][0]['members']) == 2
    with tempfile.TemporaryDirectory(prefix='fm2-contract-') as temporary:
        directory = Path(temporary)
        def run(request):
            request = dict(request, response_file=str(directory/'response.json'))
            path = directory/'request.json'; path.write_text(json.dumps(request))
            process = subprocess.run([sys.executable, str(ROOT/'src/python/entry.py'), '--request', str(path)],
                                     capture_output=True, text=True)
            return process, json.loads((directory/'response.json').read_text())
        base = {'schema':entry.REQUEST, 'action':'capabilities', 'task_id':'fixture', 'inputs':[]}
        process, response = run(base)
        checks['protocol_roundtrip'] = process.returncode == 0 and response['task_id'] == 'fixture' and response['request_sha256'] == entry.sha(directory/'request.json')
        checks['stdlib_capabilities'] = response['statistics']['stdlib_only'] and not response['statistics']['lib2to3_required']
        for name, replacement in [('wrong_schema',{'schema':'bad'}), ('unknown_action',{'action':'bad'}), ('missing_identity',{'task_id':''})]:
            process, response = run(dict(base, **replacement))
            checks[name+'_rejected'] = process.returncode != 0 and response['status'] == 'failed' and not response['artifacts']
        source = directory/'source'; source.write_text('actual')
        process, response = run(dict(base, inputs=[{'path':str(source), 'sha256':'incorrect'}]))
        checks['input_hash_mismatch_rejected'] = process.returncode != 0 and response['status'] == 'failed'
        ufo = directory/'UFO'; ufo.mkdir()
        (ufo/'particles.py').write_text("P=Particle(name='phi',mass=Param.m)\n")
        (ufo/'couplings.py').write_text("C=Coupling(name='GC',value='complex(0,1)*g')\n")
        (ufo/'lorentz.py').write_text("L=Lorentz(name='SS',spins=[1,1],structure='1')\n")
        (ufo/'vertices.py').write_text("V=Vertex(particles=[P.phi,P.phi],lorentz=[L.SS],color=['1'],couplings={(0,0):C.GC})\n")
        (ufo/'object_library.py').write_text('if False:\n    raise UFOError, "unsupported"\n')
        request = dict(base, action='normalize-ufo', directory=str(ufo),
                       inputs=[{'path':str(p), 'sha256':entry.sha(p)} for p in ufo.glob('*.py')])
        process, response = run(request)
        metadata = json.loads((directory/'ufo-metadata.json').read_text())
        checks['scalar_UFO_AST_and_legacy_raise'] = process.returncode == 0 and metadata['couplings']['GC'] == 'I*g'
        checks['UFO_originals_preserved'] = (directory/'UFO-original/object_library.py').read_text().find('raise UFOError,') >= 0
        (ufo/'couplings.py').write_text("C=Coupling(name='GC',value='complex(0,1)*g',order={'QED':1})\nC2=Coupling(name='GC2',value='complex(0,1)*r',order={'QED':2})\n")
        (ufo/'vertices.py').write_text("V=Vertex(particles=[P.phi,P.phi],lorentz=[L.SS],color=['1'],couplings={(0,0):C.GC})\nV2=Vertex(particles=[P.phi,P.phi],lorentz=[L.SS],color=['1'],couplings={(0,0):C.GC2})\n")
        request.update(vertex_order_unit=True, merge_scalar_vertices=True)
        request['inputs'] = [{'path':str(p), 'sha256':entry.sha(p)} for p in ufo.glob('*.py')]
        process, response = run(request)
        metadata = json.loads((directory/'ufo-metadata.json').read_text())
        checks['repeated_UFO_channels_merged_losslessly'] = (process.returncode == 0
            and len(metadata['vertices']) == 1 and len(metadata['vertices'][0]['terms']) == 2
            and metadata['merged_vertex_groups'][0]['original_vertices'] == 2)
        receipt = json.loads((directory/'ufo-normalization.json').read_text())
        checks['current_UFO_preimages_preserved'] = all(
            row['before_sha256'] == row['original']['sha256'] for row in receipt['changes'])
        (ufo/'unsupported.py').write_text('print "legacy syntax outside supported template"\n')
        request['inputs'] = [{'path':str(p), 'sha256':entry.sha(p)} for p in ufo.glob('*.py')]
        process, response = run(request)
        checks['unsupported_UFO_syntax_rejected'] = process.returncode != 0 and response['status'] == 'failed'
    result = {'passed': all(checks.values()), 'checks':checks}
    if args.output:
        args.output.write_text(json.dumps(result, indent=2)+'\n')
    print(json.dumps(result, indent=2))
    return 0 if result['passed'] else 1


if __name__ == '__main__':
    raise SystemExit(main())
