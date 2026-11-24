# Third-party notices

FeAmMatch2's original project code is licensed under the MIT License in `LICENSE`. The bundled components listed below retain their respective licenses or distribution terms.

- FeynRules is distributed under its upstream GPLv3 license. See `packages/FeynRules/LICENSE`.
- FeAmGen.jl is MIT-licensed. Its license is included in `packages/FeAmGen.jl/LICENSE`. The process-local scalar overrides in `src/julia/generate_offshell.jl` adapt neutral self-loop ray momenta and exact zero-external-momentum substitutions. See `licenses/FeAmGen-MIT.txt`.
- ABC4EFT is supplied under CC0-1.0. This real-scalar distribution omits the upstream tutorials and SnMat data. See `packages/ABC4EFT/LICENSE`.
- OPITeR is distributed under GPL-3.0-or-later, copyright Jae Goode, Franz Herzog and Sam Teale. The archive preserves the original integral tables. The supplied settings use MaxN=20 and MaxE=0. Upstream settings, license and hashes are included.
- FORM is downloaded directly from its [official release](https://github.com/form-dev/form/releases/tag/v5.0.2). QGRAF source is obtained from its [author](https://www.cefema-gt.tecnico.ulisboa.pt/~paulo/qgraf.html). The release contains the tool installers and interfaces for these dependencies.
- The original author supplied `packages/sunset/proc.h` and authorized its inclusion in the public FeAmMatch2 distribution. The file is included unchanged. Its SHA-256 is recorded in `packages/assets-manifest.json`.
