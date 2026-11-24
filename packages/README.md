# Dependency installation

`bash packages/build.sh` installs FORM and QGRAF locally under `packages/bin/`, restores the OPITeR tables to `packages/opiter/opiter/`, and instantiates `Project.toml` with `Manifest.toml`.

- FORM is pinned to version 5.0.2. The download origin and SHA-256 for each supported platform are recorded in `assets-manifest.json`. An already downloaded official archive can be used via `FORM_ARCHIVE`; the same SHA-256 check applies.
- QGRAF is pinned to the original 4.0.5 source bytes. See [QGRAF installation](qgraf/README.md). `FC` can point to a GNU Fortran executable.
- FeynRules uses the source from [upstream commit b3357d6](https://github.com/FeynRules/FeynRules/commit/b3357d64ac3dfa3de97d7536dcff5ba92d04f086). The Mathematica package reports version 2.3.49.
- [FeAmGen.jl](https://code.ihep.ac.cn/IHEP-Multiloop/FeAmGen.jl) is version 1.1.5, upstream tree `7fa9a9075428f27174b90a2d81f1b91026519b62`. Its Julia dependencies are resolved by the versioned manifest. Julia 1.10.12 generated the manifest. The installer registers the IHEP-Multiloop Julia registry when it is missing.
- [ABC4EFT](https://github.com/XiaoMing-lei/ABC4EFT/tree/848a1bec81c3459eee67055eb143108bce1f776c) is distributed under CC0 1.0 Universal. This copy omits the tutorials and SnMat data for the scalar basis workflow.
- OPITeR is pinned by revision and file hashes in the asset manifest. Its tarball contains the 19 original projector and inverse Gram table files.
- The original sunset FORM header is included in [sunset/](sunset/). For an alternate header, set `SunsetHeader` in the [local JSON configuration](../config/toolchain-wolfram.example.json) or use `FEAMMATCH2_SUNSET`.

`FEAMMATCH2_WOLFRAM`, `FEAMMATCH2_JULIA` and `FEAMMATCH2_PYTHON` can select runtime executables. Use absolute paths for tools outside PATH. `FM2_TOOLCHAIN` selects an alternate JSON configuration for the example scripts.
