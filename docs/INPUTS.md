# Model inputs and outputs

The UV and EFT Green models are independent files. The examples in [example1/input](../example/example1/input) and [example2/input](../example/example2/input) show the complete `.fr` input format.

- Both files define `M$ClassesDescription`, `M$Parameters`, and `Ltot`. Scalar classes are real and use `FullName -> "light"` or `FullName -> "heavy"`. The free UV action is canonical and diagonal, and the reference vacuum is the stationary origin.
- The EFT file lists independent coefficients in `FM2$WilsonCoefficients`. Their operator normalization follows the supplied `Ltot`. The input Green basis supplies the channels and independent momentum structures.
- Request fields include `Loops` (0, 1, 2), `MaxDimension` (4, 5, 6), `LightFieldMap`, `ParameterWeights`, `Tadpole` and `PhysicalBasis`. The default physical basis provider is ABC4EFT, and an explicit EFT physical action can also be supplied. To use different UV and EFT field names, supply `LightFieldMap`.
- Local tool settings can be passed as an Association to `RunMatching` or read from the JSON configuration. Explicit paths are allowed for `WolframExecutable`, `PythonExecutable`, `JuliaLauncher`, `FeynRulesDirectory`, `ABC4EFTDirectory`, `QGRAFExecutable`, `FORMExecutable`, `OPITeRDirectory`, and `SunsetHeader`.

The result exposes `GreenRules`, `GreenLagrangian`, and `Physical` with its own `Rules` and `Lagrangian`. Full runs write `run-status.json` and `runtime-manifest.json` as well as the two basis directories. See [physical conventions](PHYSICS_CONVENTIONS.md) and [the implementation appendix](APPENDIX.zh-CN.md).
