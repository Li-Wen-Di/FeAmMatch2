# FeAmMatch2

FeAmMatch2 performs off-shell tree-level, one-loop, and two-loop matching from independent UV and EFT Green-basis Lagrangians. It returns Wilson coefficients and EFT Lagrangians in the original-field Green basis and in a physical basis with canonical kinetic terms. It currently supports real scalar models and retains matching results through dimension six. See the [physical conventions](docs/PHYSICS_CONVENTIONS.md) for model requirements.

## Install and run

Use an ASCII path without spaces. Install Wolfram Kernel, Julia 1.10, Python 3.9+, GNU Fortran, and curl, and make them available on `PATH`. The Julia environment is pinned to 1.10.12. Obtain QGRAF 4.0.5 under the distribution terms on the [author's site](https://www.cefema-gt.tecnico.ulisboa.pt/~paulo/qgraf.html). Place the original `.f08` file in `packages/qgraf/` or set `QGRAF_SRC` to its path.

Run these commands from the repository root.

```bash
bash packages/build.sh
./example/example1/run.sh
./example/example2/run.sh
```

The installation script `packages/build.sh` downloads and verifies an official prebuilt FORM executable, compiles QGRAF locally, extracts the OPITeR tables and verifies each file, and instantiates the pinned Julia environment. The FORM manifest lists official releases for macOS and Linux on ARM64 and x86-64. Both paper examples passed validation on the author's Apple Silicon Mac (macOS ARM64). See the [validation report](docs/VALIDATION.md).

## Paper examples

| Example | Model and conventions | Reference |
|---|---|---|
| [example1](example/example1) | Heavy–light cubic interaction, MSbar tadpoles | Eqs. (1) and (2) of the paper |
| [example2](example/example2) | Heavy-field tadpoles canceled, the heavy-field cubic coupling counted as soft | [arXiv:2311.13630](https://arxiv.org/abs/2311.13630) |

Each example's `input/` contains the UV and EFT `.fr` files, a matching request, and both UFO models. The run script regenerates the UFOs from the `.fr` files and runs the matching calculation. The package includes Green-basis results in `output/green/` and physical-basis results in `output/physical/`, with Wilson coefficients and Lagrangians in `.wl` and `.tex` files. By default, reruns write to a new `output/run-*/` directory. A full two-loop run records the three terms of Eq. (160) for each channel in `loops/two/eq160-contributions.wl`.

## Match your own model

Start a new Wolfram kernel in the repository root.

```wolfram
Get["src/wolfram/FeAmMatch2.wl"];
task = CreateMatchingTask["/absolute/path/UV.fr", "/absolute/path/EFTGreen.fr",
  <|"Loops" -> 2, "MaxDimension" -> 6|>];
plan = PlanMatching[task];
result = RunMatching[task, "/absolute/path/to/new-run", <||>];
```

The supplied Green basis determines the planned off-shell channels. It must cover the relevant operators for the model. See [input format](docs/INPUTS.md), [physical conventions](docs/PHYSICS_CONVENTIONS.md), and the [Chinese implementation appendix](docs/APPENDIX.zh-CN.md).

## Citation and licenses

Cite *Automated diagrammatic two-loop EFT matching from hard-region expansion* by Wen-Di Li, Zhao Li, Quan-feng Wu, Chengjie Yang and Jiang-Hao Yu, and report the FeAmMatch2 release used.

FeAmMatch2's original code is licensed under the [MIT License](LICENSE). Bundled dependencies retain their own licenses and notices. See [NOTICE.md](NOTICE.md).
