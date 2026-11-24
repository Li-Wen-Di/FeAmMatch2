"""Audit or archive the public FeAmMatch2 tree using only the standard library."""
import argparse
import ast
import gzip
import hashlib
import io
import json
from pathlib import Path, PurePosixPath
import re
import tarfile

ROOT = Path(__file__).resolve().parent.parent
PUBLIC_ROOTS = {"README.md", "LICENSE", "NOTICE.md",
                "PacletInfo.wl", ".gitignore", "src", "packages", "example",
                "config", "docs", "licenses", "tools"}


def sha(data):
    return hashlib.sha256(data).hexdigest()


def paths(root):
    for path in sorted(root.rglob("*")):
        rel = path.relative_to(root)
        if not path.is_file() or rel.parts[0] not in PUBLIC_ROOTS:
            continue
        if any(p in {".git", "__pycache__", ".DS_Store"} or p.startswith(".build-") for p in rel.parts):
            continue
        if path.name.endswith((".local.json", ".pyc")) or any(p.startswith("run-") for p in rel.parts[1:-1]):
            continue
        if rel.parts[:2] == ("packages", "bin") or (
                rel.parts[:2] == ("packages", "qgraf") and path.name != "README.md"):
            continue
        if rel.parts[:4] in {("packages", "opiter", "opiter", "projectors"),
                             ("packages", "opiter", "opiter", "grammatrix")}:
            continue
        if path.is_symlink():
            raise ValueError(f"Unexpected symbolic link: {rel}")
        yield rel


def release_hashes(root):
    return {str(p): sha((root / p).read_bytes()) for p in paths(root)
            if not str(p).startswith("docs/acceptance")}


def audit(root):
    loader = (root / "src/wolfram/FeAmMatch2.wl").read_text()
    modules = {p.name for p in (root / "src/wolfram").glob("*.wl")} - {"FeAmMatch2.wl"}
    listed = set(re.findall(r'Get\[FileNameJoin\[\{DirectoryName\[\$packageFile\],"(\w+\.wl)"', loader))
    checks = {"wolfram_loader": listed == modules, "single_backend": "MatchingBackend" not in loader,
              "no_retired_directories": not any((root / p).exists() for p in ("internal", "vendor", "wolfram", "tutorials", "validation", "results")),
              "two_examples": {p.name for p in (root / "example").iterdir()} == {"example1", "example2"}}
    for example in ("example1", "example2"):
        base = root / "example" / example
        required = ["run.sh", "input/UV.fr", "input/EFTGreen.fr", "input/request.json",
                    "input/ufo/UV/vertices.py", "input/ufo/EFT/vertices.py",
                    "output/green/coefficients.wl", "output/physical/coefficients.wl"]
        checks[example] = all((base / p).is_file() for p in required) and bool((base / "run.sh").stat().st_mode & 0o111)
    for name in ("src/python", "src/check", "tools"):
        for path in (root / name).glob("*.py"):
            ast.parse(path.read_text(), filename=str(path))
    opiter = root / "packages/opiter"
    assets = json.loads((root / "packages/assets-manifest.json").read_text())
    spec = assets["opiter"]
    header = root / "packages/sunset/proc.h"
    checks["sunset_header"] = header.is_file() and sha(header.read_bytes()) == assets["sunset"]["sha256"]
    packed = opiter / "opiter-tables.tar.gz"
    checks["opiter_archive"] = sha(packed.read_bytes()) == spec["archive_sha256"]
    with tarfile.open(packed) as archive:
        packed_hashes = {f"opiter/{m.name}": sha(archive.extractfile(m).read())
                         for m in archive if m.isfile()}
    checks["opiter_files"] = len(packed_hashes) == 19 and all(
        (packed_hashes[name] if name in packed_hashes else sha((opiter / name).read_bytes())) == digest
        for name, digest in spec["files"].items())
    return {"passed": all(checks.values()), "checks": checks}


def build(root, output):
    result = audit(root)
    if not result["passed"]:
        raise ValueError(f"Failed release audit: {result['checks']}")
    report = json.loads((root / "docs/acceptance.json").read_text())
    if report.get("passed") is not True or report.get("release_files") != release_hashes(root):
        raise ValueError("Acceptance evidence does not match the public file inventory")
    output.parent.mkdir(parents=True, exist_ok=True)
    with output.open("xb") as stream, gzip.GzipFile(fileobj=stream, mode="wb", filename="", mtime=0) as compressed:
        with tarfile.open(fileobj=compressed, mode="w") as archive:
            for rel in paths(root):
                data = (root / rel).read_bytes()
                member = tarfile.TarInfo(str(PurePosixPath("FeAmMatch2") / rel.as_posix()))
                member.size = len(data)
                member.mode = 0o755 if (root / rel).stat().st_mode & 0o111 else 0o644
                archive.addfile(member, io.BytesIO(data))
    return {"archive": str(output), "sha256": sha(output.read_bytes()), "bytes": output.stat().st_size}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("audit", "build"))
    parser.add_argument("--archive", type=Path)
    args = parser.parse_args()
    if args.action == "build" and not args.archive:
        parser.error("build requires --archive")
    result = audit(ROOT) if args.action == "audit" else build(ROOT, args.archive)
    print(json.dumps(result, indent=2))
    return 0 if result.get("passed", True) else 1


if __name__ == "__main__":
    raise SystemExit(main())
