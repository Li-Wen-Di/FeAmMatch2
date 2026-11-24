"""Install the release's command-line tools without changing system packages."""
import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import platform
import shutil
import subprocess
import tarfile
import tempfile

ROOT = Path(__file__).resolve().parent


def sha(path):
    with path.open("rb") as stream:
        digest = hashlib.sha256()
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def verify(path, expected):
    if not path.is_file() or sha(path) != expected:
        raise ValueError(f"SHA-256 mismatch or missing file: {path}")


def unpack_file(archive, member, output):
    if not member.isfile():
        raise ValueError(f"Expected a regular archive member: {member.name}")
    output.parent.mkdir(parents=True, exist_ok=True)
    with archive.extractfile(member) as source, output.open("wb") as destination:
        shutil.copyfileobj(source, destination)


def install_form(work, spec):
    system = {"Darwin": "macos", "Linux": "linux"}.get(platform.system())
    arch = {"aarch64": "arm64", "arm64": "arm64", "x86_64": "x86_64"}.get(platform.machine())
    key = f"{system}-{arch}"
    if key not in spec["archives"]:
        raise ValueError(f"No pinned official FORM archive for {platform.system()} {platform.machine()}")
    version = spec["version"]
    upstream_system = "osx" if system == "macos" else system
    filename = f"form-{version}-{arch}-{upstream_system}.tar.gz"
    url = spec["release"].replace("/releases/tag/", "/releases/download/") + "/" + filename
    local = os.environ.get("FORM_ARCHIVE")
    source = Path(local) if local else work / filename
    if not local:
        subprocess.run(["curl", "-fLsS", "--retry", "2", "--max-time", "180",
                        url + "?download=1", "-o", str(source)], check=True)
    verify(source, spec["archives"][key])
    output = work / "form"
    with tarfile.open(source) as archive:
        members = [m for m in archive if m.isfile() and PurePosixPath(m.name).name == "form"]
        if len(members) != 1:
            raise ValueError("Expected exactly one FORM executable in the official archive")
        unpack_file(archive, members[0], output)
    output.chmod(0o755)
    version_output = subprocess.check_output([str(output), "-v"], text=True)
    if version not in version_output:
        raise ValueError(f"FORM binary did not report the pinned version {version}")
    print(version_output.strip(), flush=True)
    return output


def prepare_opiter(work, spec):
    opiter = ROOT / "opiter"
    tables = opiter / "opiter-tables.tar.gz"
    verify(tables, spec["archive_sha256"])
    folder_names = {"projectors", "grammatrix"}
    relative = {str(PurePosixPath(name).relative_to("opiter")): digest
                for name, digest in spec["files"].items()
                if name.startswith(("opiter/projectors/", "opiter/grammatrix/"))}
    with tarfile.open(tables) as archive:
        members = archive.getmembers()
        files = [m for m in members if m.isfile()]
        if sorted(m.name for m in files) != sorted(relative) or any(
                not m.isfile() and not (m.isdir() and m.name in folder_names) for m in members):
            raise ValueError("OPITeR table archive contents do not match the asset manifest")
        for member in files:
            path = work / "opiter" / member.name
            unpack_file(archive, member, path)
            verify(path, relative[member.name])
    destination = opiter / "opiter"
    for path, digest in spec["files"].items():
        if not path.startswith(("opiter/projectors/", "opiter/grammatrix/")):
            verify(opiter / path, digest)
    for name in relative:
        output = destination / name
        output.parent.mkdir(parents=True, exist_ok=True)
        (work / "opiter" / name).replace(output)
    print(f"Verified {len(relative)} OPITeR tables", flush=True)


def build_qgraf(work, spec):
    version = spec["version"]
    source = Path(os.environ.get("QGRAF_SRC", ROOT / "qgraf" / f"qgraf-{version}.f08")).resolve()
    if not source.is_file():
        raise ValueError(f"Obtain official QGRAF {version} source and place it at {source}, or set QGRAF_SRC")
    verify(source, spec["source_sha256"])
    output = work / "qgraf"
    subprocess.run([os.environ.get("FC", "gfortran"), "-cpp", "-P", "-Os", "-J", ".",
                    "-o", str(output), str(source)], cwd=work, check=True)
    return output


def main():
    manifest = json.loads((ROOT / "assets-manifest.json").read_text())
    with tempfile.TemporaryDirectory(prefix=".build-", dir=ROOT) as temp:
        work = Path(temp)
        qgraf = build_qgraf(work, manifest["qgraf"])
        form = install_form(work, manifest["form"])
        prepare_opiter(work, manifest["opiter"])
        subprocess.run([str(ROOT / "run-julia.sh"), str(ROOT / "install-julia.jl")],
                       cwd=ROOT, check=True)
        output = ROOT / "bin"
        output.mkdir(exist_ok=True)
        for executable in (form, qgraf):
            executable.replace(output / executable.name)
        print(f"Installed FORM and QGRAF in {output}", flush=True)


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        raise SystemExit(f"build failed: {error}") from error
