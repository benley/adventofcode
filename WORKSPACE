workspace(name = "aoc_bazel")

load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

http_archive(
    name = "rules_haskell",
    sha256 = "aba3c16015a2363b16e2f867bdc5c792fa71c68cb97d8fe95fddc41e409d6ba8",
    strip_prefix = "rules_haskell-0.15",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.15.tar.gz"],
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)
load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot",
)

stack_snapshot(
    name = "stackage",
    extra_deps = {"zlib": ["@zlib.dev//:zlib"]},
    packages = [
        "fgl",
        "split",
        "text-format",
        "text-show",
        "zlib",
    ],
    snapshot = "nightly-2020-12-14",
    # This uses an unpinned version of stack_snapshot, meaning that
    # stack is invoked on every build.  To switch to pinned stackage
    # dependencies, run `bazel run @stackage-unpinned//:pin` and
    # uncomment the following line.
    stack_snapshot_json = "//:stackage_snapshot.json",
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_git_repository",
    "nixpkgs_local_repository",
    "nixpkgs_package",
    "nixpkgs_python_configure",
)

nixpkgs_python_configure(
    repository = "@nixpkgs",
)

nixpkgs_git_repository(
    name = "nixpkgs",
    revision = "nixpkgs-unstable",
    # sha256 = …
)

load(
    "@rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)

# Fetch a GHC binary distribution from nixpkgs and register it as a toolchain.
# For more information:
# https://api.haskell.build/haskell/nixpkgs.html#haskell_register_ghc_nixpkgs
haskell_register_ghc_nixpkgs(
    attribute_path = "ghc",
    repository = "@nixpkgs",
    version = "9.0.2",
)

# For zlib.BUILD.bazel
nixpkgs_package(
    name = "nixpkgs_zlib",
    attribute_path = "zlib",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "zlib.dev",
    build_file = "//:zlib.BUILD.bazel",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "haskell-language-server",
    attribute_path = "haskell-language-server",
    repository = "@nixpkgs",
)
