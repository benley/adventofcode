workspace(name = "aoc_bazel")

load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive"
)

http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-0.13",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.13.tar.gz"],
    sha256 = "b4e2c00da9bc6668fa0404275fecfdb31beb700abdba0e029e74cacc388d94d6",
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
    "stack_snapshot"
)

stack_snapshot(
    name = "stackage",
    extra_deps = {"zlib": ["@zlib.dev//:zlib"]},
    packages = [
        "zlib",
        "split",
        "fgl",
        "text-format",
        "text-show",
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
    repository = "@nixpkgs",
    attribute_path = "ghc",
    version = "8.10.2",
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
