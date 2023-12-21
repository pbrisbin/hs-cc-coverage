# Haskell Code Climate Coverage

**Archived**: this project is no longer maintained. Please use hpc-lcov or
hpc-codecov to [re-]format Haskell coverage for ingestion into tools like
Code Climate.

Generate upload-able [Code Climate][] coverage payloads from [Hpc][] traces.

[code climate]: https://codeclimate.com/
[hpc]: https://wiki.haskell.org/Haskell_program_coverage

## Required Reading

This project is a minor player in, and extension to, the process of submitting
coverage information to Code Climate. You are encouraged to understand the
[overall mechanics][overview] of the feature and are expected to be familiar
with the [`cc-test-reporter`][cc-test-reporter] executable, which handles most
of the other steps.

[overview]: https://docs.codeclimate.com/docs/configuring-test-coverage
[cc-test-reporter]: https://github.com/codeclimate/test-reporter#readme

## Installation

**TODO**: this tool is currently unreleased. If you *really* want to use it now,
do the usual git-clone-stack-install process.

## Basic Usage

For a single-directory, Stack- and Git-using project:

```console
stack test --coverage

tix2cc | cc-test-reporter upload-coverage --id "REPORTER_ID" --input -
```

## Options

```
Usage: tix2cc [-m|--mix-dir DIR] [-t|--tix-dir DIR] [-p|--prefix PATH] [PATTERN]
  Create Code Climate coverage files from Hpc traces

Available options:
  -h,--help                Show this help text
  -m,--mix-dir DIR         Directory for .mix files, inferred for Stack or
                           dist/hpc/mix
  -t,--tix-dir DIR         Directory for .tix files, inferred for Stack or
                           dist/hpc/tix
  -p,--prefix PATH         Path prefix to apply, in the case of sub-directory
                           projects
  PATTERN                  Pattern used to locate .tix files, default is
                           **/*.tix
```

## Multiple Test Suites

Hpc (at least with Stack and using the versions I've tested) names its Tix files
as `.../<package>/<suite>/<suite>.tix`. So if you only want to process some of
your project's test suites, you can limit the Tix files processed by passing a
`PATTERN` argument. For example, if you have two test suites, `hspec` and
`doctests`, you could limit coverage uploads to the `hspec` suite with:

```console
tix2cc '**/hspec.tix' | ...
```

**NOTE**: Be sure to quote the `PATTERN` argument, so your shell doesn't try to
expand it itself.

## Non-Stack Usage

`tix2cc` uses `stack path` to automatically locate the directories where `hpc`
will place its Mix and Tix files.

If you're not using stack, you must either:

- Ensure `hpc` places its files in `dist/hpc/{mix,tix}`, or
- Pass `-m`/`--mix-dir` and `-t`/`--tix-dir`

**NOTE**: multi-package projects will not work without Stack, specifically a
version with `stack query`. We need this to find local-dependency working
directories, which is where their `.mix` files reside. I've not yet figured out
how to support this in a non-Stack context. If you know how to do so, please
open an Issue.

## Non-Git Usage

`tix2cc` executes `git` to determine values required by the Code Climate format:

- Current branch
- Current commit and timestamp
- The blob id for each file

This means `git` must be on `$PATH` and you must invoke `tix2cc` within the Git
working directory. Currently, we don't support scenarios that can't provide
that.

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
