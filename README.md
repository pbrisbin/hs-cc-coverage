# Haskell Code Climate Coverage

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

## Multiple Projects

If you are operating in the context of a "monorepo" (multiple packages present
in one repository), you will want to use the `-C`/`--directory` option. Exact
usage depends on if your repository is also a single Code Climate repository or
if you've added each package independently on Code Climate. (The latter is an
uncommon but useful workaround for Code Climate's lack of first-class
sub-directory analysis.)

### One Code Climate Repository

In this case, you can generate each package's payload then combine and upload
them together:

```sh
stack test --coverage

tix2hs --directory ./project-a > coverage/project-a.json

tix2hs --directory ./project-b > coverage/project-b.json

cc-test-reporter sum-coverage --parts 2 --output - coverage/*.json |\
  cc-test-reporter upload-coverage --id "REPORTER_ID" --input -
```

### Separate Code Climate Repositories

In this case, you need to generate and upload each payload separately:

```sh
stack test --coverage

tix2hs --directory ./project-a |\
  cc-test-reporter upload-coverage --id "PROJECT_A_REPORTER_ID" --input -

tix2hs --directory ./project-b |\
  cc-test-reporter upload-coverage --id "PROJECT_B_REPORTER_ID" --input -
```

## Non-Stack Usage

`tix2cc` uses `stack path` to automatically locate the directories where `hpc`
will place its Mix and Tix files.

If you're not using stack, you must either:

- Ensure `hpc` places its files in `dist/hpc/{mix,tix}`, or
- Pass `-m`/`--mix-dir` and `-t`/`--tix-dir`

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
