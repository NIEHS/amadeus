# Amadeus Download Tier

Apply `agents/Amadeus_AI_Coding_Protocol.md` alongside this guide. 
This file defines only download-specific responsibilities and checks.

## Scope and entry points

The download tier selects and retrieves provider data, performs supported 
archive extraction, and creates the file layout expected by the process tier.

Inspect the affected path:

`download_data()` → `download_<source>()` → download helpers → files on disk.

Primary references:

- `R/download.R`: dispatcher and source-specific download functions.
- `R/download_auxiliary.R`: transfer, authentication, URL validation,
  destination validation, and hashing helpers.
- `R/process.R` and `R/process_auxiliary.R`: downstream file expectations.
- `tests/testthat/helper-mocks-download.R`: download mock factories.
- `tests/testthat/test-download-dispatch.R`: routing tests.
- Dataset-specific offline and gated live tests.

Locate definitions rather than assuming every source uses the same helpers.

## Download contract

Before changing a route, identify:

- Dataset names and aliases accepted by `dataset_name`.
- Provider, product, version, and available spatial and temporal coverage.
- Selection arguments: dates, years, variables, tiles, extents, or granules.
- Required authentication and supported credential sources.
- Destination paths, filenames, formats, and archive structure.
- Existing-file, overwrite, extraction, and cleanup behavior.
- Rate limiting, retries, and partial-failure behavior.
- Return values with `hash = FALSE` and `hash = TRUE`.

Check the corresponding process function before changing file selection or
layout. A successful transfer is insufficient if processing cannot discover
or read the resulting files.

## Dispatch and acknowledgement

- Preserve alias-specific product selection as well as function routing.
- Verify forwarding of `directory_to_save`, `acknowledgement`, `hash`,
  `rate_limit`, credentials, and source-specific arguments.
- Forward credentials only to routes that require them.
- Require `acknowledgement = TRUE` in each source-specific download function
  before directory creation, credential prompts, requests, or other side effects.
- Test acknowledgement directly at the source function; wrapper coverage alone
  does not protect direct calls.

## Provider selection and requests

- Verify authoritative provider information when changing endpoints,
  authentication, query parameters, or product versions.
- Distinguish unavailable data from request or authentication failures.
- Do not substitute another product, release, variable, or date when the
  requested selection is unavailable.
- Preserve date-boundary semantics and spatial-selection rules.
- Handle pagination where required and avoid duplicate granule selection.
- Keep the association between each selected URL and destination file explicit.
- Check request construction independently from transfer success.

Inspect `download_run_method()`, `download_run()`, `check_url_status()`, and
`check_urls()` before creating another transfer path.

For transfer changes:

- Respect `rate_limit` and existing timeout and retry behavior.
- Bound retries and distinguish transient failures from invalid requests.
- Account for provider retry guidance where supported.
- Keep TLS verification enabled.
- Do not assume a failed metadata or HEAD request proves a GET will fail.
- Do not treat HTTP success as proof of valid data: providers may return login
  pages, error documents, or empty responses.
- Use format-appropriate validation without reading large files entirely into
  memory or imposing unsupported provider assumptions.

## Authentication

Inspect `get_token()` and `setup_nasa_token()` before changing EarthData access.

- Preserve supported token-string, token-file, and environment-variable behavior,
  including credential precedence.
- Preserve noninteractive operation for scripts and CI.
- Ensure errors and diagnostics redact tokens, authorization headers, and
  sensitive URL parameters.
- Keep credentials out of generated commands and persistent artifacts.
- Verify that redirects do not forward credentials to unintended hosts.
- Use dummy credentials in offline tests.
- Restore temporary authentication-related environment or session changes.

## Files, archives, and restart behavior

- Preserve the paths, names, formats, and nesting consumed by the process tier.
- Inspect `check_destfile()` before changing existing-file handling.
- Distinguish complete files from incomplete transfers.
- Ensure a failed transfer cannot leave a file that a later call treats as a
  completed download.
- Where compatible with the existing implementation, write to a temporary path
  and promote it after validation.
- Preserve valid existing files when replacement downloads fail.
- Keep successful files recoverable when another transfer fails.

For archive handling:

- Preserve required members and relative paths.
- Prevent extraction outside the intended destination, including traversal
  through archive paths or links.
- Verify extraction before deleting the source archive.
- Respect archive-retention and cleanup options.
- Limit cleanup to files managed by the operation.

## Disabled transfers and command files

For routes supporting `download = FALSE` or generated command files:

- Establish exactly which operations the option disables.
- Do not describe the mode as offline if discovery or URL checks still use
  the network.
- Test that the transfer executor is not invoked when downloads are disabled.
- Verify generated URLs, destination paths, and shell quoting where applicable.
- Preserve command-file retention and removal behavior.
- Do not write credentials into command files.

## Hashes and completion status

Inspect `download_hash()` and the source-specific return path.

- Preserve return behavior for both values of `hash`.
- Verify which files are hashed and whether hashing occurs before or after
  extraction and cleanup.
- Preserve ordering and file-selection rules where they affect the hash.
- Do not confuse reproducibility hashing with provider checksum verification.
- Do not use successful hashing as evidence that every requested transfer
  succeeded.
- Preserve distinctions between successful, skipped, and failed downloads.
- Keep the original transfer failure visible when subsequent cleanup also fails.

## Download-specific test design

Use the factories in `helper-mocks-download.R` where appropriate:

- `mocks_download_stack()` / `local_download_mocks()`
- `mocks_token_stack()` / `local_token_mocks()`

Inspect their defaults. Mocked transfer counts and hashes verify orchestration,
not real transfer or hashing correctness.

Choose the boundary deliberately:

| Test target | Exercise | Mock or fixture |
| --- | --- | --- |
| Dispatcher | Routing and argument forwarding | Source function |
| Source downloader | Selection, paths, options, orchestration | Transfer boundary |
| Transfer helper | Response handling and retry decisions | Controlled HTTP responses |
| Archive handling | Extraction, layout, cleanup | Small local archive |
| Hashing | File scope and return behavior | Small local files |
| Process handoff | Discovery and format compatibility | Representative downloaded layout |

Select applicable cases:

- Acknowledgement rejected before any side effect.
- Alias selects the correct source and product.
- Dates, variables, tiles, and pagination produce the expected file list.
- Credentials resolve correctly without interactive prompts.
- Existing, missing, and incomplete destination files follow the contract.
- Authentication failure, missing data, transient errors, and invalid content
  are distinguished.
- Disabled downloads do not invoke transfers.
- Partial failures preserve completed files and report failed transfers.
- Extraction and cleanup retain the expected process-tier layout.
- Hash behavior uses the intended files.

Assert actual URLs, forwarded arguments, file contents, or layout as appropriate.
Directory existence alone does not demonstrate a correct download.

## Live verification

When provider interaction needs verification:

- Use the smallest representative request.
- Check content and resulting file layout, not just status codes.
- Use the existing live-test gate and credential handling.
- Distinguish provider unavailability from local implementation failures.
- State whether the result verifies discovery, authentication, transfer,
  extraction, or the full download-to-process handoff.

Passing offline mocks does not establish current provider compatibility.

## Completion evidence

In addition to the protocol's standard handoff, identify:

- The provider/product and selection behavior affected.
- Whether the process-tier file contract remains compatible.
- Which download stages were verified offline and which were verified live.
- Any unresolved provider, credential, or partial-download limitation.

## Task-specific outputs

Apply the relevant row within the scope requested by the user.

| Task | Required output |
| --- | --- |
| Diagnose a failure | Reproduction or supporting evidence, failing stage, root cause or remaining hypotheses, and recommended fix |
| Fix a download bug | Scoped code change, regression test, and evidence that selection and file-layout contracts are preserved |
| Add a dataset | Source downloader, dispatcher integration, documented arguments and file layout, offline tests, and a gated live test where applicable |
| Update a provider endpoint | Verified provider reference, updated request construction, and checks that product identity and output layout remain compatible |
| Fix authentication | Correct credential resolution and request behavior, noninteractive tests, and redacted failure diagnostics |
| Fix incomplete downloads | Correct completion detection, recovery behavior, and tests covering partial failure and existing files |
| Improve performance | Representative before/after measurements and evidence of equivalent file selection, contents, and failure behavior |
| Download data for a user | Requested files in the specified destination and a summary of successful, skipped, and failed transfers |

For an actual data-download task:

- Establish the dataset, selection, destination, and required credentials.
- Obtain the required acknowledgement before starting transfers.
- Execute the supported download function.
- Verify the expected files and their suitability for the process tier.
- Report output paths, missing files, and any verification limitations.
- Report hashes only when requested or required by the task.