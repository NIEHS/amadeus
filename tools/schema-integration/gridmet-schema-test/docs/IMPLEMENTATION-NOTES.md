# New implementation and qualification

The direct-request example and its three external modules are newly written for this review. Old experiment drivers, expected-data fixtures, checkpoint caches, Python runtime adapters and fault scripts are absent. Required model artifacts come from the schema repository, not an earlier runtime experiment.

The additional relationship/time/geometry guards are explicit R checks after ordinary Amadeus output, not changes to the YAML model or scientific package. The new16case suite tests this implementation; the free-text interpretation case is accepted intentionally and does not approve scientific meaning.

Qualification used fresh annual2021provider files, the actual public calls and two new example points. Native results were accepted and loaded/query-checked in both native R engines. The staged folder was moved to its proposed repository layout and its schema tests run again successfully. A fresh machine/environment and full16-variable request remain team work.

During preparation, strict schema validation rejected missing grid/row-count metadata and array representation problems; those hand-off omissions were corrected. Failed development outputs are excluded. One interim check reused these newly downloaded files explicitly; final qualification used a fresh destination with two downloads, zero failures and zero skips. No scientific package change was made.

ToolRun.run_arguments contains a recorded argument/context summary, not an executable reconstruction of every literal call. The complete public arguments are visible in example_gridmet.R; source assets, request locations and dates provide further context. Output row count is0for raster processing because no tabular records are emitted; calculation counts reflect the returned rows.

Limits and different metadata conventions are documented in VALIDATION-AND-LIMITS.md. Do not infer production readiness from transport/schema success.

## Explicit-input interface update

All testers now choose variables,start,end,sites and engine explicitly. Missing/empty inputs fail before package loading/download. Old small-request observations describe an earlier supplied request, not a request silently selected now. A unified parallel-team guide supersedes the owner-first procedure. Source output folders/run IDs remain isolated. The ordinary public calls and post-science metadata rules are unchanged.
