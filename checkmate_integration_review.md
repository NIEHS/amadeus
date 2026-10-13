# checkmate validation review for planned SedonaDB and DuckDB integration

2026-10-07

- Proposal only; reviewed **10 R source files and 61 test/helper files**.
- No SedonaDB, DuckDB or DBI calls were found; database recommendations are prospective.
- Runtime adoption requires checkmate in Imports and a supported version.

- Database integration: Consistent, explicit validation can help future database adapters reuse amadeus input rules for parameters and materialized data; preserving current R behavior requires matching accepted inputs, normalization and side effects, while treating stricter checks as separate changes—not assuming checkmate provides database compatibility.
- **P1:** Priority checks for shared interfaces (arguments reused across functions or datasets, such as flags, radii and column selectors) and schemas (required columns and value types). These make common input rules easier to reuse and test across future database adapters.
- **P2:** Smaller consistency improvements to local checks, such as optional filters, dates, paths and exact choices. These reduce repeated validation and clarify errors; database benefits are indirect, and adoption depends on compatibility and practical value.
- **Start–end lines:** One-based, inclusive ranges from the dated review snapshot.
- **Current code:** Verbatim source excerpts. **Suggested code:** Replacements for those ranges unless noted.
- Diagnostics change unless retained explicitly; accepted inputs may still fail downstream.

- The snippets use documented checkmate APIs.
- Vector assertions explicitly select missing-value policies where relevant; `typed.missing = TRUE` avoids accepting empty/all-missing vectors of a different storage type.
- Scalar numbers retain infinity unless the current check already requires finiteness.
- See the maintainer references for [numeric vectors](https://mllg.github.io/checkmate/reference/checkNumeric.html), [character vectors](https://mllg.github.io/checkmate/reference/checkCharacter.html), [numbers](https://mllg.github.io/checkmate/reference/checkNumber.html), [strings](https://mllg.github.io/checkmate/reference/checkString.html), [flags](https://mllg.github.io/checkmate/reference/checkFlag.html), [choices](https://mllg.github.io/checkmate/reference/checkChoice.html), [subsets](https://mllg.github.io/checkmate/reference/checkSubset.html), [vectors](https://mllg.github.io/checkmate/reference/checkVector.html), [integer scalars](https://mllg.github.io/checkmate/reference/checkInt.html), and [directories](https://mllg.github.io/checkmate/reference/checkDirectoryExists.html).

<style>
.validation-table {width:100%; border-collapse:collapse; font-size:90%;}
.validation-table th, .validation-table td {border:1px solid #ddd; padding:8px; vertical-align:top;}
.validation-table pre {white-space:pre-wrap; overflow-wrap:anywhere; font-size:12px; min-width:220px;}
.validation-table td:last-child {min-width:250px;}
.table-scroll {overflow-x:auto;}
</style>

# major_R

- Highest-priority source recommendations; code examples are non-executing.

<div class="table-scroll">
<table class="validation-table">
<thead><tr><th>File path</th><th>affected_dataset</th><th>Start–end lines</th><th>Current code</th><th>Suggested checkmate code</th><th>Reason for replacement</th></tr></thead>
<tbody>
<tr><td><code>R/download_auxiliary.R</code></td><td>AQS; ecoregions; GEOS; GMTED; MERRA-2; NARR; NLCD; GRoADS; population; HMS; Köppen–Geiger; MODIS; TRI; NEI; gridMET; TerraClimate; HUC; EDGAR; PRISM; CropScape/CDL; GOES; IMPROVE; drought (SPEI, EDDI, USDM); OLM (download_olm in R/ignore.R). Shared download_permit guard.</td><td>204–211</td><td><pre><code class="language-r">    if (!acknowledgement) {
      stop(paste0(
        &quot;Data download acknowledgement is set to FALSE. &quot;,
        &quot;Please acknowledge that the data downloaded using this &quot;,
        &quot;function may be very large and use lots of machine storage &quot;,
        &quot;and memory.\n&quot;
      ))
    }</code></pre></td><td><pre><code class="language-r">checkmate::assertFlag(acknowledgement)
if (!acknowledgement) {
  stop(paste0(
    &quot;Data download acknowledgement is set to FALSE. &quot;,
    &quot;Please acknowledge that the data downloaded using this &quot;,
    &quot;function may be very large and use lots of machine storage &quot;,
    &quot;and memory.\n&quot;
  ))
}</code></pre></td><td><ul><li>P1 — Validate the flag before any side effects while retaining the useful consent explanation.</li><li>Tightening: nonzero numeric values previously passed !acknowledgement; now only logical scalars pass.</li><li>FALSE retains its existing message (test-gridmet.R:264–278 and test-download.R:1443–1446).</li><li>Missing/NA/vector inputs get clearer errors. assertTRUE alone would discard the explanation.</li></ul></td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>AQS; ecoregions; GEOS; GMTED; MERRA-2; NARR; NLCD; GRoADS; population; HMS; Köppen–Geiger; MODIS; TRI; NEI; gridMET; TerraClimate; HUC; EDGAR; PRISM; CropScape/CDL; GOES; IMPROVE; drought (SPEI, EDDI, USDM). Shared download_run_method helper.</td><td>324–329</td><td><pre><code class="language-r">  if (is.null(urls) || length(urls) == 0) {
    stop(&quot;No URLs provided for download.\n&quot;)
  }
  if (is.null(destfiles) || length(destfiles) != length(urls)) {
    stop(&quot;destfiles must have same length as urls.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertCharacter(urls, min.len = 1L,
  any.missing = FALSE, min.chars = 1L)
checkmate::assertCharacter(destfiles, len = length(urls),
  any.missing = FALSE, min.chars = 1L)</code></pre></td><td><ul><li>P1 — Give the downloader a consistent character-vector contract and diagnose which argument is wrong before HTTP/file work.</li><li>Tightening: the old code checked only non-NULL/length, allowing non-character values, NA and empty strings.</li><li>URLs are not validated for syntax or reachability.</li><li>Messages change; test-download.R:163–182 matches &quot;No URLs provided&quot; and &quot;same length&quot;.</li></ul></td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>GEOS; MERRA-2; MODIS (metadata/path helpers).</td><td>750–755</td><td><pre><code class="language-r">  if (is.null(path) || !is.character(path) || length(path) &lt; 1 || anyNA(path)) {
    stop(&quot;`path` must be a non-empty character vector.\n&quot;)
  }
  if (!is.character(pattern) || length(pattern) != 1L || !nzchar(pattern)) {
    stop(&quot;`pattern` must be a single non-empty character string.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertCharacter(path, min.len = 1L,
  any.missing = FALSE)
checkmate::assertString(pattern, min.chars = 1L)</code></pre></td><td><ul><li>P1 — Consolidate path-vector and scalar-pattern checks in info_resolve_paths.</li><li>Preserve multiple files/directories, duplicates and empty path strings; do not add assertFileExists before directory expansion.</li><li>Tightening: NA_character_ pattern currently passes nzchar(NA), but assertString rejects it.</li><li>Messages change; test-info-helpers.R:108–119 is affected.</li></ul></td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>GEOS at this range; MERRA-2 and MODIS for the additional include_file ranges cited in this row.</td><td>815–821</td><td><pre><code class="language-r">  if (
    !is.logical(include_file) ||
      length(include_file) != 1L ||
      is.na(include_file)
  ) {
    stop(&quot;`include_file` must be a single logical value (TRUE/FALSE).\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertFlag(include_file)</code></pre></td><td><ul><li>P1 — Replace the repeated logical/length/NA check with an explicit scalar contract.</li><li>Also applies to lines 887–893 and 988–994.</li><li>Ordinary accepted inputs stay TRUE/FALSE; error text changes and test-info-helpers.R:90–101 needs revision.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>Köppen–Geiger at this range; ecoregions for the additional frac/radius/drop ranges cited in this row.</td><td>267–275</td><td><pre><code class="language-r">    if (!is.logical(frac) || length(frac) != 1L || is.na(frac)) {
      stop(&quot;`frac` should be a single logical value (TRUE/FALSE).&quot;)
    }
    if (!is.numeric(radius) || length(radius) != 1L || is.na(radius)) {
      stop(&quot;`radius` should be a single numeric value.&quot;)
    }
    if (radius &lt; 0) {
      stop(&quot;`radius` should be greater than or equal to 0.&quot;)
    }</code></pre></td><td><pre><code class="language-r">checkmate::assertFlag(frac)
checkmate::assertNumber(radius, lower = 0, finite = FALSE)</code></pre></td><td><ul><li>P1 — One consistent flag and nonnegative numeric scalar check.</li><li>Retains acceptance of positive infinity at this validation boundary; making radius finite would be a separate input restriction (later integer conversion may already fail).</li><li>Apply the same pattern to lines 1012–1020, with assertFlag(drop) for 1024–1026.</li><li>Error text changes.</li><li>Ecoregion invalid flags are tested without text at test-ecoregion.R:296–311.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>NLCD.</td><td>581–589</td><td><pre><code class="language-r">  if (!is.numeric(radius)) {
    stop(&quot;radius is not a numeric.&quot;)
  }
  if (radius &lt; 0) {
    stop(&quot;radius has not a likely value.&quot;)
  }
  if (!is.logical(drop) || length(drop) != 1 || is.na(drop)) {
    stop(&quot;`drop` should be a single logical value (TRUE/FALSE).&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertNumber(radius, lower = 0, finite = FALSE)
checkmate::assertFlag(drop)</code></pre></td><td><ul><li>P1 — NLCD receives clear errors for empty, missing or multiple radii instead of if-condition errors.</li><li>Preserve numeric integer/double scalars and positive infinity at the guard; reject character &quot;1000&quot; as before.</li><li>Messages change: test-nlcd.R:281–288 and 298–305 match both old radius messages.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>TRI via sum_edc and the additional calculate_tri range; user-supplied emissions data passed directly to sum_edc.</td><td>2107–2110</td><td><pre><code class="language-r">    if (!is.logical(use_threshold) || length(use_threshold) != 1L ||
          is.na(use_threshold)) {
      stop(&quot;`use_threshold` must be TRUE or FALSE.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">checkmate::assertFlag(use_threshold)</code></pre></td><td><ul><li>P1 — Consolidate the scalar boolean contract in sum_edc.</li><li>Repeat for calculate_tri at 2353–2356.</li><li>Error text changes; sum_edc tests at test-sedc.R:162–171 check only an error, but test-tri.R:842–850 matches the full old message.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>TRI via sum_edc; user-supplied emissions data passed directly to sum_edc.</td><td>2124–2130</td><td><pre><code class="language-r">        if (length(c0_values) != 1L || anyNA(c0_values) ||
              !nzchar(trimws(c0_values))) {
          stop(&quot;`C0` as character must be a single non-empty column name.\n&quot;)
        }
        if (!c0_values %in% names(from)) {
          stop(&quot;`C0` column `&quot;, c0_values, &quot;` was not found in `from`.\n&quot;)
        }</code></pre></td><td><pre><code class="language-r">checkmate::assertString(c0_values)
checkmate::assertString(trimws(c0_values), min.chars = 1L,
  .var.name = &quot;C0&quot;)
checkmate::assertChoice(c0_values, names(from),
  .var.name = &quot;C0&quot;)</code></pre></td><td><ul><li>P1 — In the existing is.character branch, separate scalar validity, whitespace-only rejection and column membership.</li><li>Keep the original untrimmed name for lookup: do not assign trimws back to c0_values.</li><li>Accepted ordinary strings and exact matching stay the same; errors now identify C0.</li><li>Retain the surrounding NULL and one-column-data.frame handling. test-sedc.R:123–152 asserts errors without matching text.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>TRI via sum_edc; user-supplied emissions data passed directly to sum_edc.</td><td>2136–2141</td><td><pre><code class="language-r">      if (!is.numeric(c0_values) || length(c0_values) != nrow(from)) {
        stop(
          &quot;`C0` must be NULL, character(1) column name in `from`, &quot;,
          &quot;or a numeric vector with length `nrow(from)`.\n&quot;
        )
      }</code></pre></td><td><pre><code class="language-r">checkmate::assertNumeric(c0_values, len = nrow(from),
  any.missing = TRUE, finite = FALSE, typed.missing = TRUE,
  .var.name = &quot;C0&quot;)</code></pre></td><td><ul><li>P1 — After existing C0 normalization, combine numeric type and row-count checks.</li><li>Explicitly preserve missing numeric values and infinity; typed.missing prevents accidentally accepting logical NA vectors or empty logical vectors.</li><li>No new positivity constraint.</li><li>Error text changes; test-sedc.R:143–152 checks only an error.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>MODIS (including MCD14ML fire); HMS; NARR; GEOS; MERRA-2; gridMET; TerraClimate; PRISM; EDGAR; GOES; drought (SPEI, EDDI, USDM); IMPROVE. Shared temporal-summary helpers; calc_summarize_temporal has no current internal callers.</td><td>1254–1259</td><td><pre><code class="language-r">  if (!locs_id %in% names(covar)) {
    stop(sprintf(&quot;`locs_id` column &#x27;%s&#x27; not found in `covar`.\n&quot;, locs_id))
  }
  if (!time_col %in% names(covar)) {
    stop(sprintf(&quot;`time_col` column &#x27;%s&#x27; not found in `covar`.\n&quot;, time_col))
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertString(locs_id)
checkmate::assertChoice(locs_id, names(covar))
checkmate::assertString(time_col)
checkmate::assertChoice(time_col, names(covar))</code></pre></td><td><ul><li>P1 — Validate schema selectors before grouping; reuse in calc_summarize_native_time (1337–1342) and calc_summarize_temporal (1484–1495).</li><li>Tightening: %in% previously coerced some numeric/factor selectors; require explicit character column names.</li><li>Exact matching and existing empty-string column names remain allowed.</li><li>Error messages change.</li><li>These checks validate available columns, not SQL quoting or connection state.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>MODIS (including MCD14ML fire); HMS; NARR; GEOS; MERRA-2; gridMET; TerraClimate; PRISM; EDGAR; GOES; drought (SPEI, EDDI, USDM); IMPROVE. Shared temporal-summary helpers; calc_summarize_temporal has no current internal callers.</td><td>1260–1270</td><td><pre><code class="language-r">  if (!is.null(group_cols_extra)) {
    missing_extra &lt;- setdiff(group_cols_extra, names(covar))
    if (length(missing_extra) &gt; 0L) {
      stop(
        sprintf(
          &quot;Grouping column(s) not found in `covar`: %s.\n&quot;,
          paste(missing_extra, collapse = &quot;, &quot;)
        )
      )
    }
  }</code></pre></td><td><pre><code class="language-r">if (!is.null(group_cols_extra)) {
  checkmate::assertCharacter(group_cols_extra,
    any.missing = FALSE, typed.missing = TRUE)
  checkmate::assertSubset(group_cols_extra, names(covar))
}</code></pre></td><td><ul><li>P1 — A reusable optional grouping-column contract; retain NULL, character(0), duplicate selectors, and column order.</li><li>Tightening: reject non-character selectors and NA even if malformed input happens to have an NA column name.</li><li>Same family appears at 1343–1353 and 1496–1506.</li><li>Error text changes; retain missing-column context through the argument name.</li></ul></td></tr>
<tr><td><code>R/process.R</code></td><td>MODIS MCD14ML fire detections.</td><td>692–694</td><td><pre><code class="language-r">  if (!all(required_cols %in% names(txt_data))) {
    stop(&quot;MCD14ML input is missing one or more required columns.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertSubset(required_cols, names(txt_data),
  .var.name = &quot;required MCD14ML columns&quot;)</code></pre></td><td><ul><li>P1 — Report which required columns are absent, while allowing extra fields and any order.</li><li>Both operands already contain character names.</li><li>Suitable for checking materialized import schemas before database ingestion.</li><li>Error text changes; do not reverse the subset arguments.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>IMPROVE.</td><td>5120–5123</td><td><pre><code class="language-r">  if (!is.numeric(radius) || length(radius) != 1L ||
      !is.finite(radius) || radius &lt; 0) {
    stop(&quot;`radius` must be a finite nonnegative number in meters.&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertNumber(radius, lower = 0, finite = TRUE)</code></pre></td><td><ul><li>P1 — IMPROVE already requires a finite nonnegative numeric scalar, so this consolidates the existing contract without intentionally tightening it.</li><li>Keep this distinct from radius guards that currently accept Inf.</li><li>Message changes; test-improve.R:505–506 matches &quot;radius&quot; and should still identify the argument.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>IMPROVE.</td><td>5124–5127</td><td><pre><code class="language-r">  if (!is.character(locs_id) || length(locs_id) != 1L ||
      anyNA(locs_id) || !locs_id %in% names(locs)) {
    stop(&quot;`locs_id` must name an identifier column in `locs`.&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertString(locs_id)
checkmate::assertChoice(locs_id, names(locs))</code></pre></td><td><ul><li>P1 — Identify invalid selectors and missing columns separately before converting locations.</li><li>Preserve the existing scalar character and exact membership requirement.</li><li>Keep unique/nonmissing ID and reserved-output-name checks below this block: they protect joins and output schema.</li><li>Error text changes.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>IMPROVE.</td><td>5147–5152</td><td><pre><code class="language-r">  if (!all(required %in% names(values))) {
    stop(&quot;`from` must contain FactDate, ParamCode, and FactValue.&quot;)
  }
  if (!is.numeric(values$FactValue)) {
    stop(&quot;`FactValue` must be numeric.&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertSubset(required, names(values),
  .var.name = &quot;required IMPROVE columns&quot;)
checkmate::assertNumeric(values$FactValue,
  any.missing = TRUE, finite = FALSE, typed.missing = TRUE,
  .var.name = &quot;FactValue&quot;)</code></pre></td><td><ul><li>P1 — Consolidate required-column and numeric-value contracts while preserving numeric NA/Inf and extra columns.</li><li>Do not impose generic complete-case validation on exposure data. typed.missing retains strict type checking for all-missing/empty vectors.</li><li>Error text changes.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>IMPROVE.</td><td>5158–5160</td><td><pre><code class="language-r">  if (anyNA(parameters) || any(!nzchar(parameters))) {
    stop(&quot;`ParamCode` must contain nonempty, nonmissing parameter codes.&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertCharacter(parameters, min.chars = 1L,
  any.missing = FALSE, typed.missing = TRUE,
  .var.name = &quot;ParamCode&quot;)</code></pre></td><td><ul><li>P1 — Combine missing-value and empty-code validation after the existing as.character conversion.</li><li>Preserve character(0), whitespace-only codes and numeric-to-character normalization.</li><li>Do not move the assertion before conversion.</li><li>Error text changes.</li></ul></td></tr>
<tr><td><code>R/process.R</code></td><td>TRI.</td><td>1477–1484</td><td><pre><code class="language-r">  if (!is.character(variables) || length(variables) &lt; 1 || anyNA(variables)) {
    stop(
      &quot;`variables` must be a non-empty character vector of regex patterns.\n&quot;
    )
  }
  if (length(variables) &gt; 0 &amp;&amp; any(!nzchar(trimws(variables)))) {
    stop(&quot;`variables` cannot include empty patterns.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertCharacter(variables, min.len = 1L,
  any.missing = FALSE)
checkmate::assertCharacter(trimws(variables), min.chars = 1L,
  any.missing = FALSE, .var.name = &quot;variables&quot;)</code></pre></td><td><ul><li>P2 — Collapse type/length/NA and whitespace checks without modifying the regex patterns used later.</li><li>Empty vectors and whitespace-only patterns remain invalid; do not treat min.len as a per-string length check.</li><li>Error text changes; regex syntax and matching are still separate concerns.</li></ul></td></tr>
<tr><td><code>R/process.R</code></td><td>TRI.</td><td>1485–1497</td><td><pre><code class="language-r">  if (!is.null(chemical)) {
    if (!is.character(chemical) || length(chemical) &lt; 1 || anyNA(chemical)) {
      stop(&quot;`chemical` must be NULL or a non-empty character vector.\n&quot;)
    }
    if (any(!nzchar(trimws(chemical)))) {
      stop(&quot;`chemical` cannot include empty patterns.\n&quot;)
    }
  }
  if (
    !is.logical(ignore_case) || length(ignore_case) != 1 || is.na(ignore_case)
  ) {
    stop(&quot;`ignore_case` must be TRUE or FALSE.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">if (!is.null(chemical)) {
  checkmate::assertCharacter(chemical, min.len = 1L,
    any.missing = FALSE)
  checkmate::assertCharacter(trimws(chemical), min.chars = 1L,
    any.missing = FALSE, .var.name = &quot;chemical&quot;)
}
checkmate::assertFlag(ignore_case)</code></pre></td><td><ul><li>P2 — Preserve optional chemical filters and their original regex text, while making the flag contract consistent.</li><li>NULL remains allowed but character(0) remains invalid.</li><li>Messages change; no change to industry_group matching on the following line.</li></ul></td></tr>
<tr><td><code>R/process.R</code></td><td>IMPROVE.</td><td>4719–4721</td><td><pre><code class="language-r">  if (is.null(path) || !dir.exists(path)) {
    stop(&quot;`path` must be a valid directory path.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertString(path)
checkmate::assertDirectoryExists(path)</code></pre></td><td><ul><li>P2 — An existing-directory check is appropriate for process_improve, which reads a directory.</li><li>Scalar checking must precede the filesystem check.</li><li>No new readability/writability policy is proposed; no creation is performed.</li><li>Errors become argument-specific.</li><li>Do not copy this to download destinations that are created later or to .gdb inputs.</li></ul></td></tr>
<tr><td><code>R/process.R</code></td><td>GRoADS.</td><td>2366–2370</td><td><pre><code class="language-r">  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop(
      &quot;`path` must be a single file path to a .shp or .gdb roads file.&quot;
    )
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertString(path)</code></pre></td><td><ul><li>P2 — Replace only the scalar character/NA guard for roads.</li><li>Preserve file.exists and extension checks below it: a .gdb is a directory and must remain accepted.</li><li>Empty strings still fail existence validation later.</li><li>Messages change; do not substitute an ordinary-file-only assertion for the whole block.</li></ul></td></tr>
<tr><td><code>R/download.R</code></td><td>GOES (including goes_adp).</td><td>4940–4943</td><td><pre><code class="language-r">  satellite &lt;- as.character(satellite)
  if (!satellite %in% c(&quot;16&quot;, &quot;18&quot;)) {
    stop(&quot;satellite must be &#x27;16&#x27; or &#x27;18&#x27;.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">satellite &lt;- as.character(satellite)
checkmate::assertChoice(satellite, c(&quot;16&quot;, &quot;18&quot;))</code></pre></td><td><ul><li>P2 — A finite scalar choice gives a clearer error.</li><li>Keep as.character so numeric 16/18 and factors continue to normalize as before.</li><li>No partial matching is introduced.</li><li>Errors change; test-goes.R:45–54 matches &quot;satellite must be&quot; and needs review.</li></ul></td></tr>
<tr><td><code>R/download.R</code></td><td>Drought: SPEI and EDDI only; USDM does not use this timescale guard.</td><td>5368–5370</td><td><pre><code class="language-r">    if (is.na(timescale) || timescale &lt; 1L) {
      stop(&quot;`timescale` must be a positive integer (months).\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">checkmate::assertInt(timescale, lower = 1L)</code></pre></td><td><ul><li>P2 — Inside the existing source branch, validate the already-coerced timescale.</li><li>Keep as.integer at 5366: moving the assertion ahead of it would reject numeric strings and fractional inputs that are currently truncated.</li><li>Empty/vector/NA values get deliberate scalar errors. test-drought.R:251–260 matches &quot;timescale&quot;, which remains the argument name.</li></ul></td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>TRI (get_tri_info metadata helper).</td><td>657–659</td><td><pre><code class="language-r">    if (!is.numeric(year) || length(year) != 1 || is.na(year)) {
      stop(&quot;`year` must be NULL or a single numeric value.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">checkmate::assertNumber(year, finite = FALSE)</code></pre></td><td><ul><li>P2 — Inside the existing !is.null(year) branch, replace the type/length/NA guard.</li><li>Do not impose an integer year or historical range silently: the current guard accepts fractional years and infinity.</li><li>Error text changes; NULL remains handled by the outer branch.</li></ul></td></tr>
<tr><td><code>R/download.R</code></td><td>AQS.</td><td>285–288</td><td><pre><code class="language-r">    if (length(year) == 1) {
      year &lt;- c(year, year)
    }
    stopifnot(length(year) == 2)</code></pre></td><td><pre><code class="language-r">if (length(year) == 1) {
  year &lt;- c(year, year)
}
checkmate::assertVector(year, len = 2L, strict = FALSE)</code></pre></td><td><ul><li>P2 — Preserve endpoint normalization, then report the required length clearly instead of an expression failure.</li><li>Tightening: assertVector excludes some non-vector objects that bare length accepts; confirm supported year classes before adoption.</li><li>It intentionally adds no numeric/date parsing rule.</li><li>Error text changes.</li><li>Apply the analogous date pattern below rather than an integer-only check.</li></ul></td></tr>
<tr><td><code>R/download.R</code></td><td>Drought: SPEI, EDDI and USDM at this range; GEOS for the analogous process.R range cited in this row.</td><td>5351–5354</td><td><pre><code class="language-r">  if (length(date) == 1L) {
    date &lt;- c(date, date)
  }
  stopifnot(length(date) == 2L)</code></pre></td><td><pre><code class="language-r">if (length(date) == 1L) {
  date &lt;- c(date, date)
}
checkmate::assertVector(date, len = 2L, strict = FALSE)</code></pre></td><td><ul><li>P2 — Consolidate repeated length guards while allowing classed Date/POSIXct vectors and character dates; strict=FALSE allows attributes.</li><li>Scalar duplication must remain before the assertion.</li><li>Tightening: non-vector objects with a length method may now fail; strict=FALSE still allows attributes and some structured vectors, so this is not a date-type validator.</li><li>Date parsing, sorting and source availability checks stay separate.</li><li>The same pattern occurs throughout process.R, e.g. 3125–3128.</li><li>Error text changes.</li></ul></td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>Dataset-independent location validation (process_conformity); no current internal dataset callers.</td><td>28–30</td><td><pre><code class="language-r">    if (!all(keyword %in% names(locs))) {
      stop(&quot;locs should have &#x27;lon&#x27;, &#x27;lat&#x27;, (and &#x27;time&#x27;) fields.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">checkmate::assertSubset(keyword, names(locs),
  .var.name = &quot;required location columns&quot;)</code></pre></td><td><ul><li>P2 — Name the missing required location fields; keep keyword construction above the block so check_time continues to determine whether time is required.</li><li>Extra fields remain accepted.</li><li>Error text changes; no new spatial-class or CRS policy.</li></ul></td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>Dataset-independent spatiotemporal conversion (dt_as_mysftime, also reached through conversion helpers); applies to user-supplied data, with no specific dataset pipeline identified.</td><td>85–87</td><td><pre><code class="language-r">  if (any(!(c(lonname, latname, timename) %in% colnames(x)))) {
    stop(&quot;Some of lon, lat, time columns missing or mispelled&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertSubset(c(lonname, latname, timename),
  colnames(x), .var.name = &quot;location/time columns&quot;)</code></pre></td><td><ul><li>P2 — Replace a generic misspelled-column error with the missing selectors.</li><li>Keep the preceding data.table-class check separate.</li><li>Tightening: unlike %in%, the subset check rejects non-character selectors instead of coercing them.</li><li>NULL selectors still drop out of c(); deciding to require three scalar strings is a separate contract change.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>Dataset-independent temporal-function validation (check_fun_temporal); no current internal dataset callers.</td><td>1025–1045</td><td><pre><code class="language-r">  if (
    !is.character(fun_temporal) ||
      length(fun_temporal) != 1L
  ) {
    stop(
      paste0(
        &quot;`fun_temporal` must be NULL or a single &quot;,
        &quot;character string.\n&quot;
      )
    )
  }
  if (!fun_temporal %in% allowed) {
    stop(
      sprintf(
        paste0(
          &quot;`fun_temporal` must be one of: %s.\n&quot;
        ),
        paste(allowed, collapse = &quot;, &quot;)
      )
    )
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertString(fun_temporal)
checkmate::assertChoice(fun_temporal, allowed)</code></pre></td><td><ul><li>P2 — Replace repeated scalar-string and enumerated-choice validation, preserving the NULL return at 1021–1023 and invisible(NULL) at 1046.</li><li>This is exact membership already, so partial names remain invalid.</li><li>Error text changes; test-calc.R:698–712 matches &quot;single&quot; and &quot;must be one of&quot;.</li><li>This is not a replacement for match.arg.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>MODIS (including MCD14ML fire); HMS; NARR; GEOS; MERRA-2; gridMET; TerraClimate; PRISM; EDGAR; GOES; drought (SPEI, EDDI, USDM); IMPROVE. Reached through bucket_time_by_unit and calc_summarize_by when temporal grouping is requested.</td><td>1169–1171</td><td><pre><code class="language-r">  if (!is.character(unit) || length(unit) != 1L || !unit %in% names(aliases)) {
    stop(&quot;`unit` must be one valid `.by_time` time-unit token.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertString(unit)
checkmate::assertChoice(unit, names(aliases))</code></pre></td><td><ul><li>P2 — Consistent exact-token validation before aliases[[unit]] normalization.</li><li>Preserve all alias names, including plurals.</li><li>Reject NA with a direct argument error rather than relying on the membership test.</li><li>Error messages change; output normalization remains untouched.</li></ul></td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>gridMET (process_gridmet layer-time parsing).</td><td>1072–1072</td><td><pre><code class="language-r">  stopifnot(is.character(layer_names))</code></pre></td><td><pre><code class="language-r">checkmate::assertCharacter(layer_names,
  any.missing = TRUE, typed.missing = TRUE)</code></pre></td><td><ul><li>P2 — Replace the opaque type assertion at the layer-name parsing boundary while keeping the existing source-specific parse error below it.</li><li>Preserve character NA and character(0) at this guard; typed.missing avoids accepting all-missing or empty vectors of other types.</li><li>Error text changes.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>NLCD; GMTED; NARR; GEOS; population; MERRA-2; gridMET; TerraClimate; GOES (via calc_worker); MODIS raster products (via calculate_modis_daily); PRISM; EDGAR; CropScape/CDL; drought (SPEI, EDDI, USDM). Applies when raster weights reach this guard.</td><td>406–411</td><td><pre><code class="language-r">  if (!is.numeric(terra::values(weights_obj)[, 1])) {
    stop(&quot;`weights` raster values must be numeric.&quot;)
  }
  if (any(terra::values(weights_obj)[, 1] &lt; 0, na.rm = TRUE)) {
    stop(&quot;`weights` values must be non-negative.&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertNumeric(terra::values(weights_obj)[, 1],
  lower = 0, any.missing = TRUE, finite = FALSE,
  typed.missing = TRUE, .var.name = &quot;weights raster values&quot;)</code></pre></td><td><ul><li>P2 — Combine numeric and nonnegative checks while preserving NA cells and positive infinity.</li><li>Keep raster class, single-layer, CRS and overlap checks as domain validations.</li><li>Error text changes; this is not a proposal to scan a future lazy database relation eagerly.</li></ul></td></tr>
<tr><td><code>R/process.R</code></td><td>NLCD.</td><td>1199–1204</td><td><pre><code class="language-r">    if (!is.character(path) || is.null(path)) {
      stop(&quot;path is not a character.&quot;)
    }
    if (!dir.exists(path)) {
      stop(&quot;path does not exist.&quot;)
    }</code></pre></td><td><pre><code class="language-r">checkmate::assertString(path)
checkmate::assertDirectoryExists(path)</code></pre></td><td><ul><li>P2 — Consolidate process_nlcd character/scalar and existing-directory validation; add no access-permission requirement.</li><li>Preserve directory inputs and later year/product checks. Invalid-input diagnostics change; verify empty, missing and vector paths.</li><li>Update diagnostic expectations in test-nlcd.R:202–209 and 360–371 only if this replacement is implemented.</li></ul></td></tr>
<tr><td><code>R/process.R</code></td><td>GOES (including goes_adp).</td><td>4428–4437</td><td><pre><code class="language-r">  if (!all(file.exists(paths))) {
    missing_paths &lt;- paths[!file.exists(paths)]
    stop(
      paste0(
        &quot;Some GOES paths do not exist:\n&quot;,
        paste0(&quot;  - &quot;, missing_paths, collapse = &quot;\n&quot;),
        &quot;\nFiles must match pattern &#x27;^OR_(ADP|ABI-L2-ADP).*\\.nc$&#x27;.\n&quot;
      )
    )
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertFileExists(paths)</code></pre></td><td><ul><li>P2 — Candidate after directory expansion, GOES filename filtering and the existing nonempty-selection guard.</li><li>Tightening: require files rather than any existing filesystem entry. Retain GOES naming/date rules; generic errors lose the current filename-pattern explanation.</li><li>The missing-path test at test-goes.R:978–985 matches the old diagnostic. Adopt only if the simpler check is worth that loss of context.</li></ul></td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>Shared download_run_method: all downloader datasets listed in its URL/destination row.</td><td>357–361</td><td><pre><code class="language-r">    # Create directory if needed
    destdir &lt;- dirname(destfile)
    if (!dir.exists(destdir)) {
      dir.create(destdir, recursive = TRUE)
    }</code></pre></td><td><pre><code class="language-r">    # Create directory if needed
    destdir &lt;- dirname(destfile)
    if (!dir.exists(destdir)) {
      dir.create(destdir, recursive = TRUE)
    }
    checkmate::assertPathForOutput(destfile, overwrite = TRUE)</code></pre></td><td><ul><li>P2 — Optional stronger preflight check after parent-directory creation and existing-file filtering; retain the creation block.</li><li>Behavior change: requires a writable parent; existing output files must be readable and writable. overwrite = TRUE allows retrying existing empty files.</li><li>Placement here makes validation errors stop the call before per-file request handling. Decide whether failures should instead join the existing per-file failure report before adopting.</li><li>Keep cache decisions, retries, cleanup and nonzero-size verification. Test missing parents, existing empty/nonempty files and directory-valued destinations.</li></ul></td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>OLM via download_olm in R/ignore.R; direct download_sink users.</td><td>620–623</td><td><pre><code class="language-r">    if (file.exists(command_txt)) {
      file.remove(command_txt)
    }
    sink(file = command_txt, append = FALSE)</code></pre></td><td><pre><code class="language-r">    checkmate::assertPathForOutput(command_txt, overwrite = TRUE)
    if (file.exists(command_txt)) {
      file.remove(command_txt)
    }
    sink(file = command_txt, append = FALSE)</code></pre></td><td><ul><li>P2 — Optional preflight check before deleting/recreating a command-log file; preserve existing sink behavior.</li><li>Behavior change: overwrite = TRUE requires an existing file to be readable and writable, whereas deletion/recreation can depend on parent permissions instead.</li><li>Test new/existing files, missing parents and directory-valued paths. Existing test-download.R:536–547 covers an existing writable file; no automatic expectation rewrite is needed.</li><li>Do not replace directory creation, mixed file/directory expansion, HUC/USDM discovery branches or download cache checks with unconditional assertions.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>TRI via sum_edc; user-supplied emissions data.</td><td>2100–2103</td><td><pre><code class="language-r">    if (!is.numeric(decay_range) || length(decay_range) != 1L ||
          is.na(decay_range) || decay_range &lt;= 0) {
      stop(&quot;`decay_range` must be a single positive numeric value.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testNumber(decay_range, finite = FALSE) || decay_range &lt;= 0) {
      stop(&quot;`decay_range` must be a single positive numeric value.\n&quot;)
    }</code></pre></td><td><ul><li>P1 — Shared decay-range contract: consolidate numeric scalar validation; retain the strict positive check, Inf acceptance and existing error.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>TRI via sum_edc; user-supplied emissions data.</td><td>2104–2106</td><td><pre><code class="language-r">    if (!is.character(target_fields) || length(target_fields) &lt; 1L) {
      stop(&quot;`target_fields` must be a non-empty character vector.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testCharacter(target_fields, min.len = 1L, any.missing = TRUE, typed.missing = TRUE)) {
      stop(&quot;`target_fields` must be a non-empty character vector.\n&quot;)
    }</code></pre></td><td><ul><li>P1 — Shared target-column selector contract: preserve NA_character_ acceptance and subsequent missing-column detection. Retain the existing error; add no nonblank restriction.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>TRI (calculate_tri).</td><td>2350–2352</td><td><pre><code class="language-r">  if (!is.numeric(decay_range)) {
    stop(&quot;`decay_range` should be numeric.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testNumeric(decay_range, any.missing = TRUE, typed.missing = TRUE, finite = FALSE)) {
    stop(&quot;`decay_range` should be numeric.\n&quot;)
  }</code></pre></td><td><ul><li>P1 — Shared decay-range argument: retain type-only validation, typed empty/missing values, Inf and later checks. Preserve the error; verify classed/dimensioned inputs before adoption.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>TRI (calculate_tri target-field values).</td><td>2424–2426</td><td><pre><code class="language-r">              if (!is.numeric(tri_col_c0)) {
                stop(&quot;TRI target field `&quot;, tri_col, &quot;` is not numeric.\n&quot;)
              }</code></pre></td><td><pre><code class="language-r">              if (!checkmate::testNumeric(tri_col_c0, any.missing = TRUE, typed.missing = TRUE, finite = FALSE)) {
                stop(&quot;TRI target field `&quot;, tri_col, &quot;` is not numeric.\n&quot;)
              }</code></pre></td><td><ul><li>P1 — Validate numeric schema values while preserving typed empty/missing values and Inf. Retain the field-specific error; verify classed/dimensioned inputs before adoption.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>HMS (calculate_hms).</td><td>2609–2611</td><td><pre><code class="language-r">  if (!is.logical(frac) || length(frac) != 1L || is.na(frac)) {
    stop(&quot;`frac` should be a single logical value (TRUE/FALSE).&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testFlag(frac)) {
    stop(&quot;`frac` should be a single logical value (TRUE/FALSE).&quot;)
  }</code></pre></td><td><ul><li>P1 — Extend the shared frac flag pattern to HMS. Preserve TRUE/FALSE acceptance, the existing stop message and validation position.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>GRoADS (calculate_groads).</td><td>3413–3415</td><td><pre><code class="language-r">  if (!is.logical(drop) || length(drop) != 1 || is.na(drop)) {
    stop(&quot;`drop` should be a single logical value (TRUE/FALSE).&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testFlag(drop)) {
    stop(&quot;`drop` should be a single logical value (TRUE/FALSE).&quot;)
  }</code></pre></td><td><ul><li>P1 — Extend the shared drop flag pattern to GRoADS. Preserve TRUE/FALSE acceptance, the existing stop message and validation position.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>EDGAR (calculate_edgar).</td><td>4275–4277</td><td><pre><code class="language-r">  if (!is.numeric(radius) || length(radius) != 1) {
    stop(&quot;`radius` must be numeric(1).&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testNumeric(radius, len = 1L, any.missing = TRUE, finite = FALSE, typed.missing = TRUE)) {
    stop(&quot;`radius` must be numeric(1).&quot;)
  }</code></pre></td><td><ul><li>P1 — Shared radius argument: preserve this guard&#x27;s numeric NA/NaN, Inf and negative-value acceptance. Logical NA remains invalid; retain the existing error. Verify classed/dimensioned inputs before adoption.</li></ul></td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>Datasets using shared temporal-summary helpers (check_by_time).</td><td>1106–1114</td><td><pre><code class="language-r">  if (!is.character(.by_time) || length(.by_time) != 1L) {
    stop(
      paste0(
        &quot;`.by_time` must be NULL or a single character string naming &quot;,
        &quot;a temporal unit.
&quot;
      )
    )
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testCharacter(.by_time, len = 1L, any.missing = TRUE, typed.missing = TRUE)) {
    stop(
      paste0(
        &quot;`.by_time` must be NULL or a single character string naming &quot;,
        &quot;a temporal unit.
&quot;
      )
    )
  }</code></pre></td><td><ul><li>P1 — Shared temporal-grouping interface: preserve optional NULL and first-stage NA_character_ acceptance; retain the subsequent membership error and invisible(NULL) return.</li></ul></td></tr>
</tbody></table>
</div>

## Summary: strongest meaning-preserving replacements

These are the strongest replacement candidates, not mandatory fixes. Replace only the condition inside the existing `if (...) stop(...)` block; retain the exact stop body, validation position and surrounding logic. The conditions below are whitespace-normalized. They preserve the reviewed ordinary-input contracts; confirm classed/attributed inputs against a pinned checkmate version before implementation.

<div class="table-scroll">
<table class="validation-table">
<thead><tr><th>Scope</th><th>Current condition</th><th>Checkmate replacement condition</th><th>Preserve</th></tr></thead>
<tbody>
<tr><td>Scalar logical flags (frac, drop, use_threshold, include_file)</td><td><pre><code class="language-r">!is.logical(frac) || length(frac) != 1L || is.na(frac)</code></pre></td><td><pre><code class="language-r">!checkmate::testFlag(frac)</code></pre></td><td>Use the corresponding argument name; TRUE and FALSE remain valid.</td></tr>
<tr><td>Numeric scalar radius — R/calculate_covariates.R:270–272, 1015–1017</td><td><pre><code class="language-r">!is.numeric(radius) || length(radius) != 1L || is.na(radius)</code></pre></td><td><pre><code class="language-r">!checkmate::testNumber(radius, finite = FALSE)</code></pre></td><td>Keep the separate negative-radius guard; retain Inf acceptance.</td></tr>
<tr><td>Finite nonnegative radius — R/calculate_covariates.R:5120–5123</td><td><pre><code class="language-r">!is.numeric(radius) || length(radius) != 1L || !is.finite(radius) || radius &lt; 0</code></pre></td><td><pre><code class="language-r">!checkmate::testNumber(radius, lower = 0, finite = TRUE)</code></pre></td><td>Preserve the existing finite, nonnegative scalar requirement.</td></tr>
<tr><td>Positive decay range — R/calculate_covariates.R:2100–2103</td><td><pre><code class="language-r">!is.numeric(decay_range) || length(decay_range) != 1L || is.na(decay_range) || decay_range &lt;= 0</code></pre></td><td><pre><code class="language-r">!checkmate::testNumber(decay_range, finite = FALSE) || decay_range &lt;= 0</code></pre></td><td>Retain strict positivity and positive Inf acceptance.</td></tr>
<tr><td>Required MCD14ML columns — R/process.R:692–694</td><td><pre><code class="language-r">!all(required_cols %in% names(txt_data))</code></pre></td><td><pre><code class="language-r">!checkmate::testSubset(required_cols, names(txt_data))</code></pre></td><td>Internally constructed character names; extra/reordered columns remain valid.</td></tr>
<tr><td>Required IMPROVE columns — R/calculate_covariates.R:5147–5149</td><td><pre><code class="language-r">!all(required %in% names(values))</code></pre></td><td><pre><code class="language-r">!checkmate::testSubset(required, names(values))</code></pre></td><td>Keep subset direction and the existing required-field error; do not constrain data values.</td></tr>
</tbody></table>
</div>

## Compatibility decisions for the team

- Retain `match.arg()` for defaults, unique partial matches and `several.ok = TRUE`.
- `assertChoice()` validates exact membership without equivalent selection or normalization.

- Retain `missing()` control flow: hourly defaults (`process.R:1912`) and HUC fallback discovery (`process.R:4273`).
- Keep mock failures, no-data handling, regex/date parsing, spatial checks, availability rules and incompatible-option errors as domain logic.

- Retain `check_geom()` (`calculate_covariates_auxiliary.R:998–1000`): Mixed logical/character choices need an explicit contract.
- `c(FALSE, "sf", "terra")` becomes character; `assertChoice()` would reject logical FALSE.
- Preserve existing character "FALSE" support and IMPROVE’s TRUE-to-"terra" normalization.

- Do not replace positional class checks in `check_mysftime()` and `check_mysf()` with broad inheritance checks without deciding whether subclasses and different class order are supported.
- Do not globally forbid missing values in data.frames or switch all date inputs to assertDate: existing APIs normalize character dates and retain missing environmental measurements.
- The extent ordering checks in `extent_to_modis_tiles()` already give useful xmin/ymin diagnostics and remain outside this prioritized table.

- Start database adoption with P1 selectors, schemas and scalar inputs.
- Validate parameters and materialized batches without collecting entire remote tables.
- Decide backend date, integer, missing-value and geometry mappings separately; retain ID and CRS checks.

# test_R

## Checkmate replacement suggestions

The two tables classify complete recommendation blocks by their actual dependency on production changes. The dependent table retains its existing diagnostic-impact column names and order; the standalone table retains the existing six-column layout (including Affected dataset). Earlier five-column standalone entries gain dataset metadata without altering their code. Diagnostic entries moved to the standalone table retain their impact text in Reason for replacement.

There are **4 dependent recommendations** and **38 standalone recommendations**. All 44 original entries are accounted for: two identical code pairs are consolidated across their GEOS and HMS line ranges, yielding 42 rows. No distinct test location, explanation or code alternative was discarded. The two unchanged gridMET/SPEI entries are retained as preservation recommendations, not new replacements.

## Test replacements dependent on proposed changes to the major R files

Implement these complete blocks only with the corresponding diagnostic-changing source proposals. The download URL, negative NLCD radius and TRI use_threshold regexes require new messages; the sum_edc block also contains a use_threshold regex that requires the proposed change. Adopting diagnostic-preserving test* predicates alone does not justify these edits. Existing provisional-regex caveats remain applicable.

<div class="table-scroll">
<table class="validation-table">
<thead><tr><th>Test path</th><th>Start–end lines</th><th>Existing expectation / impact</th><th>the existing code</th><th>suggested checkmate changes</th></tr></thead>
<tbody>
<tr><td><code>tests/testthat/test-download.R</code></td><td>163–182</td><td>Matches `No URLs provided` and `same length`; URL/destination vector assertions replace both diagnostics.</td><td><pre><code class="language-r">  testthat::expect_error(
    download_run_method(urls = NULL),
    &quot;No URLs provided&quot;
  )

  testthat::expect_error(
    download_run_method(
      urls = c(&quot;http://example.com&quot;),
      destfiles = c(&quot;file1.nc&quot;, &quot;file2.nc&quot;) # length mismatch
    ),
    &quot;same length&quot;
  )

  testthat::expect_error(
    download_run_method(
      urls = character(0),
      destfiles = character(0)
    ),
    &quot;No URLs provided&quot;
  )</code></pre></td><td><ul><li>Preserve calls and inputs; verify provisional regexes against the selected checkmate version.</li></ul><pre><code class="language-r">  testthat::expect_error(
    download_run_method(urls = NULL),
    &quot;urls.*(character|length)&quot;
  )

  testthat::expect_error(
    download_run_method(
      urls = c(&quot;http://example.com&quot;),
      destfiles = c(&quot;file1.nc&quot;, &quot;file2.nc&quot;) # length mismatch
    ),
    &quot;destfiles.*length&quot;
  )

  testthat::expect_error(
    download_run_method(
      urls = character(0),
      destfiles = character(0)
    ),
    &quot;urls.*(character|length)&quot;
  )</code></pre></td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>281–288; 298–305</td><td>Matches `radius is not a numeric.` and `radius has not a likely value.`; both change.</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      radius = &quot;1000&quot;
    ),
    &quot;radius is not a numeric.&quot;
  )</code></pre><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      radius = -3
    ),
    &quot;radius has not a likely value.&quot;
  )</code></pre></td><td><ul><li>Preserve calls and inputs; verify provisional regexes against the selected checkmate version.</li></ul><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      radius = &quot;1000&quot;
    ),
    &quot;radius.*(number|numeric)&quot;
  )

  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      radius = -3
    ),
    &quot;radius.*(&gt;=|lower|bound)&quot;
  )</code></pre></td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>842–850</td><td>Matches the use_threshold TRUE/FALSE sentence; assertFlag changes wording.</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      decay_range = 50000L,
      use_threshold = NA
    ),
    regexp = &quot;`use_threshold` must be TRUE or FALSE&quot;
  )</code></pre></td><td><ul><li>Preserve calls and inputs; verify provisional regexes against the selected checkmate version.</li></ul><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      decay_range = 50000L,
      use_threshold = NA
    ),
    regexp = &quot;use_threshold.*(logical|NA|missing)&quot;
  )</code></pre></td></tr>
<tr><td><code>tests/testthat/test-sedc.R</code></td><td>123–180</td><td>C0, target_fields, use_threshold and decay_range errors mostly have no regex; retained errors alone do not verify equivalent accepted inputs.</td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = &quot;NOT_A_COLUMN&quot;
    )
  )
  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = &quot;&quot;
    )
  )
  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = rep(1, nrow(tri_r) - 1)
    )
  )
  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = 1
    )
  )
  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      use_threshold = NA
    )
  )
  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = -10,
      target_fields = targcols[1]
    )
  )</code></pre></td><td><ul><li>Preserve calls and inputs; verify provisional regexes against the selected checkmate version.</li><li>The C0 membership case uses its public argument label; the string and vector cases add constraint matching.</li><li>Leave target_fields and decay_range cases unchanged: this review has not proposed replacing those guards.</li></ul><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = &quot;NOT_A_COLUMN&quot;
    ),
    regexp = &quot;C0&quot;
  )
  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = &quot;&quot;
    ),
    regexp = &quot;C0.*(character|length)&quot;
  )
  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = rep(1, nrow(tri_r) - 1)
    ),
    regexp = &quot;C0.*length&quot;
  )
  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = 1
    )
  )
  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      use_threshold = NA
    ),
    regexp = &quot;use_threshold.*(logical|NA|missing)&quot;
  )
  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = -10,
      target_fields = targcols[1]
    )
  )</code></pre></td></tr>
</tbody></table>
</div>

## Standalone unit and integration test replacements

These suggestions require no changes to the main R files. This includes diagnostic regexes that already match current source messages, even when their original explanations discussed future Checkmate adoption. Each such row explains its independent applicability. Preserve the existing compatibility caveats: independence does not remove the need to verify the proposed test-only changes. The fraction rows require only the proposed test helper shown below, and test-only adoption requires the documented Checkmate dependency.

<div class="table-scroll">
<table class="validation-table">
<thead><tr><th>File path</th><th>Affected dataset</th><th>Start–end lines</th><th>Current code</th><th>Suggested checkmate code</th><th>Reason for replacement</th></tr></thead>
<tbody>
<tr><td><code>tests/testthat/helper-skips.R</code></td><td>Shared live-test credential helper</td><td>32–32</td><td><pre><code class="language-r">  stopifnot(length(var) == 1L, is.character(var))</code></pre></td><td><pre><code class="language-r">checkmate::assert_character(var, len = 1L,
  any.missing = TRUE, typed.missing = TRUE)</code></pre></td><td><ul><li>Low priority — Optional test-helper input assertion: report an invalid environment-variable-name argument directly. any.missing=TRUE deliberately preserves the current guard&#x27;s acceptance of NA_character_; rejecting NA/empty names would be a separate, reasonable tightening. typed.missing=TRUE retains rejection of logical NA.</li><li>Keep Sys.getenv and testthat::skip below it so absent credentials remain skips, not failures.</li><li>Error text changes; no direct skip_if_no_credentials error-message expectation was found.</li></ul></td></tr>
<tr><td><code>tests/testthat/test-cropscape.R</code></td><td>CropScape/CDL</td><td>415–417</td><td><pre><code class="language-r">      testthat::expect_type(fractions, &quot;double&quot;)
      testthat::expect_false(anyNA(fractions))
      testthat::expect_true(all(fractions &gt;= 0 &amp; fractions &lt;= 1))</code></pre></td><td><pre><code class="language-r">checkmate::expect_double(fractions, lower = 0, upper = 1,
  any.missing = FALSE, typed.missing = TRUE)</code></pre></td><td><ul><li>P1 — Combine the existing double type, missingness and [0, 1] contract into one diagnostic expectation.</li><li>Keep the column-count check at 414 and sum/tolerance at 418.</li><li>Do not use expect_numeric (which admits integers), min.len or unique.</li><li>Expectation counts and failure text change; verify classed/dimensioned outputs before adoption.</li></ul></td></tr>
<tr><td><code>tests/testthat/test-ecoregion.R</code></td><td>Ecoregions</td><td>592–600</td><td><pre><code class="language-r">  testthat::expect_true(
    all(vapply(ecor_frac[, frc_cols, drop = FALSE], is.numeric, logical(1)))
  )
  testthat::expect_true(
    all(as.matrix(ecor_frac[, frc_cols, drop = FALSE]) &gt;= 0, na.rm = TRUE)
  )
  testthat::expect_true(
    all(as.matrix(ecor_frac[, frc_cols, drop = FALSE]) &lt;= 1, na.rm = TRUE)
  )</code></pre></td><td><pre><code class="language-r">expect_fraction_columns(ecor_frac, frc_cols,
  any.missing = TRUE, label = &quot;ecor_frac&quot;)</code></pre></td><td><ul><li>P1 — Use the proposed helper below to identify the failing fraction column and avoid repeated matrix conversion.</li><li>Preserve integer/double acceptance, empty numeric columns and missing/all-missing values.</li><li>Keep the independent minimum column count at 591.</li><li>Custom numeric classes/coercion methods need equivalence checks; expectation count and failure text change.</li></ul></td></tr>
<tr><td><code>tests/testthat/test-hms.R</code></td><td>HMS smoke</td><td>687–695</td><td><pre><code class="language-r">  testthat::expect_true(
    all(vapply(hms_frac[, smoke_cols], is.numeric, logical(1)))
  )
  testthat::expect_true(
    all(as.matrix(hms_frac[, smoke_cols]) &gt;= 0, na.rm = TRUE)
  )
  testthat::expect_true(
    all(as.matrix(hms_frac[, smoke_cols]) &lt;= 1, na.rm = TRUE)
  )</code></pre></td><td><pre><code class="language-r">expect_fraction_columns(hms_frac, smoke_cols,
  any.missing = TRUE, label = &quot;hms_frac&quot;)</code></pre></td><td><ul><li>P1 — Reuse the same fraction contract and column-labelled diagnostics.</li><li>Preserve the three-column check at 686 and allow numeric NA/NaN values as the current na.rm=TRUE checks do.</li><li>If the selection regresses to one column, the helper avoids the old dropping-subset failure; that diagnostic behavior deliberately changes.</li></ul></td></tr>
<tr><td><code>tests/testthat/test-download.R</code></td><td>Shared download helpers; MODIS tile lookup</td><td>2676–2679</td><td><pre><code class="language-r">  testthat::expect_true(is.character(tiles))
  testthat::expect_true(length(tiles) &gt; 0)
  testthat::expect_true(all(grepl(&quot;^h[0-9]{2}v[0-9]{2}$&quot;, tiles)))
  testthat::expect_true(length(unique(tiles)) &gt;= 10)</code></pre></td><td><pre><code class="language-r">checkmate::expect_character(tiles, min.len = 1L,
  any.missing = FALSE, typed.missing = TRUE,
  pattern = &quot;^h[0-9]{2}v[0-9]{2}$&quot;)
testthat::expect_gte(length(unique(tiles)), 10L)</code></pre></td><td><ul><li>P2 — Consolidate character type, nonempty vector and tile format while retaining the distinct-count requirement.</li><li>Duplicates remain allowed; unique=TRUE would change the contract.</li><li>Preserve regex anchors.</li><li>NA already fails the old regex test.</li><li>Expectation count and failure diagnostics change.</li></ul></td></tr>
<tr><td><code>tests/testthat/test-download.R</code></td><td>Shared download helpers; MODIS tile lookup</td><td>681–682</td><td><pre><code class="language-r">    testthat::expect_true(is.character(result))
    testthat::expect_true(nchar(result) &gt; 0)</code></pre></td><td><pre><code class="language-r">checkmate::expect_string(result, min.chars = 1L)</code></pre></td><td><ul><li>P2 — Express the existing scalar nonempty character hash contract directly.</li><li>The old expect_true(nchar(result) &gt; 0) already rejects missing, empty and multi-element results.</li><li>Do not add hexadecimal or fixed-length restrictions at this site.</li><li>Verify classed/dimensioned strings; diagnostics change.</li></ul></td></tr>
<tr><td><code>tests/testthat/test-download.R</code></td><td>Shared download helpers; MODIS tile lookup</td><td>733–735</td><td><pre><code class="language-r">    testthat::expect_type(result, &quot;character&quot;)
    testthat::expect_true(nchar(result) &gt; 0)
    testthat::expect_true(grepl(&quot;^[a-f0-9]+$&quot;, result)) # MD5 hash format</code></pre></td><td><pre><code class="language-r">checkmate::expect_string(result, min.chars = 1L,
  pattern = &quot;^[a-f0-9]+$&quot;)</code></pre></td><td><ul><li>P2 — Combine the existing scalar nonempty character and lowercase hexadecimal requirements.</li><li>A 32-character MD5 restriction is not currently tested and is not proposed.</li><li>Expectation counts and diagnostic text change; no material runtime gain is claimed.</li></ul></td></tr>
<tr><td><code>tests/testthat/helper-download-tests.R</code></td><td>Shared download tests</td><td>7–7</td><td><pre><code class="language-r">  testthat::expect_true(dir.exists(directory_to_save))</code></pre></td><td><pre><code class="language-r">testthat::expect_length(directory_to_save, 1L)
checkmate::expect_directory_exists(directory_to_save)</code></pre></td><td><ul><li>P2 — Improve missing-directory diagnostics in the shared helper.</li><li>Retain an explicit length-one expectation: directory existence validators can accept vectors, whereas the old expect_true(dir.exists(...)) requires a scalar result.</li><li>Intended inputs are character paths; the validator may reject coercible non-character inputs accepted by dir.exists.</li><li>Do not add access permissions.</li><li>Preserve file.exists(commands_path) at 8 and the separate all(url_status) requirement.</li></ul></td></tr>
<tr><td><code>tests/testthat/test-aqs.R</code></td><td>AQS</td><td>98–99</td><td><pre><code class="language-r">    testthat::expect_true(dir.exists(&quot;zip_files&quot;))
    testthat::expect_true(dir.exists(&quot;data_files&quot;))</code></pre></td><td><pre><code class="language-r">checkmate::expect_directory_exists(&quot;zip_files&quot;)
checkmate::expect_directory_exists(&quot;data_files&quot;)</code></pre></td><td><ul><li>P2 — Literal scalar directory paths need no additional length check.</li><li>Directory-specific errors are more informative than a failed TRUE comparison.</li><li>Preserve negative directory checks elsewhere and do not reinterpret base file.exists checks as regular-file-only checks.</li><li>Diagnostics change; filesystem work remains.</li></ul></td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>GEOS; MERRA-2; MODIS</td><td>90–101</td><td><pre><code class="language-r">    testthat::expect_error(
      get_geos_info(path = geos_path, include_file = &quot;yes&quot;),
      regexp = &quot;single logical value&quot;
    )
    testthat::expect_error(
      get_merra2_info(path = merra_path, include_file = NA),
      regexp = &quot;single logical value&quot;
    )
    testthat::expect_error(
      get_modis_info(path = modis_path, include_file = c(TRUE, FALSE)),
      regexp = &quot;single logical value&quot;
    )</code></pre></td><td><ul><li>Preserve calls and inputs; verify provisional regexes against the selected checkmate version.</li></ul><pre><code class="language-r">    testthat::expect_error(
      get_geos_info(path = geos_path, include_file = &quot;yes&quot;),
      regexp = &quot;include_file.*(logical|length|NA|missing)&quot;
    )
    testthat::expect_error(
      get_merra2_info(path = merra_path, include_file = NA),
      regexp = &quot;include_file.*(logical|length|NA|missing)&quot;
    )
    testthat::expect_error(
      get_modis_info(path = modis_path, include_file = c(TRUE, FALSE)),
      regexp = &quot;include_file.*(logical|length|NA|missing)&quot;
    )</code></pre></td><td>Matches `single logical value` for include_file; assertFlag changes wording.<p>Independent: the current include_file messages already contain the argument name and “logical”.</p></td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>GEOS; MERRA-2; MODIS</td><td>108–119</td><td><pre><code class="language-r">    testthat::expect_error(
      get_geos_info(path = 1),
      regexp = &quot;non-empty character vector&quot;
    )
    testthat::expect_error(
      get_merra2_info(path = NA_character_),
      regexp = &quot;non-empty character vector&quot;
    )
    testthat::expect_error(
      get_modis_info(path = list(&quot;.&quot;)),
      regexp = &quot;non-empty character vector&quot;
    )</code></pre></td><td><ul><li>Preserve calls and inputs; verify provisional regexes against the selected checkmate version.</li></ul><pre><code class="language-r">    testthat::expect_error(
      get_geos_info(path = 1),
      regexp = &quot;path.*(character|length|NA|missing)&quot;
    )
    testthat::expect_error(
      get_merra2_info(path = NA_character_),
      regexp = &quot;path.*(character|length|NA|missing)&quot;
    )
    testthat::expect_error(
      get_modis_info(path = list(&quot;.&quot;)),
      regexp = &quot;path.*(character|length|NA|missing)&quot;
    )</code></pre></td><td>Matches `non-empty character vector` for path; assertCharacter changes wording.<p>Independent: the current path message already contains “path” and “character”.</p></td></tr>
<tr><td><code>tests/testthat/test-goes.R</code></td><td>GOES</td><td>45–54</td><td><pre><code class="language-r">  testthat::expect_error(
    download_goes(
      date = c(&quot;2024-01-01&quot;, &quot;2024-01-01&quot;),
      satellite = &quot;99&quot;,
      product = &quot;ADP-C&quot;,
      directory_to_save = tempdir(),
      acknowledgement = TRUE
    ),
    regexp = &quot;satellite must be&quot;
  )</code></pre></td><td><ul><li>Preserve calls and inputs; verify provisional regexes against the selected checkmate version.</li></ul><pre><code class="language-r">  testthat::expect_error(
    download_goes(
      date = c(&quot;2024-01-01&quot;, &quot;2024-01-01&quot;),
      satellite = &quot;99&quot;,
      product = &quot;ADP-C&quot;,
      directory_to_save = tempdir(),
      acknowledgement = TRUE
    ),
    regexp = &quot;satellite.*(16|18)&quot;
  )</code></pre></td><td>Matches `satellite must be`; assertChoice changes wording/capitalization and quoting.<p>Independent: the current satellite message already lists 16 and 18.</p></td></tr>
<tr><td><code>tests/testthat/test-calc.R</code></td><td>Shared temporal-function validation</td><td>698–712</td><td><pre><code class="language-r">  testthat::expect_error(
    check_fun_temporal(42),
    regexp = &quot;single&quot;
  )
  testthat::expect_error(
    check_fun_temporal(c(&quot;mean&quot;, &quot;sum&quot;)),
    regexp = &quot;single&quot;
  )
})

testthat::test_that(&quot;check_fun_temporal errors on unknown function name&quot;, {
  testthat::expect_error(
    check_fun_temporal(&quot;variance&quot;),
    regexp = &quot;must be one of&quot;
  )</code></pre></td><td><ul><li>Preserve calls and inputs; verify provisional regexes against the selected checkmate version.</li></ul><pre><code class="language-r">  testthat::expect_error(
    check_fun_temporal(42),
    regexp = &quot;fun_temporal.*(string|character|length)&quot;
  )
  testthat::expect_error(
    check_fun_temporal(c(&quot;mean&quot;, &quot;sum&quot;)),
    regexp = &quot;fun_temporal.*(string|character|length)&quot;
  )
})

testthat::test_that(&quot;check_fun_temporal errors on unknown function name&quot;, {
  testthat::expect_error(
    check_fun_temporal(&quot;variance&quot;),
    regexp = &quot;fun_temporal.*(mean|median|sum|max|min)&quot;
  )</code></pre></td><td>Matches `single` and `must be one of` for fun_temporal; verify against assertString/assertChoice wording.<p>Independent: current type errors name fun_temporal and “character string”; the choice error already lists the allowed functions.</p></td></tr>
<tr><td><code>tests/testthat/test-gridmet.R</code></td><td>gridMET</td><td>264–278</td><td><pre><code class="language-r">  &quot;download_gridmet(acknowledgement=FALSE): stops before creating directories&quot;,
  {
    directory_to_save &lt;- file.path(withr::local_tempdir(), &quot;gridmet&quot;)

    testthat::expect_error(
      download_gridmet(
        variables = &quot;pr&quot;,
        year = 2018,
        directory_to_save = directory_to_save,
        acknowledgement = FALSE
      ),
      regexp = &quot;acknowledgement is set to FALSE&quot;
    )
    testthat::expect_false(dir.exists(directory_to_save))
  }</code></pre></td><td><ul><li>No change: the proposed assertFlag plus retained FALSE stop preserves this diagnostic.</li><li>Keep the package call and negative directory-existence expectation; do not replace the latter with a positive directory validator.</li></ul><pre><code class="language-r">  &quot;download_gridmet(acknowledgement=FALSE): stops before creating directories&quot;,
  {
    directory_to_save &lt;- file.path(withr::local_tempdir(), &quot;gridmet&quot;)

    testthat::expect_error(
      download_gridmet(
        variables = &quot;pr&quot;,
        year = 2018,
        directory_to_save = directory_to_save,
        acknowledgement = FALSE
      ),
      regexp = &quot;acknowledgement is set to FALSE&quot;
    )
    testthat::expect_false(dir.exists(directory_to_save))
  }</code></pre></td><td>Matches `acknowledgement is set to FALSE`; the proposed hybrid guard deliberately preserves it and the before-directory-creation behavior.<p>Independent preservation case: current and suggested code are identical; retain this test whether or not production validation changes.</p></td></tr>
<tr><td><code>tests/testthat/test-drought.R</code></td><td>Drought — SPEI</td><td>251–260</td><td><pre><code class="language-r">  testthat::expect_error(
    download_drought(
      source = &quot;spei&quot;,
      date = &quot;2020-06-01&quot;,
      timescale = -1L,
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE
    ),
    regexp = &quot;timescale&quot;
  )</code></pre></td><td><ul><li>No change required to the existing argument-name regex. assertInt runs in production after as.integer; retain this package call.</li><li>A future test may also match the lower-bound constraint once actual wording is verified.</li></ul><pre><code class="language-r">  testthat::expect_error(
    download_drought(
      source = &quot;spei&quot;,
      date = &quot;2020-06-01&quot;,
      timescale = -1L,
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE
    ),
    regexp = &quot;timescale&quot;
  )</code></pre></td><td>Matches `timescale`; expected to remain meaningful after assertInt, but still rerun.<p>Independent preservation case: current and suggested code are identical; the existing timescale diagnostic already matches.</p></td></tr>
<tr><td><code>tests/testthat/test-ecoregion.R</code></td><td>Ecoregions</td><td>296–311</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = &quot;site_id&quot;,
      frac = NA
    )
  )
  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = &quot;site_id&quot;,
      drop = NA
    )
  )</code></pre></td><td><ul><li>Preserve calls and inputs; verify provisional regexes against the selected checkmate version.</li></ul><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = &quot;site_id&quot;,
      frac = NA
    ),
    regexp = &quot;frac.*(logical|NA|missing)&quot;
  )
  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = &quot;site_id&quot;,
      drop = NA
    ),
    regexp = &quot;drop.*(logical|NA|missing)&quot;
  )</code></pre></td><td>Invalid frac/drop cases check only an error; verify valid FALSE as well as TRUE when implementing the shared flag pattern.<p>Independent: current frac/drop diagnostics already contain each argument name and “logical”.</p></td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>NLCD</td><td>202–209</td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = 1L),
    &quot;path is not a character&quot;
  )
  testthat::expect_error(
    process_nlcd(path = &quot;/universe/galaxy/solarsys/earth/usa.nc&quot;),
    &quot;path does not exist&quot;
  )</code></pre></td><td><ul><li>Preserve inputs and package calls. Proposed regexes are provisional; verify against the selected checkmate version.</li></ul><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = 1L),
    &quot;path.*(string|character)&quot;
  )
  testthat::expect_error(
    process_nlcd(path = &quot;/universe/galaxy/solarsys/earth/usa.nc&quot;),
    &quot;path.*(exist|directory)&quot;
  )</code></pre></td><td>NLCD path type/existence messages change with assertString and assertDirectoryExists.<p>Independent: current path diagnostics already match both proposed regexes.</p></td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>NLCD</td><td>360–371</td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = 3, year = 2),
    &quot;path is not a character.&quot;
  )
  # -- nlcd_path does not exist
  nice_sentence &lt;- &quot;That&#x27;s one small step for a man, a giant leap for mankind.&quot;
  testthat::expect_error(
    process_nlcd(
      path = nice_sentence
    ),
    &quot;path does not exist.&quot;
  )</code></pre></td><td><ul><li>Preserve inputs and package calls. Proposed regexes are provisional; verify against the selected checkmate version.</li></ul><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = 3, year = 2),
    &quot;path.*(string|character)&quot;
  )
  # -- nlcd_path does not exist
  nice_sentence &lt;- &quot;That&#x27;s one small step for a man, a giant leap for mankind.&quot;
  testthat::expect_error(
    process_nlcd(
      path = nice_sentence
    ),
    &quot;path.*(exist|directory)&quot;
  )</code></pre></td><td>NLCD path type/existence messages change with assertString and assertDirectoryExists.<p>Independent: current path diagnostics already match both proposed regexes.</p></td></tr>
<tr><td><code>tests/testthat/test-goes.R</code></td><td>GOES</td><td>978–985</td><td><pre><code class="language-r">    testthat::expect_error(
      process_goes(
        date = &quot;2018-01-01&quot;,
        variable = &quot;Smoke&quot;,
        path = c(existing_path, missing_path)
      ),
      regexp = &quot;Some GOES paths do not exist&quot;
    )</code></pre></td><td><ul><li>Preserve the existing/missing file scenario. Verify the provisional regex only if the production replacement is adopted.</li></ul><pre><code class="language-r">    testthat::expect_error(
      process_goes(
        date = &quot;2018-01-01&quot;,
        variable = &quot;Smoke&quot;,
        path = c(existing_path, missing_path)
      ),
      regexp = &quot;paths.*exist&quot;
    )</code></pre></td><td>GOES missing-path wording changes if assertFileExists replaces the contextual error.<p>Independent: the current “Some GOES paths do not exist” message already matches paths.*exist.</p></td></tr>
<tr><td><code>tests/testthat/test-by-summarize.R</code></td><td>Shared temporal summarization (synthetic data)</td><td>37–37</td><td><pre><code class="language-r">  testthat::expect_true(all(c(&quot;site_id&quot;, &quot;time&quot;, &quot;level&quot;, &quot;value&quot;) %in% names(out_day)))</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;site_id&quot;, &quot;time&quot;, &quot;level&quot;, &quot;value&quot;),<br>  choices = names(out_day)<br>)</code></pre></td><td>Checks the four required grouping/value columns. Retain the row-count and NULL-summary identity expectations.</td></tr>
<tr><td><code>tests/testthat/test-coverage-followup.R</code></td><td>MODIS fire points</td><td>401–401</td><td><pre><code class="language-r">  testthat::expect_true(all(c(&quot;fire_count_00000&quot;, &quot;frp_00000&quot;) %in% names(vector_result)))</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;fire_count_00000&quot;, &quot;frp_00000&quot;),<br>  choices = names(vector_result)<br>)</code></pre></td><td>Checks both fire output fields. Retain data.frame, fire-count, formatted-date and dates_dropped assertions.</td></tr>
<tr><td><code>tests/testthat/test-drought.R</code></td><td>Drought — USDM</td><td>114–114</td><td><pre><code class="language-r">  testthat::expect_true(all(c(&quot;DM&quot;, &quot;date&quot;, &quot;source&quot;) %in% names(usdm)))</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;DM&quot;, &quot;date&quot;, &quot;source&quot;),<br>  choices = names(usdm)<br>)</code></pre></td><td>Checks the processed weekly schema. Retain SpatVector, two-row, source-value and EPSG:4326 assertions.</td></tr>
<tr><td><code>tests/testthat/test-drought.R</code></td><td>Drought — USDM</td><td>2033–2033</td><td><pre><code class="language-r">    testthat::expect_true(all(c(&quot;DM&quot;, &quot;date&quot;, &quot;source&quot;) %in% names(processed)))</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;DM&quot;, &quot;date&quot;, &quot;source&quot;),<br>  choices = names(processed)<br>)</code></pre></td><td>Checks the processed multi-date schema. Retain spatial class, row count and exact dates.</td></tr>
<tr><td><code>tests/testthat/test-ecoregion.R</code></td><td>Ecoregions</td><td>429–431</td><td><pre><code class="language-r">  testthat::expect_true(<br>    all(c(&quot;NA_L2NAME&quot;, &quot;US_L3NAME&quot;, &quot;NA_L3NAME&quot;) %in% names(erras))<br>  )</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;NA_L2NAME&quot;, &quot;US_L3NAME&quot;, &quot;NA_L3NAME&quot;),<br>  choices = names(erras)<br>)</code></pre></td><td>Checks all three regional-name fields before exercising the missing-US_L3NAME fallback; preserve that subsequent scenario.</td></tr>
<tr><td><code>tests/testthat/test-geos.R</code></td><td>GEOS-CF</td><td>32–32; 64–64</td><td><pre><code class="language-r">    testthat::expect_true(all(c(&quot;success&quot;, &quot;failed&quot;, &quot;skipped&quot;) %in% names(result)))</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;success&quot;, &quot;failed&quot;, &quot;skipped&quot;),<br>  choices = names(result)<br>)</code></pre></td><td>Checks the download summary fields for a date range. Retain list type, positive processed total, credential skips and the download call.<br>Checks the download summary fields for a single date. Retain list type, positive processed total and the separate single-date scenario.</td></tr>
<tr><td><code>tests/testthat/test-hms.R</code></td><td>HMS smoke</td><td>300–302; 345–347</td><td><pre><code class="language-r">  testthat::expect_true(<br>    all(c(&quot;Density&quot;, &quot;Date&quot;) %in% names(hms))<br>  )</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;Density&quot;, &quot;Date&quot;),<br>  choices = names(hms)<br>)</code></pre></td><td>Checks Density and Date for a date range. Retain the two-column, spatial-class, CRS and cropping assertions.<br>Checks Density and Date for a single date. Retain the two-column, spatial-class, CRS and cropping assertions.</td></tr>
<tr><td><code>tests/testthat/test-hms.R</code></td><td>HMS smoke</td><td>1169–1171</td><td><pre><code class="language-r">    testthat::expect_true(<br>      all(c(&quot;Start&quot;, &quot;End&quot;, &quot;Density&quot;) %in% names(hms_raw))<br>    )</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;Start&quot;, &quot;End&quot;, &quot;Density&quot;),<br>  choices = names(hms_raw)<br>)</code></pre></td><td>Checks raw polygon metadata fields. Retain geometry validity, CRS, row count and the separate Density-value membership check.</td></tr>
<tr><td><code>tests/testthat/test-improve.R</code></td><td>IMPROVE</td><td>16–16</td><td><pre><code class="language-r">  testthat::expect_true(all(c(&quot;SiteCode&quot;, &quot;Latitude&quot;, &quot;Longitude&quot;, &quot;ProgramKey&quot;) %in% names(sites)))</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;SiteCode&quot;, &quot;Latitude&quot;, &quot;Longitude&quot;, &quot;ProgramKey&quot;),<br>  choices = names(sites)<br>)</code></pre></td><td>Checks built-in site metadata fields. Retain data.table class, site count, unique SiteCode and known-site/program assertions.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>GEOS-CF</td><td>11–11</td><td><pre><code class="language-r">    testthat::expect_true(all(c(&quot;collection&quot;, &quot;variable&quot;) %in% names(geos_info)))</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;collection&quot;, &quot;variable&quot;),<br>  choices = names(geos_info)<br>)</code></pre></td><td>Checks metadata column presence. Retain data.frame class and specific collection/variable-value assertions.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>GEOS-CF</td><td>17–17</td><td><pre><code class="language-r">    testthat::expect_true(all(c(&quot;collection&quot;, &quot;variable&quot;, &quot;file&quot;) %in% names(geos_info_file)))</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;collection&quot;, &quot;variable&quot;, &quot;file&quot;),<br>  choices = names(geos_info_file)<br>)</code></pre></td><td>Checks the include_file=TRUE schema. Retain the separate filename-pattern assertion.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>MERRA-2</td><td>29–29</td><td><pre><code class="language-r">    testthat::expect_true(all(c(&quot;collection&quot;, &quot;variable&quot;) %in% names(merra_info)))</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;collection&quot;, &quot;variable&quot;),<br>  choices = names(merra_info)<br>)</code></pre></td><td>Checks metadata column presence. Retain data.frame class and specific collection/variable-value assertions.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>MERRA-2</td><td>37–37</td><td><pre><code class="language-r">    testthat::expect_true(all(c(&quot;collection&quot;, &quot;variable&quot;, &quot;file&quot;) %in% names(merra_info_file)))</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;collection&quot;, &quot;variable&quot;, &quot;file&quot;),<br>  choices = names(merra_info_file)<br>)</code></pre></td><td>Checks the include_file=TRUE schema. Retain the separate MERRA-2/FWI filename-pattern assertion.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>MODIS</td><td>49–49</td><td><pre><code class="language-r">    testthat::expect_true(all(c(&quot;product&quot;, &quot;subdataset&quot;) %in% names(modis_info)))</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;product&quot;, &quot;subdataset&quot;),<br>  choices = names(modis_info)<br>)</code></pre></td><td>Checks product/subdataset metadata columns. Retain class and named-product/subdataset-value assertions.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>MODIS</td><td>58–58</td><td><pre><code class="language-r">    testthat::expect_true(all(c(&quot;product&quot;, &quot;subdataset&quot;, &quot;file&quot;) %in% names(modis_info_file)))</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;product&quot;, &quot;subdataset&quot;, &quot;file&quot;),<br>  choices = names(modis_info_file)<br>)</code></pre></td><td>Checks the include_file=TRUE schema. Retain the separate HDF/H5 extension assertion.</td></tr>
<tr><td><code>tests/testthat/test-prism.R</code></td><td>PRISM</td><td>363–363</td><td><pre><code class="language-r">  testthat::expect_true(all(c(&quot;ppt_1000&quot;, &quot;tmin_1000&quot;) %in% colnames(res)))</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;ppt_1000&quot;, &quot;tmin_1000&quot;),<br>  choices = colnames(res)<br>)</code></pre></td><td>Checks both radius-suffixed covariates. Keep colnames(res), the 1000-metre calculation and its warning handling unchanged.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>TRI</td><td>280–282</td><td><pre><code class="language-r">  testthat::expect_true(<br>    all(c(&quot;TRI_CHEMICAL_COMPOUND_ID&quot;, &quot;CHEMICAL&quot;, &quot;CASN&quot;) %in% names(chems))<br>  )</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;TRI_CHEMICAL_COMPOUND_ID&quot;, &quot;CHEMICAL&quot;, &quot;CASN&quot;),<br>  choices = names(chems)<br>)</code></pre></td><td>Checks chemical metadata fields. Retain data.frame class and exact BENZENE/CASN/filter assertions.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>TRI</td><td>291–293</td><td><pre><code class="language-r">  testthat::expect_true(<br>    all(c(&quot;INDUSTRY_SECTOR_CODE&quot;, &quot;INDUSTRY_SECTOR&quot;) %in% names(inds))<br>  )</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;INDUSTRY_SECTOR_CODE&quot;, &quot;INDUSTRY_SECTOR&quot;),<br>  choices = names(inds)<br>)</code></pre></td><td>Checks industry metadata fields. Retain data.frame class and expected sector-code/name assertions.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>TRI</td><td>590–592</td><td><pre><code class="language-r">  testthat::expect_true(<br>    all(c(&quot;STACK_AIR_100&quot;, &quot;STACK_AIR_200&quot;) %in% names(tri_multi_chem))<br>  )</code></pre></td><td><pre><code class="language-r">checkmate::expect_subset(<br>  c(&quot;STACK_AIR_100&quot;, &quot;STACK_AIR_200&quot;),<br>  choices = names(tri_multi_chem)<br>)</code></pre></td><td>Checks both requested chemical output columns. Retain the BENZENE/TOLUENE filtering inputs and STACK_AIR variable.</td></tr>
</tbody></table>
</div>

- This document proposes changes only; no test helper or test implementation has been created.


### Proposed shared fraction helper

- The two fraction-column rows above depend on this proposed test-only helper, suitable for a future `tests/testthat/helper-validation.R`.
- It is shown here only; the file has not been created.

```r
expect_fraction_columns <- function(data, columns, any.missing, label) {
  invisible(lapply(columns, function(column) {
    checkmate::expect_numeric(
      data[[column]],
      lower = 0,
      upper = 1,
      any.missing = any.missing,
      all.missing = TRUE,
      typed.missing = TRUE,
      label = paste0(label, "[[", dQuote(column), "]]")
    )
  }))
}
```

- Keep column selection/count and fraction-sum expectations at the call sites.
- An empty selection makes no expectations in this helper, so those count checks are essential.
- Do not add a helper-level nonempty assertion or force missingness to FALSE.
- Ordinary integer/double fraction columns retain their existing bounds contract; test reporter output and custom numeric classes still need verification.
- Direct `expect_numeric()` preserves the diagnostic reason that `expect_true(testNumeric(...))` would discard.

- The documented [double expectations](https://mllg.github.io/checkmate/reference/checkDouble.html), [numeric expectations](https://mllg.github.io/checkmate/reference/checkNumeric.html), [character expectations](https://mllg.github.io/checkmate/reference/checkCharacter.html), [string expectations](https://mllg.github.io/checkmate/reference/checkString.html), and [directory expectations](https://mllg.github.io/checkmate/reference/checkDirectoryExists.html) support these proposals.
- Fewer temporary vectors or matrix conversions may help, but no performance improvement has been measured.

### Suggestions intentionally not carried over

- The README's native class/count cleanup and shared live-download postcondition are useful independent maintenance ideas, but neither requires checkmate; they are outside this replacement table.
- Preserve native `expect_length()`, `expect_gt()`, S3/S4 checks and the distinction between at least one nonempty downloaded file and all files being nonempty.
- Do not add MD5 length restrictions, read/write permissions, strict nonmissing tokens, regular-file requirements or numeric checks where the old tests establish only bounds.
- Keep mock failures, skips, error expectations, exact results and binary set-membership tests.

- For a test-only adoption, declare checkmate in Suggests; if the major_R runtime recommendations are also implemented, Imports already supplies it and a duplicate Suggests entry is unnecessary.
- Select a minimum version supporting the proposed expectation arguments and verify compatibility with the project's testthat setup.
- Do not silently skip these core assertions when checkmate is absent.

## Diagnostic compatibility and implementation notes

- Preserve package calls, invalid inputs and enclosing test structure.
- Verify proposed error regexes against the selected checkmate version.
- Match argument names and constraints; avoid generic or unqualified error checks.
- Disjoint source excerpts follow the listed line-range order.


- Examples deliberately excluded from replacement are the simulated HTTP 404 in `test-drought.R:613`, missing-argument trap in `test-drought.R:1464`, network failures in `test-download.R`, and live-test rethrows.
- Skips and mock dispatch branches should retain their current meaning; checkmate would not improve them.

- If implementation is approved later, add focused contract cases using descriptions such as `"download_run_method(urls=NA_character_): rejects missing URL before HTTP"`.
- Cover NULL versus omission, empty and multi-element vectors, logical versus numeric flags, numeric versus character selectors, NA/NaN/Inf, whitespace-only strings, retained partial matches, and credential skips.
- In particular, preserve single-date duplication, Date/POSIXct inputs, numeric satellite normalization, timescale truncation, optional NULL arguments and missing numeric exposure values.
- These are recommendations only; no tests were added or edited.

## Review verification

- Original review scanned 71 R files and checked quoted snippets against source ranges.
- The optimization README and test-impact excerpts were reconciled; only this review document was edited.
- Rscript was unavailable because `libRblas.so` was missing; checks were static, with no R execution or HTML rendering.
- Before implementation, pin checkmate and run affected mocked tests, including classed-value and diagnostic compatibility checks.


## Additional behavior-preserving test recommendations (2026-10-13)

The compound required-name entries in the standalone table preserve the additional recommendations from this review. Each replacement remains one testthat expectation with the same pass/fail condition for the inspected column-name checks; failure wording becomes checkmate-specific. Required names are literal character vectors and `names()`/`colnames()` supply character names or NULL, so no mixed-type coercion is lost. Extra names, duplicate names and reordered names remain allowed; absent required names still fail. Do not reverse the subset arguments or replace these checks with exact-set equality.

The maintainer's [subset reference](https://mllg.github.io/checkmate/reference/checkSubset.html) documents `expect_subset()`, its testthat integration and type-matching rule. Also reviewed the maintainer references for [numeric vectors](https://mllg.github.io/checkmate/reference/checkNumeric.html), [character vectors](https://mllg.github.io/checkmate/reference/checkCharacter.html), [numbers](https://mllg.github.io/checkmate/reference/checkNumber.html), [strings](https://mllg.github.io/checkmate/reference/checkString.html), [flags](https://mllg.github.io/checkmate/reference/checkFlag.html), [choices](https://mllg.github.io/checkmate/reference/checkChoice.html), [vectors](https://mllg.github.io/checkmate/reference/checkVector.html), [integer scalars](https://mllg.github.io/checkmate/reference/checkInt.html) and [directories](https://mllg.github.io/checkmate/reference/checkDirectoryExists.html). No additional recommendations are made for those families where replacements would add constraints, weaken behavioral checks or duplicate earlier rows. Existing helpers were inspected; the credential guard and directory helper are already documented, and mock/skip behavior remains outside these proposals.

Paths, inclusive line ranges and current snippets for those entries were checked against repository files. Verification is static: Rscript cannot start because `libRblas.so` is missing. No runtime equivalence or test-suite execution is claimed. The earlier test-only dependency guidance still applies; no implementation or dependency files were changed.
