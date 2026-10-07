# checkmate validation review for planned SedonaDB and DuckDB integration

2026-10-07

`checkmate` can give user-facing functions consistent, argument-specific errors for types, lengths, missing values and allowed choices. At planned database-facing interfaces, the same contracts can reject malformed selectors and materialized input schemas before expensive ingestion or query execution. Assertions do not replace SQL parameterization, identifier quoting, connection validity checks, spatial validation or backend-specific type conversion.

This is a discussion proposal, not an implementation. Reviewed all **10 `.R` files in `R/` and 61 `.R` files in `tests/testthat/`**, including helpers and live tests. No SedonaDB, DuckDB or `DBI::` calls occur in those files; database implications here are prospective. `DESCRIPTION` currently lists none of checkmate, SedonaDB or DuckDB as dependencies. Adopting runtime assertions would require adding checkmate to Imports and selecting a supported version; no dependency change is made here.

Line ranges are one-based and inclusive in the working-tree snapshot reviewed on the date above. Every Current code cell is a verbatim contiguous repository excerpt, including indentation. Suggested cells replace just those ranges unless the reason says otherwise. **P1** marks shared interfaces and schema checks; **P2** marks smaller consistency improvements. Every replacement changes diagnostics unless expressly retained; test-output expectations also change the number and form of reported failures. Accepted-input notes describe the validation boundary, not a guarantee that downstream computation succeeds.

The snippets use documented checkmate APIs. Vector assertions explicitly select missing-value policies where relevant; `typed.missing = TRUE` avoids accepting empty/all-missing vectors of a different storage type. Scalar numbers retain infinity unless the current check already requires finiteness. See the maintainer references for [numeric vectors](https://mllg.github.io/checkmate/reference/checkNumeric.html), [character vectors](https://mllg.github.io/checkmate/reference/checkCharacter.html), [numbers](https://mllg.github.io/checkmate/reference/checkNumber.html), [strings](https://mllg.github.io/checkmate/reference/checkString.html), [flags](https://mllg.github.io/checkmate/reference/checkFlag.html), [choices](https://mllg.github.io/checkmate/reference/checkChoice.html), [subsets](https://mllg.github.io/checkmate/reference/checkSubset.html), [vectors](https://mllg.github.io/checkmate/reference/checkVector.html), [integer scalars](https://mllg.github.io/checkmate/reference/checkInt.html), and [directories](https://mllg.github.io/checkmate/reference/checkDirectoryExists.html).

<style>
.validation-table {width:100%; border-collapse:collapse; font-size:90%;}
.validation-table th, .validation-table td {border:1px solid #ddd; padding:8px; vertical-align:top;}
.validation-table pre {white-space:pre-wrap; overflow-wrap:anywhere; font-size:12px; min-width:220px;}
.validation-table td:last-child {min-width:250px;}
.table-scroll {overflow-x:auto;}
</style>

# major_R

The strongest candidates are below. Source excerpts and proposals are inert code blocks inside HTML tables, so viewing this document does not execute downloads or proposed assertions.

<div class="table-scroll">
<table class="validation-table">
<thead><tr><th>File path</th><th>Start–end lines</th><th>Current code</th><th>Suggested checkmate code</th><th>Reason for replacement</th></tr></thead>
<tbody>
<tr><td><code>R/download_auxiliary.R</code></td><td>204–211</td><td><pre><code class="language-r">    if (!acknowledgement) {
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
}</code></pre></td><td>P1 — Validate the flag before any side effects while retaining the useful consent explanation. Tightening: nonzero numeric values previously passed !acknowledgement; now only logical scalars pass. FALSE retains its existing message (test-gridmet.R:264–278 and test-download.R:1443–1446). Missing/NA/vector inputs get clearer errors. assertTRUE alone would discard the explanation.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>324–329</td><td><pre><code class="language-r">  if (is.null(urls) || length(urls) == 0) {
    stop(&quot;No URLs provided for download.\n&quot;)
  }
  if (is.null(destfiles) || length(destfiles) != length(urls)) {
    stop(&quot;destfiles must have same length as urls.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertCharacter(urls, min.len = 1L,
  any.missing = FALSE, min.chars = 1L)
checkmate::assertCharacter(destfiles, len = length(urls),
  any.missing = FALSE, min.chars = 1L)</code></pre></td><td>P1 — Give the downloader a consistent character-vector contract and diagnose which argument is wrong before HTTP/file work. Tightening: the old code checked only non-NULL/length, allowing non-character values, NA and empty strings. URLs are not validated for syntax or reachability. Messages change; test-download.R:163–182 matches &quot;No URLs provided&quot; and &quot;same length&quot;.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>750–755</td><td><pre><code class="language-r">  if (is.null(path) || !is.character(path) || length(path) &lt; 1 || anyNA(path)) {
    stop(&quot;`path` must be a non-empty character vector.\n&quot;)
  }
  if (!is.character(pattern) || length(pattern) != 1L || !nzchar(pattern)) {
    stop(&quot;`pattern` must be a single non-empty character string.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertCharacter(path, min.len = 1L,
  any.missing = FALSE)
checkmate::assertString(pattern, min.chars = 1L)</code></pre></td><td>P1 — Consolidate path-vector and scalar-pattern checks in info_resolve_paths. Preserve multiple files/directories, duplicates and empty path strings; do not add assertFileExists before directory expansion. Tightening: NA_character_ pattern currently passes nzchar(NA), but assertString rejects it. Messages change; test-info-helpers.R:108–119 is affected.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>815–821</td><td><pre><code class="language-r">  if (
    !is.logical(include_file) ||
      length(include_file) != 1L ||
      is.na(include_file)
  ) {
    stop(&quot;`include_file` must be a single logical value (TRUE/FALSE).\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertFlag(include_file)</code></pre></td><td>P1 — Replace the repeated logical/length/NA check with an explicit scalar contract. Also applies to lines 887–893 and 988–994. Ordinary accepted inputs stay TRUE/FALSE; error text changes and test-info-helpers.R:90–101 needs revision.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>267–275</td><td><pre><code class="language-r">    if (!is.logical(frac) || length(frac) != 1L || is.na(frac)) {
      stop(&quot;`frac` should be a single logical value (TRUE/FALSE).&quot;)
    }
    if (!is.numeric(radius) || length(radius) != 1L || is.na(radius)) {
      stop(&quot;`radius` should be a single numeric value.&quot;)
    }
    if (radius &lt; 0) {
      stop(&quot;`radius` should be greater than or equal to 0.&quot;)
    }</code></pre></td><td><pre><code class="language-r">checkmate::assertFlag(frac)
checkmate::assertNumber(radius, lower = 0, finite = FALSE)</code></pre></td><td>P1 — One consistent flag and nonnegative numeric scalar check. Retains acceptance of positive infinity at this validation boundary; making radius finite would be a separate input restriction (later integer conversion may already fail). Apply the same pattern to lines 1012–1020, with assertFlag(drop) for 1024–1026. Error text changes. Ecoregion invalid flags are tested without text at test-ecoregion.R:296–311.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>581–589</td><td><pre><code class="language-r">  if (!is.numeric(radius)) {
    stop(&quot;radius is not a numeric.&quot;)
  }
  if (radius &lt; 0) {
    stop(&quot;radius has not a likely value.&quot;)
  }
  if (!is.logical(drop) || length(drop) != 1 || is.na(drop)) {
    stop(&quot;`drop` should be a single logical value (TRUE/FALSE).&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertNumber(radius, lower = 0, finite = FALSE)
checkmate::assertFlag(drop)</code></pre></td><td>P1 — NLCD receives clear errors for empty, missing or multiple radii instead of if-condition errors. Preserve numeric integer/double scalars and positive infinity at the guard; reject character &quot;1000&quot; as before. Messages change: test-nlcd.R:281–288 and 298–305 match both old radius messages.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2107–2110</td><td><pre><code class="language-r">    if (!is.logical(use_threshold) || length(use_threshold) != 1L ||
          is.na(use_threshold)) {
      stop(&quot;`use_threshold` must be TRUE or FALSE.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">checkmate::assertFlag(use_threshold)</code></pre></td><td>P1 — Consolidate the scalar boolean contract in sum_edc. Repeat for calculate_tri at 2353–2356. Error text changes; sum_edc tests at test-sedc.R:162–171 check only an error, but test-tri.R:842–850 matches the full old message.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2124–2130</td><td><pre><code class="language-r">        if (length(c0_values) != 1L || anyNA(c0_values) ||
              !nzchar(trimws(c0_values))) {
          stop(&quot;`C0` as character must be a single non-empty column name.\n&quot;)
        }
        if (!c0_values %in% names(from)) {
          stop(&quot;`C0` column `&quot;, c0_values, &quot;` was not found in `from`.\n&quot;)
        }</code></pre></td><td><pre><code class="language-r">checkmate::assertString(c0_values)
checkmate::assertString(trimws(c0_values), min.chars = 1L,
  .var.name = &quot;C0&quot;)
checkmate::assertChoice(c0_values, names(from),
  .var.name = &quot;C0&quot;)</code></pre></td><td>P1 — In the existing is.character branch, separate scalar validity, whitespace-only rejection and column membership. Keep the original untrimmed name for lookup: do not assign trimws back to c0_values. Accepted ordinary strings and exact matching stay the same; errors now identify C0. Retain the surrounding NULL and one-column-data.frame handling. test-sedc.R:123–152 asserts errors without matching text.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2136–2141</td><td><pre><code class="language-r">      if (!is.numeric(c0_values) || length(c0_values) != nrow(from)) {
        stop(
          &quot;`C0` must be NULL, character(1) column name in `from`, &quot;,
          &quot;or a numeric vector with length `nrow(from)`.\n&quot;
        )
      }</code></pre></td><td><pre><code class="language-r">checkmate::assertNumeric(c0_values, len = nrow(from),
  any.missing = TRUE, finite = FALSE, typed.missing = TRUE,
  .var.name = &quot;C0&quot;)</code></pre></td><td>P1 — After existing C0 normalization, combine numeric type and row-count checks. Explicitly preserve missing numeric values and infinity; typed.missing prevents accidentally accepting logical NA vectors or empty logical vectors. No new positivity constraint. Error text changes; test-sedc.R:143–152 checks only an error.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1254–1259</td><td><pre><code class="language-r">  if (!locs_id %in% names(covar)) {
    stop(sprintf(&quot;`locs_id` column &#x27;%s&#x27; not found in `covar`.\n&quot;, locs_id))
  }
  if (!time_col %in% names(covar)) {
    stop(sprintf(&quot;`time_col` column &#x27;%s&#x27; not found in `covar`.\n&quot;, time_col))
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertString(locs_id)
checkmate::assertChoice(locs_id, names(covar))
checkmate::assertString(time_col)
checkmate::assertChoice(time_col, names(covar))</code></pre></td><td>P1 — Validate schema selectors before grouping; reuse in calc_summarize_native_time (1337–1342) and calc_summarize_temporal (1484–1495). Tightening: %in% previously coerced some numeric/factor selectors; require explicit character column names. Exact matching and existing empty-string column names remain allowed. Error messages change. These checks validate available columns, not SQL quoting or connection state.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1260–1270</td><td><pre><code class="language-r">  if (!is.null(group_cols_extra)) {
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
}</code></pre></td><td>P1 — A reusable optional grouping-column contract; retain NULL, character(0), duplicate selectors, and column order. Tightening: reject non-character selectors and NA even if malformed input happens to have an NA column name. Same family appears at 1343–1353 and 1496–1506. Error text changes; retain missing-column context through the argument name.</td></tr>
<tr><td><code>R/process.R</code></td><td>692–694</td><td><pre><code class="language-r">  if (!all(required_cols %in% names(txt_data))) {
    stop(&quot;MCD14ML input is missing one or more required columns.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertSubset(required_cols, names(txt_data),
  .var.name = &quot;required MCD14ML columns&quot;)</code></pre></td><td>P1 — Report which required columns are absent, while allowing extra fields and any order. Both operands already contain character names. Suitable for checking materialized import schemas before database ingestion. Error text changes; do not reverse the subset arguments.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5120–5123</td><td><pre><code class="language-r">  if (!is.numeric(radius) || length(radius) != 1L ||
      !is.finite(radius) || radius &lt; 0) {
    stop(&quot;`radius` must be a finite nonnegative number in meters.&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertNumber(radius, lower = 0, finite = TRUE)</code></pre></td><td>P1 — IMPROVE already requires a finite nonnegative numeric scalar, so this consolidates the existing contract without intentionally tightening it. Keep this distinct from radius guards that currently accept Inf. Message changes; test-improve.R:505–506 matches &quot;radius&quot; and should still identify the argument.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5124–5127</td><td><pre><code class="language-r">  if (!is.character(locs_id) || length(locs_id) != 1L ||
      anyNA(locs_id) || !locs_id %in% names(locs)) {
    stop(&quot;`locs_id` must name an identifier column in `locs`.&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertString(locs_id)
checkmate::assertChoice(locs_id, names(locs))</code></pre></td><td>P1 — Identify invalid selectors and missing columns separately before converting locations. Preserve the existing scalar character and exact membership requirement. Keep unique/nonmissing ID and reserved-output-name checks below this block: they protect joins and output schema. Error text changes.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5147–5152</td><td><pre><code class="language-r">  if (!all(required %in% names(values))) {
    stop(&quot;`from` must contain FactDate, ParamCode, and FactValue.&quot;)
  }
  if (!is.numeric(values$FactValue)) {
    stop(&quot;`FactValue` must be numeric.&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertSubset(required, names(values),
  .var.name = &quot;required IMPROVE columns&quot;)
checkmate::assertNumeric(values$FactValue,
  any.missing = TRUE, finite = FALSE, typed.missing = TRUE,
  .var.name = &quot;FactValue&quot;)</code></pre></td><td>P1 — Consolidate required-column and numeric-value contracts while preserving numeric NA/Inf and extra columns. Do not impose generic complete-case validation on exposure data. typed.missing retains strict type checking for all-missing/empty vectors. Error text changes.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5158–5160</td><td><pre><code class="language-r">  if (anyNA(parameters) || any(!nzchar(parameters))) {
    stop(&quot;`ParamCode` must contain nonempty, nonmissing parameter codes.&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertCharacter(parameters, min.chars = 1L,
  any.missing = FALSE, typed.missing = TRUE,
  .var.name = &quot;ParamCode&quot;)</code></pre></td><td>P1 — Combine missing-value and empty-code validation after the existing as.character conversion. Preserve character(0), whitespace-only codes and numeric-to-character normalization. Do not move the assertion before conversion. Error text changes.</td></tr>
<tr><td><code>R/process.R</code></td><td>1477–1484</td><td><pre><code class="language-r">  if (!is.character(variables) || length(variables) &lt; 1 || anyNA(variables)) {
    stop(
      &quot;`variables` must be a non-empty character vector of regex patterns.\n&quot;
    )
  }
  if (length(variables) &gt; 0 &amp;&amp; any(!nzchar(trimws(variables)))) {
    stop(&quot;`variables` cannot include empty patterns.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertCharacter(variables, min.len = 1L,
  any.missing = FALSE)
checkmate::assertCharacter(trimws(variables), min.chars = 1L,
  any.missing = FALSE, .var.name = &quot;variables&quot;)</code></pre></td><td>P2 — Collapse type/length/NA and whitespace checks without modifying the regex patterns used later. Empty vectors and whitespace-only patterns remain invalid; do not treat min.len as a per-string length check. Error text changes; regex syntax and matching are still separate concerns.</td></tr>
<tr><td><code>R/process.R</code></td><td>1485–1497</td><td><pre><code class="language-r">  if (!is.null(chemical)) {
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
checkmate::assertFlag(ignore_case)</code></pre></td><td>P2 — Preserve optional chemical filters and their original regex text, while making the flag contract consistent. NULL remains allowed but character(0) remains invalid. Messages change; no change to industry_group matching on the following line.</td></tr>
<tr><td><code>R/process.R</code></td><td>4719–4721</td><td><pre><code class="language-r">  if (is.null(path) || !dir.exists(path)) {
    stop(&quot;`path` must be a valid directory path.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertString(path)
checkmate::assertDirectoryExists(path)</code></pre></td><td>P2 — An existing-directory check is appropriate for process_improve, which reads a directory. Scalar checking must precede the filesystem check. No new readability/writability policy is proposed; no creation is performed. Errors become argument-specific. Do not copy this to download destinations that are created later or to .gdb inputs.</td></tr>
<tr><td><code>R/process.R</code></td><td>2366–2370</td><td><pre><code class="language-r">  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop(
      &quot;`path` must be a single file path to a .shp or .gdb roads file.&quot;
    )
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertString(path)</code></pre></td><td>P2 — Replace only the scalar character/NA guard for roads. Preserve file.exists and extension checks below it: a .gdb is a directory and must remain accepted. Empty strings still fail existence validation later. Messages change; do not substitute an ordinary-file-only assertion for the whole block.</td></tr>
<tr><td><code>R/download.R</code></td><td>4940–4943</td><td><pre><code class="language-r">  satellite &lt;- as.character(satellite)
  if (!satellite %in% c(&quot;16&quot;, &quot;18&quot;)) {
    stop(&quot;satellite must be &#x27;16&#x27; or &#x27;18&#x27;.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">satellite &lt;- as.character(satellite)
checkmate::assertChoice(satellite, c(&quot;16&quot;, &quot;18&quot;))</code></pre></td><td>P2 — A finite scalar choice gives a clearer error. Keep as.character so numeric 16/18 and factors continue to normalize as before. No partial matching is introduced. Errors change; test-goes.R:45–54 matches &quot;satellite must be&quot; and needs review.</td></tr>
<tr><td><code>R/download.R</code></td><td>5368–5370</td><td><pre><code class="language-r">    if (is.na(timescale) || timescale &lt; 1L) {
      stop(&quot;`timescale` must be a positive integer (months).\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">checkmate::assertInt(timescale, lower = 1L)</code></pre></td><td>P2 — Inside the existing source branch, validate the already-coerced timescale. Keep as.integer at 5366: moving the assertion ahead of it would reject numeric strings and fractional inputs that are currently truncated. Empty/vector/NA values get deliberate scalar errors. test-drought.R:251–260 matches &quot;timescale&quot;, which remains the argument name.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>657–659</td><td><pre><code class="language-r">    if (!is.numeric(year) || length(year) != 1 || is.na(year)) {
      stop(&quot;`year` must be NULL or a single numeric value.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">checkmate::assertNumber(year, finite = FALSE)</code></pre></td><td>P2 — Inside the existing !is.null(year) branch, replace the type/length/NA guard. Do not impose an integer year or historical range silently: the current guard accepts fractional years and infinity. Error text changes; NULL remains handled by the outer branch.</td></tr>
<tr><td><code>R/download.R</code></td><td>285–288</td><td><pre><code class="language-r">    if (length(year) == 1) {
      year &lt;- c(year, year)
    }
    stopifnot(length(year) == 2)</code></pre></td><td><pre><code class="language-r">if (length(year) == 1) {
  year &lt;- c(year, year)
}
checkmate::assertVector(year, len = 2L, strict = FALSE)</code></pre></td><td>P2 — Preserve endpoint normalization, then report the required length clearly instead of an expression failure. Tightening: assertVector excludes some non-vector objects that bare length accepts; confirm supported year classes before adoption. It intentionally adds no numeric/date parsing rule. Error text changes. Apply the analogous date pattern below rather than an integer-only check.</td></tr>
<tr><td><code>R/download.R</code></td><td>5351–5354</td><td><pre><code class="language-r">  if (length(date) == 1L) {
    date &lt;- c(date, date)
  }
  stopifnot(length(date) == 2L)</code></pre></td><td><pre><code class="language-r">if (length(date) == 1L) {
  date &lt;- c(date, date)
}
checkmate::assertVector(date, len = 2L, strict = FALSE)</code></pre></td><td>P2 — Consolidate repeated length guards while allowing classed Date/POSIXct vectors and character dates; strict=FALSE allows attributes. Scalar duplication must remain before the assertion. Tightening: non-vector objects with a length method may now fail; strict=FALSE still allows attributes and some structured vectors, so this is not a date-type validator. Date parsing, sorting and source availability checks stay separate. The same pattern occurs throughout process.R, e.g. 3125–3128. Error text changes.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>28–30</td><td><pre><code class="language-r">    if (!all(keyword %in% names(locs))) {
      stop(&quot;locs should have &#x27;lon&#x27;, &#x27;lat&#x27;, (and &#x27;time&#x27;) fields.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">checkmate::assertSubset(keyword, names(locs),
  .var.name = &quot;required location columns&quot;)</code></pre></td><td>P2 — Name the missing required location fields; keep keyword construction above the block so check_time continues to determine whether time is required. Extra fields remain accepted. Error text changes; no new spatial-class or CRS policy.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>85–87</td><td><pre><code class="language-r">  if (any(!(c(lonname, latname, timename) %in% colnames(x)))) {
    stop(&quot;Some of lon, lat, time columns missing or mispelled&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertSubset(c(lonname, latname, timename),
  colnames(x), .var.name = &quot;location/time columns&quot;)</code></pre></td><td>P2 — Replace a generic misspelled-column error with the missing selectors. Keep the preceding data.table-class check separate. Tightening: unlike %in%, the subset check rejects non-character selectors instead of coercing them. NULL selectors still drop out of c(); deciding to require three scalar strings is a separate contract change.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1025–1045</td><td><pre><code class="language-r">  if (
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
checkmate::assertChoice(fun_temporal, allowed)</code></pre></td><td>P2 — Replace repeated scalar-string and enumerated-choice validation, preserving the NULL return at 1021–1023 and invisible(NULL) at 1046. This is exact membership already, so partial names remain invalid. Error text changes; test-calc.R:698–712 matches &quot;single&quot; and &quot;must be one of&quot;. This is not a replacement for match.arg.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1169–1171</td><td><pre><code class="language-r">  if (!is.character(unit) || length(unit) != 1L || !unit %in% names(aliases)) {
    stop(&quot;`unit` must be one valid `.by_time` time-unit token.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertString(unit)
checkmate::assertChoice(unit, names(aliases))</code></pre></td><td>P2 — Consistent exact-token validation before aliases[[unit]] normalization. Preserve all alias names, including plurals. Reject NA with a direct argument error rather than relying on the membership test. Error messages change; output normalization remains untouched.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>1072–1072</td><td><pre><code class="language-r">  stopifnot(is.character(layer_names))</code></pre></td><td><pre><code class="language-r">checkmate::assertCharacter(layer_names,
  any.missing = TRUE, typed.missing = TRUE)</code></pre></td><td>P2 — Replace the opaque type assertion at the layer-name parsing boundary while keeping the existing source-specific parse error below it. Preserve character NA and character(0) at this guard; typed.missing avoids accepting all-missing or empty vectors of other types. Error text changes.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>406–411</td><td><pre><code class="language-r">  if (!is.numeric(terra::values(weights_obj)[, 1])) {
    stop(&quot;`weights` raster values must be numeric.&quot;)
  }
  if (any(terra::values(weights_obj)[, 1] &lt; 0, na.rm = TRUE)) {
    stop(&quot;`weights` values must be non-negative.&quot;)
  }</code></pre></td><td><pre><code class="language-r">checkmate::assertNumeric(terra::values(weights_obj)[, 1],
  lower = 0, any.missing = TRUE, finite = FALSE,
  typed.missing = TRUE, .var.name = &quot;weights raster values&quot;)</code></pre></td><td>P2 — Combine numeric and nonnegative checks while preserving NA cells and positive infinity. Keep raster class, single-layer, CRS and overlap checks as domain validations. Error text changes; this is not a proposal to scan a future lazy database relation eagerly.</td></tr>
</tbody></table>
</div>

## Compatibility decisions for the team

Retain `match.arg()` in this proposal. For example, `download.R:126` and `calculate_covariates.R:124` select from defaults and support unique partial matches; `download.R:636` explicitly uses `several.ok = TRUE`. `assertChoice()` merely validates exact membership and does not return the same normalized selection or handle an omitted argument's default choice vector in the same way. No reviewed call offers a sufficiently clear benefit to justify a replacement while preserving all that behavior. Existing exact-membership helpers above are different cases.

Retain both production uses of `missing()`: `process.R:1912` selects an hourly default when data_field is omitted, and `process.R:4273` triggers HUC fallback discovery when path is omitted or absent. These are control flow, not mandatory-argument assertions. Likewise, leave mock failure injection, no-data messages, regex selection failures, date parsing, CRS/geometry/overlap checks, source-specific availability rules and incompatible-option errors as domain logic.

Do not mechanically replace `check_geom()` (`calculate_covariates_auxiliary.R:998–1000`) with `assertChoice(geom, c(FALSE, "sf", "terra"))`: R coerces that choices vector to character, but checkmate checks compatible types, so logical FALSE would be rejected. Existing `%in%` also accepts character "FALSE". A strict mixed-type contract would need an explicit decision, and IMPROVE separately normalizes TRUE to "terra". No replacement is proposed here.

Do not replace positional class checks in `check_mysftime()` and `check_mysf()` with broad inheritance checks without deciding whether subclasses and different class order are supported. Do not globally forbid missing values in data.frames or switch all date inputs to assertDate: existing APIs normalize character dates and retain missing environmental measurements. The extent ordering checks in `extent_to_modis_tiles()` already give useful xmin/ymin diagnostics and remain outside this prioritized table.

For database adoption, start with selector, schema and scalar contracts in P1. Apply them to user parameters and already-materialized batches, not by collecting entire remote tables merely to validate them. Decide backend mappings for dates, integer widths, missing values and geometry separately. Assertions on names do not establish uniqueness of identifier values or preserve CRS; existing ID and spatial checks must remain.

# test_R

The test suite mainly contains output expectations and deliberate failure simulations, rather than input guards. The recommendations below now also include useful compound output expectations from [CHECKMATE_OPTIMIZATION_README_2.md](CHECKMATE_OPTIMIZATION_README_2.md), checked against the current repository. That is the on-disk file corresponding to the requested optimization README2. Use checkmate `expect_*()` for package outputs and reserve `assert_*()` for helper-input validation. Keep well-chosen native testthat expectations; assertions inside result tests can abort a test early.

The existing optional credential-helper recommendation is retained below as lower priority. Eight additional source blocks, spanning six recommendation groups, follow it. This document proposes changes only; no test helper or test implementation has been created.

<div class="table-scroll">
<table class="validation-table">
<thead><tr><th>File path</th><th>Start–end lines</th><th>Current code</th><th>Suggested checkmate code</th><th>Reason for replacement</th></tr></thead>
<tbody>
<tr><td><code>tests/testthat/helper-skips.R</code></td><td>32–32</td><td><pre><code class="language-r">  stopifnot(length(var) == 1L, is.character(var))</code></pre></td><td><pre><code class="language-r">checkmate::assert_character(var, len = 1L,
  any.missing = TRUE, typed.missing = TRUE)</code></pre></td><td>Low priority — Optional test-helper input assertion: report an invalid environment-variable-name argument directly. any.missing=TRUE deliberately preserves the current guard&#x27;s acceptance of NA_character_; rejecting NA/empty names would be a separate, reasonable tightening. typed.missing=TRUE retains rejection of logical NA. Keep Sys.getenv and testthat::skip below it so absent credentials remain skips, not failures. Error text changes; no direct skip_if_no_credentials error-message expectation was found.</td></tr>
</tbody></table>
</div>


## Useful compound output expectations from the optimization review

<div class="table-scroll">
<table class="validation-table">
<thead><tr><th>File path</th><th>Start–end lines</th><th>Current code</th><th>Suggested checkmate code</th><th>Reason for replacement</th></tr></thead>
<tbody>
<tr><td><code>tests/testthat/test-cropscape.R</code></td><td>415–417</td><td><pre><code class="language-r">      testthat::expect_type(fractions, &quot;double&quot;)
      testthat::expect_false(anyNA(fractions))
      testthat::expect_true(all(fractions &gt;= 0 &amp; fractions &lt;= 1))</code></pre></td><td><pre><code class="language-r">checkmate::expect_double(fractions, lower = 0, upper = 1,
  any.missing = FALSE, typed.missing = TRUE)</code></pre></td><td>P1 — Combine the existing double type, missingness and [0, 1] contract into one diagnostic expectation. Keep the column-count check at 414 and sum/tolerance at 418. Do not use expect_numeric (which admits integers), min.len or unique. Expectation counts and failure text change; verify classed/dimensioned outputs before adoption. No measured speedup is claimed.</td></tr>
<tr><td><code>tests/testthat/test-ecoregion.R</code></td><td>592–600</td><td><pre><code class="language-r">  testthat::expect_true(
    all(vapply(ecor_frac[, frc_cols, drop = FALSE], is.numeric, logical(1)))
  )
  testthat::expect_true(
    all(as.matrix(ecor_frac[, frc_cols, drop = FALSE]) &gt;= 0, na.rm = TRUE)
  )
  testthat::expect_true(
    all(as.matrix(ecor_frac[, frc_cols, drop = FALSE]) &lt;= 1, na.rm = TRUE)
  )</code></pre></td><td><pre><code class="language-r">expect_fraction_columns(ecor_frac, frc_cols,
  any.missing = TRUE, label = &quot;ecor_frac&quot;)</code></pre></td><td>P1 — Use the proposed helper below to identify the failing fraction column and avoid repeated matrix conversion. Preserve integer/double acceptance, empty numeric columns and missing/all-missing values. Keep the independent minimum column count at 591. Custom numeric classes/coercion methods need equivalence checks; expectation count and failure text change.</td></tr>
<tr><td><code>tests/testthat/test-hms.R</code></td><td>687–695</td><td><pre><code class="language-r">  testthat::expect_true(
    all(vapply(hms_frac[, smoke_cols], is.numeric, logical(1)))
  )
  testthat::expect_true(
    all(as.matrix(hms_frac[, smoke_cols]) &gt;= 0, na.rm = TRUE)
  )
  testthat::expect_true(
    all(as.matrix(hms_frac[, smoke_cols]) &lt;= 1, na.rm = TRUE)
  )</code></pre></td><td><pre><code class="language-r">expect_fraction_columns(hms_frac, smoke_cols,
  any.missing = TRUE, label = &quot;hms_frac&quot;)</code></pre></td><td>P1 — Reuse the same fraction contract and column-labelled diagnostics. Preserve the three-column check at 686 and allow numeric NA/NaN values as the current na.rm=TRUE checks do. If the selection regresses to one column, the helper avoids the old dropping-subset failure; that diagnostic behavior deliberately changes. No blanket nonmissing requirement or guaranteed speedup.</td></tr>
<tr><td><code>tests/testthat/test-download.R</code></td><td>2676–2679</td><td><pre><code class="language-r">  testthat::expect_true(is.character(tiles))
  testthat::expect_true(length(tiles) &gt; 0)
  testthat::expect_true(all(grepl(&quot;^h[0-9]{2}v[0-9]{2}$&quot;, tiles)))
  testthat::expect_true(length(unique(tiles)) &gt;= 10)</code></pre></td><td><pre><code class="language-r">checkmate::expect_character(tiles, min.len = 1L,
  any.missing = FALSE, typed.missing = TRUE,
  pattern = &quot;^h[0-9]{2}v[0-9]{2}$&quot;)
testthat::expect_gte(length(unique(tiles)), 10L)</code></pre></td><td>P2 — Consolidate character type, nonempty vector and tile format while retaining the distinct-count requirement. Duplicates remain allowed; unique=TRUE would change the contract. Preserve regex anchors. NA already fails the old regex test. Expectation count and failure diagnostics change.</td></tr>
<tr><td><code>tests/testthat/test-download.R</code></td><td>681–682</td><td><pre><code class="language-r">    testthat::expect_true(is.character(result))
    testthat::expect_true(nchar(result) &gt; 0)</code></pre></td><td><pre><code class="language-r">checkmate::expect_string(result, min.chars = 1L)</code></pre></td><td>P2 — Express the existing scalar nonempty character hash contract directly. The old expect_true(nchar(result) &gt; 0) already rejects missing, empty and multi-element results. Do not add hexadecimal or fixed-length restrictions at this site. Verify classed/dimensioned strings; diagnostics change.</td></tr>
<tr><td><code>tests/testthat/test-download.R</code></td><td>733–735</td><td><pre><code class="language-r">    testthat::expect_type(result, &quot;character&quot;)
    testthat::expect_true(nchar(result) &gt; 0)
    testthat::expect_true(grepl(&quot;^[a-f0-9]+$&quot;, result)) # MD5 hash format</code></pre></td><td><pre><code class="language-r">checkmate::expect_string(result, min.chars = 1L,
  pattern = &quot;^[a-f0-9]+$&quot;)</code></pre></td><td>P2 — Combine the existing scalar nonempty character and lowercase hexadecimal requirements. A 32-character MD5 restriction is not currently tested and is not proposed. Expectation counts and diagnostic text change; no material runtime gain is claimed.</td></tr>
<tr><td><code>tests/testthat/helper-download-tests.R</code></td><td>7–7</td><td><pre><code class="language-r">  testthat::expect_true(dir.exists(directory_to_save))</code></pre></td><td><pre><code class="language-r">testthat::expect_length(directory_to_save, 1L)
checkmate::expect_directory_exists(directory_to_save)</code></pre></td><td>P2 — Improve missing-directory diagnostics in the shared helper. Retain an explicit length-one expectation: directory existence validators can accept vectors, whereas the old expect_true(dir.exists(...)) requires a scalar result. Intended inputs are character paths; the validator may reject coercible non-character inputs accepted by dir.exists. Do not add access permissions. Preserve file.exists(commands_path) at 8 and the separate all(url_status) requirement.</td></tr>
<tr><td><code>tests/testthat/test-aqs.R</code></td><td>98–99</td><td><pre><code class="language-r">    testthat::expect_true(dir.exists(&quot;zip_files&quot;))
    testthat::expect_true(dir.exists(&quot;data_files&quot;))</code></pre></td><td><pre><code class="language-r">checkmate::expect_directory_exists(&quot;zip_files&quot;)
checkmate::expect_directory_exists(&quot;data_files&quot;)</code></pre></td><td>P2 — Literal scalar directory paths need no additional length check. Directory-specific errors are more informative than a failed TRUE comparison. Preserve negative directory checks elsewhere and do not reinterpret base file.exists checks as regular-file-only checks. Diagnostics change; filesystem work remains.</td></tr>
</tbody></table>
</div>

### Proposed shared fraction helper

The two fraction-column rows above depend on this proposed test-only helper, suitable for a future `tests/testthat/helper-validation.R`. It is shown here only; the file has not been created.

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

Keep column selection/count and fraction-sum expectations at the call sites. An empty selection makes no expectations in this helper, so those count checks are essential. Do not add a helper-level nonempty assertion or force missingness to FALSE. Ordinary integer/double fraction columns retain their existing bounds contract; test reporter output and custom numeric classes still need verification. Direct `expect_numeric()` preserves the diagnostic reason that `expect_true(testNumeric(...))` would discard.

The documented [double expectations](https://mllg.github.io/checkmate/reference/checkDouble.html), [numeric expectations](https://mllg.github.io/checkmate/reference/checkNumeric.html), [character expectations](https://mllg.github.io/checkmate/reference/checkCharacter.html), [string expectations](https://mllg.github.io/checkmate/reference/checkString.html), and [directory expectations](https://mllg.github.io/checkmate/reference/checkDirectoryExists.html) support these proposals. Fewer temporary vectors or matrix conversions may help, but no performance improvement has been measured.

### Suggestions intentionally not carried over

The README's native class/count cleanup and shared live-download postcondition are useful independent maintenance ideas, but neither requires checkmate; they are outside this replacement table. Preserve native `expect_length()`, `expect_gt()`, S3/S4 checks and the distinction between at least one nonempty downloaded file and all files being nonempty. Do not add MD5 length restrictions, read/write permissions, strict nonmissing tokens, regular-file requirements or numeric checks where the old tests establish only bounds. Keep mock failures, skips, error expectations, exact results and binary set-membership tests.

For a test-only adoption, declare checkmate in Suggests; if the major_R runtime recommendations are also implemented, Imports already supplies it and a duplicate Suggests entry is unnecessary. Select a minimum version supporting the proposed expectation arguments and verify compatibility with the project's testthat setup. Do not silently skip these core assertions when checkmate is absent.

## Existing test expectations affected by major_R

These are test adaptations to the proposed major_R checkmate assertions, not replacements of package behavior tests with direct validator calls. The new columns quote every listed source range and show concrete proposed test code. For disjoint ranges, the existing-code blocks follow the listed order; their suggested replacements are shown together. Some listed ranges include enclosing test structure, which is preserved. When implementing a row above, keep the same invalid-input scenario and update its expected diagnostic only after observing the pinned checkmate version. Prefer matching the argument name plus the relevant constraint, not complete punctuation or generic "Assertion" text. Do not weaken all tests to unqualified expect_error().

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
  )</code></pre></td><td><p>Keep the package call and invalid input unchanged; revise only the error regex after implementing the corresponding major_R assertion. The constraint regex below is provisional until checked against the chosen checkmate version.</p><pre><code class="language-r">  testthat::expect_error(
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
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>90–101</td><td>Matches `single logical value` for include_file; assertFlag changes wording.</td><td><pre><code class="language-r">    testthat::expect_error(
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
    )</code></pre></td><td><p>Keep the package call and invalid input unchanged; revise only the error regex after implementing the corresponding major_R assertion. The constraint regex below is provisional until checked against the chosen checkmate version.</p><pre><code class="language-r">    testthat::expect_error(
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
    )</code></pre></td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>108–119</td><td>Matches `non-empty character vector` for path; assertCharacter changes wording.</td><td><pre><code class="language-r">    testthat::expect_error(
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
    )</code></pre></td><td><p>Keep the package call and invalid input unchanged; revise only the error regex after implementing the corresponding major_R assertion. The constraint regex below is provisional until checked against the chosen checkmate version.</p><pre><code class="language-r">    testthat::expect_error(
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
  )</code></pre></td><td><p>Keep the package call and invalid input unchanged; revise only the error regex after implementing the corresponding major_R assertion. The constraint regex below is provisional until checked against the chosen checkmate version.</p><pre><code class="language-r">  testthat::expect_error(
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
  )</code></pre></td><td><p>Keep the package call and invalid input unchanged; revise only the error regex after implementing the corresponding major_R assertion. The constraint regex below is provisional until checked against the chosen checkmate version.</p><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      decay_range = 50000L,
      use_threshold = NA
    ),
    regexp = &quot;use_threshold.*(logical|NA|missing)&quot;
  )</code></pre></td></tr>
<tr><td><code>tests/testthat/test-goes.R</code></td><td>45–54</td><td>Matches `satellite must be`; assertChoice changes wording/capitalization and quoting.</td><td><pre><code class="language-r">  testthat::expect_error(
    download_goes(
      date = c(&quot;2024-01-01&quot;, &quot;2024-01-01&quot;),
      satellite = &quot;99&quot;,
      product = &quot;ADP-C&quot;,
      directory_to_save = tempdir(),
      acknowledgement = TRUE
    ),
    regexp = &quot;satellite must be&quot;
  )</code></pre></td><td><p>Keep the package call and invalid input unchanged; revise only the error regex after implementing the corresponding major_R assertion. The constraint regex below is provisional until checked against the chosen checkmate version.</p><pre><code class="language-r">  testthat::expect_error(
    download_goes(
      date = c(&quot;2024-01-01&quot;, &quot;2024-01-01&quot;),
      satellite = &quot;99&quot;,
      product = &quot;ADP-C&quot;,
      directory_to_save = tempdir(),
      acknowledgement = TRUE
    ),
    regexp = &quot;satellite.*(16|18)&quot;
  )</code></pre></td></tr>
<tr><td><code>tests/testthat/test-calc.R</code></td><td>698–712</td><td>Matches `single` and `must be one of` for fun_temporal; verify against assertString/assertChoice wording.</td><td><pre><code class="language-r">  testthat::expect_error(
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
  )</code></pre></td><td><p>Keep the package call and invalid input unchanged; revise only the error regex after implementing the corresponding major_R assertion. The constraint regex below is provisional until checked against the chosen checkmate version.</p><pre><code class="language-r">  testthat::expect_error(
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
  )</code></pre></td></tr>
<tr><td><code>tests/testthat/test-gridmet.R</code></td><td>264–278</td><td>Matches `acknowledgement is set to FALSE`; the proposed hybrid guard deliberately preserves it and the before-directory-creation behavior.</td><td><pre><code class="language-r">  &quot;download_gridmet(acknowledgement=FALSE): stops before creating directories&quot;,
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
  }</code></pre></td><td><p>No change: the proposed assertFlag plus retained FALSE stop preserves this diagnostic. Keep the package call and negative directory-existence expectation; do not replace the latter with a positive directory validator.</p><pre><code class="language-r">  &quot;download_gridmet(acknowledgement=FALSE): stops before creating directories&quot;,
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
  }</code></pre></td></tr>
<tr><td><code>tests/testthat/test-drought.R</code></td><td>251–260</td><td>Matches `timescale`; expected to remain meaningful after assertInt, but still rerun.</td><td><pre><code class="language-r">  testthat::expect_error(
    download_drought(
      source = &quot;spei&quot;,
      date = &quot;2020-06-01&quot;,
      timescale = -1L,
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE
    ),
    regexp = &quot;timescale&quot;
  )</code></pre></td><td><p>No change required to the existing argument-name regex. assertInt runs in production after as.integer; retain this package call. A future test may also match the lower-bound constraint once actual wording is verified.</p><pre><code class="language-r">  testthat::expect_error(
    download_drought(
      source = &quot;spei&quot;,
      date = &quot;2020-06-01&quot;,
      timescale = -1L,
      directory_to_save = withr::local_tempdir(),
      acknowledgement = TRUE
    ),
    regexp = &quot;timescale&quot;
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
  )</code></pre></td><td><p>Keep the package call and invalid input unchanged; revise only the error regex after implementing the corresponding major_R assertion. The constraint regex below is provisional until checked against the chosen checkmate version. The C0 membership case uses its public argument label; the string and vector cases add constraint matching. Leave target_fields and decay_range cases unchanged: this review has not proposed replacing those guards.</p><pre><code class="language-r">  testthat::expect_error(
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
<tr><td><code>tests/testthat/test-ecoregion.R</code></td><td>296–311</td><td>Invalid frac/drop cases check only an error; verify valid FALSE as well as TRUE when implementing the shared flag pattern.</td><td><pre><code class="language-r">  testthat::expect_error(
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
  )</code></pre></td><td><p>Keep the package call and invalid input unchanged; revise only the error regex after implementing the corresponding major_R assertion. The constraint regex below is provisional until checked against the chosen checkmate version.</p><pre><code class="language-r">  testthat::expect_error(
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
  )</code></pre></td></tr>
</tbody></table>
</div>


Examples deliberately excluded from replacement are the simulated HTTP 404 in `test-drought.R:613`, missing-argument trap in `test-drought.R:1464`, network failures in `test-download.R`, and live-test rethrows. Skips and mock dispatch branches should retain their current meaning; checkmate would not improve them.

If implementation is approved later, add focused contract cases using descriptions such as `"download_run_method(urls=NA_character_): rejects missing URL before HTTP"`. Cover NULL versus omission, empty and multi-element vectors, logical versus numeric flags, numeric versus character selectors, NA/NaN/Inf, whitespace-only strings, retained partial matches, and credential skips. In particular, preserve single-date duplication, Date/POSIXct inputs, numeric satellite normalization, timescale truncation, optional NULL arguments and missing numeric exposure values. These are recommendations only; no tests were added or edited.

## Review verification

Repository snippets were extracted directly by their line ranges and checked against source text. All 71 reviewed `.R` files were scanned for the requested constructs and surrounding validation/test context. The optimization README was reconciled against the current source blocks; all quoted excerpts, including the expanded test-impact table, were rechecked. Only this review document was edited in that reconciliation. No package code, tests, other reports or dependency declarations were edited. Rscript could not start in this environment because `libRblas.so` was unavailable, so snippets were checked statically against maintainer API documentation; neither R execution nor HTML rendering is claimed. Pinning checkmate and running the affected mocked tests is required before implementation, particularly for classed values and version-specific diagnostics.
