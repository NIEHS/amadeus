# Behavior-preserving checkmate integration review

2026-10-07

This is a static review of all **10 source files and 61 test/helper files** (`.R`) in `R/` and `tests/testthat/`, including helpers, live tests, wrapper dispatch and indirect callers. Only `checkmate_integration_reviw.md` is written. That is the exact requested filename; the pre-existing `checkmate_integration_review.md` is a separate file and remains unchanged. This review supersedes that earlier document's recommendations for the stricter compatibility requirement in this request.

**Recommendation:** use checkmate only where it expresses the existing contract. Prefer `test*()` inside existing `if (...) stop(...)` blocks when preserving errors matters. Retain normalization, fallback handling, filesystem actions, domain-specific messages and weak checks whose apparent replacements would introduce stricter input requirements. Do not add assertions merely because a condition contains `length()`, `is.*()` or `%in%`.

“Candidate — retain diagnostics” preserves the current error body, validation location and intended ordinary scalar/vector acceptance. It is a proposal, not a runtime-proven equivalence claim for arbitrary classed objects, custom length/is.na methods, matrices/arrays or encodings. Such inputs must be compared against a pinned checkmate version before implementation; if their behavior differs, keep the original code. “Optional — diagnostic change” is explicitly not exact behavior preservation and must be skipped if messages/condition details are part of the required contract. “Keep existing” supplies the unchanged code in the Suggested column deliberately; it is not a missing recommendation.

All source excerpts are contiguous, verbatim and line-numbered from the current snapshot. Suggested snippets replace only their stated range; surrounding branches and subsequent operations remain in place. No new checks are moved earlier across file reads, consent checks or directory creation. No performance benefit is claimed.

The earlier assertion-based process_tri hybrid preserved blank-pattern errors but changed the chemical-type and ignore_case errors. Under this request's stronger compatibility constraint, the preferred hybrid below retains **all** existing messages by using predicates. Consequently the two existing regex expectations do not need rewriting. Also, the earlier coverage statement was incomplete: `test-tri.R:460–467` already tests `variables = 49`; empty and missing vectors remain separate coverage opportunities.

<style>
.validation-table {width:100%; border-collapse:collapse; font-size:90%;}
.validation-table th,.validation-table td {border:1px solid #ddd;padding:8px;vertical-align:top;}
.validation-table pre {white-space:pre-wrap;overflow-wrap:anywhere;min-width:240px;}
.table-scroll {overflow-x:auto;}
</style>

# major_R

## Closest replacements, preserving existing diagnostics

The explicit missing-value policies matter: type-only checks must continue admitting typed empty/missing vectors, while nonmissing guards must continue rejecting them. `finite = FALSE` retains infinity where the original code allowed it. `typed.missing = TRUE` prevents an all-missing or empty vector of another type from passing. Predicates do not coerce or assign inputs. Refer to the maintainer documentation for [character checks](https://mllg.github.io/checkmate/reference/checkCharacter.html), [numeric checks](https://mllg.github.io/checkmate/reference/checkNumeric.html), [scalar numbers](https://mllg.github.io/checkmate/reference/checkNumber.html), [flags](https://mllg.github.io/checkmate/reference/checkFlag.html), [strings](https://mllg.github.io/checkmate/reference/checkString.html), [functions](https://mllg.github.io/checkmate/reference/checkFunction.html) and [subsets](https://mllg.github.io/checkmate/reference/checkSubset.html).

<div class="table-scroll"><table class="validation-table">
<thead><tr><th>File path</th><th>Start–end lines</th><th>Current Code</th><th>Suggested checkmate / retained code</th><th>Decision and compatibility reason</th></tr></thead><tbody>
<tr><td><code>R/calculate_covariates.R</code></td><td>267–269</td><td><pre><code class="language-r">    if (!is.logical(frac) || length(frac) != 1L || is.na(frac)) {
      stop(&quot;`frac` should be a single logical value (TRUE/FALSE).&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testFlag(frac)) {
      stop(&quot;`frac` should be a single logical value (TRUE/FALSE).&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Consolidates the existing logical/type/length/NA contract. Keep the same stop message, call setting, branch position and successful continuation. TRUE and FALSE remain valid; numeric 0/1, NA, NULL and vectors remain invalid. No test-message rewrite is intended.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>270–272</td><td><pre><code class="language-r">    if (!is.numeric(radius) || length(radius) != 1L || is.na(radius)) {
      stop(&quot;`radius` should be a single numeric value.&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testNumber(radius, finite = FALSE)) {
      stop(&quot;`radius` should be a single numeric value.&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Retain the separate negative-radius guard at 273–275 so invalid type and negative value keep different messages. Infinity remains accepted by this guard.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>581–583</td><td><pre><code class="language-r">  if (!is.numeric(radius)) {
    stop(&quot;radius is not a numeric.&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testNumeric(radius, any.missing = TRUE, typed.missing = TRUE, finite = FALSE)) {
    stop(&quot;radius is not a numeric.&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Type-only replacement: retain typed empty vectors and typed missing values; do not add length, bounds, existence or finiteness constraints. Keep the original stop and all later checks. For classed or dimensioned objects, this is conditional on differential verification; retain the original guard if acceptance differs.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>587–589</td><td><pre><code class="language-r">  if (!is.logical(drop) || length(drop) != 1 || is.na(drop)) {
    stop(&quot;`drop` should be a single logical value (TRUE/FALSE).&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testFlag(drop)) {
    stop(&quot;`drop` should be a single logical value (TRUE/FALSE).&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Consolidates the existing logical/type/length/NA contract. Keep the same stop message, call setting, branch position and successful continuation. TRUE and FALSE remain valid; numeric 0/1, NA, NULL and vectors remain invalid. No test-message rewrite is intended.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1012–1014</td><td><pre><code class="language-r">    if (!is.logical(frac) || length(frac) != 1L || is.na(frac)) {
      stop(&quot;`frac` should be a single logical value (TRUE/FALSE).&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testFlag(frac)) {
      stop(&quot;`frac` should be a single logical value (TRUE/FALSE).&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Consolidates the existing logical/type/length/NA contract. Keep the same stop message, call setting, branch position and successful continuation. TRUE and FALSE remain valid; numeric 0/1, NA, NULL and vectors remain invalid. No test-message rewrite is intended.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1015–1017</td><td><pre><code class="language-r">    if (!is.numeric(radius) || length(radius) != 1L || is.na(radius)) {
      stop(&quot;`radius` should be a single numeric value.&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testNumber(radius, finite = FALSE)) {
      stop(&quot;`radius` should be a single numeric value.&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Retain the separate negative-radius guard and subsequent rounding. Do not require finiteness.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1024–1026</td><td><pre><code class="language-r">    if (!is.logical(drop) || length(drop) != 1L || is.na(drop)) {
      stop(&quot;`drop` should be a single logical value (TRUE/FALSE).&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testFlag(drop)) {
      stop(&quot;`drop` should be a single logical value (TRUE/FALSE).&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Consolidates the existing logical/type/length/NA contract. Keep the same stop message, call setting, branch position and successful continuation. TRUE and FALSE remain valid; numeric 0/1, NA, NULL and vectors remain invalid. No test-message rewrite is intended.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1395–1397</td><td><pre><code class="language-r">      if (!is.null(from_secondary) &amp;&amp; !is.character(from_secondary)) {
        stop(&quot;from_secondary should be a character vector of file paths.\n&quot;)
      }</code></pre></td><td><pre><code class="language-r">      if (!is.null(from_secondary) &amp;&amp; !checkmate::testCharacter(from_secondary, any.missing = TRUE, typed.missing = TRUE)) {
        stop(&quot;from_secondary should be a character vector of file paths.\n&quot;)
      }</code></pre></td><td>Candidate — retain diagnostics — Keep inside from_is_character. Do not apply this to the SpatRaster branch or change the conditional relationship between inputs.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1410–1415</td><td><pre><code class="language-r">    if (from_is_character &amp;&amp; !is.function(preprocess)) {
      stop(
        &quot;preprocess should be one of process_modis_merge,
process_modis_swath, or process_blackmarble.&quot;
      )
    }</code></pre></td><td><pre><code class="language-r">    if (from_is_character &amp;&amp; !checkmate::testFunction(preprocess)) {
      stop(
        &quot;preprocess should be one of process_modis_merge,
process_modis_swath, or process_blackmarble.&quot;
      )
    }</code></pre></td><td>Candidate — retain diagnostics — Keep the character-input branch restriction and the existing explanatory error. Do not constrain formal argument names or arity.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2100–2103</td><td><pre><code class="language-r">    if (!is.numeric(decay_range) || length(decay_range) != 1L ||
          is.na(decay_range) || decay_range &lt;= 0) {
      stop(&quot;`decay_range` must be a single positive numeric value.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testNumber(decay_range, finite = FALSE) || decay_range &lt;= 0) {
      stop(&quot;`decay_range` must be a single positive numeric value.\n&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Keep the strict &gt;0 condition: lower=0 would admit zero, while a small positive lower bound would reject valid positive values. Positive infinity remains allowed.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2104–2106</td><td><pre><code class="language-r">    if (!is.character(target_fields) || length(target_fields) &lt; 1L) {
      stop(&quot;`target_fields` must be a non-empty character vector.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testCharacter(target_fields, min.len = 1L, any.missing = TRUE, typed.missing = TRUE)) {
      stop(&quot;`target_fields` must be a non-empty character vector.\n&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Preserve NA_character_ acceptance at this guard; subsequent missing-column detection remains unchanged. Do not add nonmissing or nonblank requirements.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2107–2110</td><td><pre><code class="language-r">    if (!is.logical(use_threshold) || length(use_threshold) != 1L ||
          is.na(use_threshold)) {
      stop(&quot;`use_threshold` must be TRUE or FALSE.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testFlag(use_threshold)) {
      stop(&quot;`use_threshold` must be TRUE or FALSE.\n&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Consolidates the existing logical/type/length/NA contract. Keep the same stop message, call setting, branch position and successful continuation. TRUE and FALSE remain valid; numeric 0/1, NA, NULL and vectors remain invalid. No test-message rewrite is intended.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2136–2141</td><td><pre><code class="language-r">      if (!is.numeric(c0_values) || length(c0_values) != nrow(from)) {
        stop(
          &quot;`C0` must be NULL, character(1) column name in `from`, &quot;,
          &quot;or a numeric vector with length `nrow(from)`.\n&quot;
        )
      }</code></pre></td><td><pre><code class="language-r">      if (!checkmate::testNumeric(c0_values, len = nrow(from), any.missing = TRUE, finite = FALSE, typed.missing = TRUE)) {
        stop(
          &quot;`C0` must be NULL, character(1) column name in `from`, &quot;,
          &quot;or a numeric vector with length `nrow(from)`.\n&quot;
        )
      }</code></pre></td><td>Candidate — retain diagnostics — Keep numeric missing values, infinity and empty numeric vectors when nrow(from)=0. Retain preceding column lookup and one-column-data.frame unwrapping; retain public C0 message.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2350–2352</td><td><pre><code class="language-r">  if (!is.numeric(decay_range)) {
    stop(&quot;`decay_range` should be numeric.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testNumeric(decay_range, any.missing = TRUE, typed.missing = TRUE, finite = FALSE)) {
    stop(&quot;`decay_range` should be numeric.\n&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Type-only replacement: retain typed empty vectors and typed missing values; do not add length, bounds, existence or finiteness constraints. Keep the original stop and all later checks. For classed or dimensioned objects, this is conditional on differential verification; retain the original guard if acceptance differs.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2353–2356</td><td><pre><code class="language-r">  if (!is.logical(use_threshold) || length(use_threshold) != 1L ||
        is.na(use_threshold)) {
    stop(&quot;`use_threshold` must be TRUE or FALSE.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testFlag(use_threshold)) {
    stop(&quot;`use_threshold` must be TRUE or FALSE.\n&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Consolidates the existing logical/type/length/NA contract. Keep the same stop message, call setting, branch position and successful continuation. TRUE and FALSE remain valid; numeric 0/1, NA, NULL and vectors remain invalid. No test-message rewrite is intended.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2424–2426</td><td><pre><code class="language-r">              if (!is.numeric(tri_col_c0)) {
                stop(&quot;TRI target field `&quot;, tri_col, &quot;` is not numeric.\n&quot;)
              }</code></pre></td><td><pre><code class="language-r">              if (!checkmate::testNumeric(tri_col_c0, any.missing = TRUE, typed.missing = TRUE, finite = FALSE)) {
                stop(&quot;TRI target field `&quot;, tri_col, &quot;` is not numeric.\n&quot;)
              }</code></pre></td><td>Candidate — retain diagnostics — Type-only replacement: retain typed empty vectors and typed missing values; do not add length, bounds, existence or finiteness constraints. Keep the original stop and all later checks. For classed or dimensioned objects, this is conditional on differential verification; retain the original guard if acceptance differs.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2609–2611</td><td><pre><code class="language-r">  if (!is.logical(frac) || length(frac) != 1L || is.na(frac)) {
    stop(&quot;`frac` should be a single logical value (TRUE/FALSE).&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testFlag(frac)) {
    stop(&quot;`frac` should be a single logical value (TRUE/FALSE).&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Consolidates the existing logical/type/length/NA contract. Keep the same stop message, call setting, branch position and successful continuation. TRUE and FALSE remain valid; numeric 0/1, NA, NULL and vectors remain invalid. No test-message rewrite is intended.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>3413–3415</td><td><pre><code class="language-r">  if (!is.logical(drop) || length(drop) != 1 || is.na(drop)) {
    stop(&quot;`drop` should be a single logical value (TRUE/FALSE).&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testFlag(drop)) {
    stop(&quot;`drop` should be a single logical value (TRUE/FALSE).&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Consolidates the existing logical/type/length/NA contract. Keep the same stop message, call setting, branch position and successful continuation. TRUE and FALSE remain valid; numeric 0/1, NA, NULL and vectors remain invalid. No test-message rewrite is intended.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>4275–4277</td><td><pre><code class="language-r">  if (!is.numeric(radius) || length(radius) != 1) {
    stop(&quot;`radius` must be numeric(1).&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testNumeric(radius, len = 1L, any.missing = TRUE, finite = FALSE, typed.missing = TRUE)) {
    stop(&quot;`radius` must be numeric(1).&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Important overlooked case: EDGAR checks numeric type and length only. Preserve numeric NA/NaN, Inf and negative values at this guard; testNumber with its default na.ok=FALSE would tighten it. Logical NA remains invalid.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5120–5123</td><td><pre><code class="language-r">  if (!is.numeric(radius) || length(radius) != 1L ||
      !is.finite(radius) || radius &lt; 0) {
    stop(&quot;`radius` must be a finite nonnegative number in meters.&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testNumber(radius, lower = 0, finite = TRUE)) {
    stop(&quot;`radius` must be a finite nonnegative number in meters.&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — IMPROVE already requires a finite, nonnegative scalar; preserve its meters-specific error.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5147–5149</td><td><pre><code class="language-r">  if (!all(required %in% names(values))) {
    stop(&quot;`from` must contain FactDate, ParamCode, and FactValue.&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testSubset(required, names(values))) {
    stop(&quot;`from` must contain FactDate, ParamCode, and FactValue.&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Both sides are internally established character names. Keep the required-field diagnostic; do not impose missingness restrictions on the data.frame values.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5150–5152</td><td><pre><code class="language-r">  if (!is.numeric(values$FactValue)) {
    stop(&quot;`FactValue` must be numeric.&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testNumeric(values$FactValue, any.missing = TRUE, typed.missing = TRUE, finite = FALSE)) {
    stop(&quot;`FactValue` must be numeric.&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Type-only replacement: retain typed empty vectors and typed missing values; do not add length, bounds, existence or finiteness constraints. Keep the original stop and all later checks. For classed or dimensioned objects, this is conditional on differential verification; retain the original guard if acceptance differs.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1025–1035</td><td><pre><code class="language-r">  if (
    !is.character(fun_temporal) ||
      length(fun_temporal) != 1L
  ) {
    stop(
      paste0(
        &quot;`fun_temporal` must be NULL or a single &quot;,
        &quot;character string.\n&quot;
      )
    )
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testCharacter(fun_temporal, len = 1L, any.missing = TRUE, typed.missing = TRUE)) {
    stop(
      paste0(
        &quot;`fun_temporal` must be NULL or a single &quot;,
        &quot;character string.\n&quot;
      )
    )
  }</code></pre></td><td>Candidate — retain diagnostics — Preserve the first-stage acceptance of NA_character_; the following membership guard rejects it with the existing choices message. Keep optional NULL early return and final invisible(NULL), so a validator does not change the helper return value.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1106–1114</td><td><pre><code class="language-r">  if (!is.character(.by_time) || length(.by_time) != 1L) {
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
  }</code></pre></td><td>Candidate — retain diagnostics — Preserve the first-stage acceptance of NA_character_; the following membership guard rejects it with the existing choices message. Keep optional NULL early return and final invisible(NULL), so a validator does not change the helper return value.</td></tr>
<tr><td><code>R/download.R</code></td><td>3125–3135</td><td><pre><code class="language-r">  if (
    !is.character(jurisdiction) ||
      length(jurisdiction) != 1 ||
      is.na(jurisdiction)
  ) {
    stop(
      &quot;`jurisdiction` must be a single character value such as &quot;,
      &quot;\&quot;US\&quot;, \&quot;AZ\&quot;, or \&quot;tbl\&quot;.\n&quot;,
      call. = FALSE
    )
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testString(jurisdiction)) {
    stop(
      &quot;`jurisdiction` must be a single character value such as &quot;,
      &quot;\&quot;US\&quot;, \&quot;AZ\&quot;, or \&quot;tbl\&quot;.\n&quot;,
      call. = FALSE
    )
  }</code></pre></td><td>Candidate — retain diagnostics — Preserve the original multiline error and call.=FALSE. Keep trimws, case normalization and US/state/tbl dispatch afterward. In particular, test-coverage-followup.R matches the original scalar-type message.</td></tr>
<tr><td><code>R/ignore.R</code></td><td>276–278</td><td><pre><code class="language-r">    if (!is.character(path) || is.null(path)) {
      stop(&quot;path is not a character.&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testCharacter(path, any.missing = TRUE, typed.missing = TRUE)) {
      stop(&quot;path is not a character.&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Type-only replacement: retain typed empty vectors and typed missing values; do not add length, bounds, existence or finiteness constraints. Keep the original stop and all later checks. For classed or dimensioned objects, this is conditional on differential verification; retain the original guard if acceptance differs.</td></tr>
<tr><td><code>R/process.R</code></td><td>347–355</td><td><pre><code class="language-r">      if (
        !is.character(subdataset) ||
          length(subdataset) != 1L ||
          is.na(subdataset) ||
          !nzchar(subdataset)
      ) {
        stop(&quot;Argument subdataset should be a non-empty character value.\n&quot;)
      }
      # we use var to get detailed information in subdatasets</code></pre></td><td><pre><code class="language-r">      if (!checkmate::testString(subdataset, min.chars = 1L)) {
        stop(&quot;Argument subdataset should be a non-empty character value.\n&quot;)
      }
      # we use var to get detailed information in subdatasets</code></pre></td><td>Candidate — retain diagnostics — Keep inside the non-HDF branch. Empty strings and NA were already rejected; whitespace-only strings remain accepted. Keep subdataset discovery and the curvilinear guard.</td></tr>
<tr><td><code>R/process.R</code></td><td>456–458</td><td><pre><code class="language-r">  if (!is.character(path)) {
    stop(&quot;Argument path should be a list of hdf files (character).\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testCharacter(path, any.missing = TRUE, typed.missing = TRUE)) {
    stop(&quot;Argument path should be a list of hdf files (character).\n&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Type-only replacement: retain typed empty vectors and typed missing values; do not add length, bounds, existence or finiteness constraints. Keep the original stop and all later checks. For classed or dimensioned objects, this is conditional on differential verification; retain the original guard if acceptance differs.</td></tr>
<tr><td><code>R/process.R</code></td><td>459–461</td><td><pre><code class="language-r">  if (!is.null(path_secondary) &amp;&amp; !is.character(path_secondary)) {
    stop(&quot;Argument path_secondary should be a list of hdf files (character).\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!is.null(path_secondary) &amp;&amp; !checkmate::testCharacter(path_secondary, any.missing = TRUE, typed.missing = TRUE)) {
    stop(&quot;Argument path_secondary should be a list of hdf files (character).\n&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Preserve optional NULL and type-only semantics; do not require at least one path or check existence here.</td></tr>
<tr><td><code>R/process.R</code></td><td>692–694</td><td><pre><code class="language-r">  if (!all(required_cols %in% names(txt_data))) {
    stop(&quot;MCD14ML input is missing one or more required columns.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testSubset(required_cols, names(txt_data))) {
    stop(&quot;MCD14ML input is missing one or more required columns.\n&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — required_cols is constructed as character names immediately above. Keep source-specific missing-column message and allow extra/reordered columns.</td></tr>
<tr><td><code>R/process.R</code></td><td>1199–1201</td><td><pre><code class="language-r">    if (!is.character(path) || is.null(path)) {
      stop(&quot;path is not a character.&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testCharacter(path, any.missing = TRUE, typed.missing = TRUE)) {
      stop(&quot;path is not a character.&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Type-only replacement: retain typed empty vectors and typed missing values; do not add length, bounds, existence or finiteness constraints. Keep the original stop and all later checks. For classed or dimensioned objects, this is conditional on differential verification; retain the original guard if acceptance differs.</td></tr>
<tr><td><code>R/process.R</code></td><td>1205–1207</td><td><pre><code class="language-r">    if (!is.numeric(year)) {
      stop(&quot;year is not a numeric.&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testNumeric(year, any.missing = TRUE, typed.missing = TRUE, finite = FALSE)) {
      stop(&quot;year is not a numeric.&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Type-only replacement: retain typed empty vectors and typed missing values; do not add length, bounds, existence or finiteness constraints. Keep the original stop and all later checks. For classed or dimensioned objects, this is conditional on differential verification; retain the original guard if acceptance differs.</td></tr>
<tr><td><code>R/process.R</code></td><td>1477–1481</td><td><pre><code class="language-r">  if (!is.character(variables) || length(variables) &lt; 1 || anyNA(variables)) {
    stop(
      &quot;`variables` must be a non-empty character vector of regex patterns.\n&quot;
    )
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testCharacter(variables, min.len = 1L, any.missing = FALSE, typed.missing = TRUE)) {
    stop(
      &quot;`variables` must be a non-empty character vector of regex patterns.\n&quot;
    )
  }</code></pre></td><td>Candidate — retain diagnostics — Retain the exact regex-pattern error and the following trimws blank-pattern check. No assignment of trimmed values, no regex syntax validation, and no change to ignore_case semantics.</td></tr>
<tr><td><code>R/process.R</code></td><td>1486–1488</td><td><pre><code class="language-r">    if (!is.character(chemical) || length(chemical) &lt; 1 || anyNA(chemical)) {
      stop(&quot;`chemical` must be NULL or a non-empty character vector.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testCharacter(chemical, min.len = 1L, any.missing = FALSE, typed.missing = TRUE)) {
      stop(&quot;`chemical` must be NULL or a non-empty character vector.\n&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Keep inside !is.null(chemical); preserve the original NULL-or-character message and blank-pattern guard. NULL remains optional; character(0) and NA remain invalid.</td></tr>
<tr><td><code>R/process.R</code></td><td>1493–1497</td><td><pre><code class="language-r">  if (
    !is.logical(ignore_case) || length(ignore_case) != 1 || is.na(ignore_case)
  ) {
    stop(&quot;`ignore_case` must be TRUE or FALSE.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testFlag(ignore_case)) {
    stop(&quot;`ignore_case` must be TRUE or FALSE.\n&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Consolidates the existing logical/type/length/NA contract. Keep the same stop message, call setting, branch position and successful continuation. TRUE and FALSE remain valid; numeric 0/1, NA, NULL and vectors remain invalid. No test-message rewrite is intended.</td></tr>
<tr><td><code>R/process.R</code></td><td>2366–2370</td><td><pre><code class="language-r">  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop(
      &quot;`path` must be a single file path to a .shp or .gdb roads file.&quot;
    )
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testString(path)) {
    stop(
      &quot;`path` must be a single file path to a .shp or .gdb roads file.&quot;
    )
  }</code></pre></td><td>Candidate — retain diagnostics — Keep file.exists and .shp/.gdb checks separately. Do not use a regular-file-only assertion because a .gdb may be a directory.</td></tr>
<tr><td><code>R/process.R</code></td><td>4132–4134</td><td><pre><code class="language-r">    if (!is.character(path) || is.null(path)) {
      stop(&quot;path is not a character.&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testCharacter(path, any.missing = TRUE, typed.missing = TRUE)) {
      stop(&quot;path is not a character.&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Type-only replacement: retain typed empty vectors and typed missing values; do not add length, bounds, existence or finiteness constraints. Keep the original stop and all later checks. For classed or dimensioned objects, this is conditional on differential verification; retain the original guard if acceptance differs.</td></tr>
<tr><td><code>R/process.R</code></td><td>4196–4198</td><td><pre><code class="language-r">    if (!is.character(path) || is.null(path)) {
      stop(&quot;path is not a character.&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testCharacter(path, any.missing = TRUE, typed.missing = TRUE)) {
      stop(&quot;path is not a character.&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Type-only replacement: retain typed empty vectors and typed missing values; do not add length, bounds, existence or finiteness constraints. Keep the original stop and all later checks. For classed or dimensioned objects, this is conditional on differential verification; retain the original guard if acceptance differs.</td></tr>
<tr><td><code>R/process.R</code></td><td>4199–4201</td><td><pre><code class="language-r">    if (!is.numeric(year)) {
      stop(&quot;year is not a numeric.&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testNumeric(year, any.missing = TRUE, typed.missing = TRUE, finite = FALSE)) {
      stop(&quot;year is not a numeric.&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Type-only replacement: retain typed empty vectors and typed missing values; do not add length, bounds, existence or finiteness constraints. Keep the original stop and all later checks. For classed or dimensioned objects, this is conditional on differential verification; retain the original guard if acceptance differs.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>28–30</td><td><pre><code class="language-r">    if (!all(keyword %in% names(locs))) {
      stop(&quot;locs should have &#x27;lon&#x27;, &#x27;lat&#x27;, (and &#x27;time&#x27;) fields.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testSubset(keyword, names(locs))) {
      stop(&quot;locs should have &#x27;lon&#x27;, &#x27;lat&#x27;, (and &#x27;time&#x27;) fields.\n&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — keyword is an internally constructed character vector; check_time still removes time beforehand. Retain the original location-field message and conversion behavior.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>657–659</td><td><pre><code class="language-r">    if (!is.numeric(year) || length(year) != 1 || is.na(year)) {
      stop(&quot;`year` must be NULL or a single numeric value.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!checkmate::testNumber(year, finite = FALSE)) {
      stop(&quot;`year` must be NULL or a single numeric value.\n&quot;)
    }</code></pre></td><td>Candidate — retain diagnostics — Keep inside the existing !is.null(year) branch, after tri_read_raw. Fractional years and infinities remain allowed here; do not impose an integer year range or move validation ahead of file reading.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>750–752</td><td><pre><code class="language-r">  if (is.null(path) || !is.character(path) || length(path) &lt; 1 || anyNA(path)) {
    stop(&quot;`path` must be a non-empty character vector.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testCharacter(path, min.len = 1L, any.missing = FALSE, typed.missing = TRUE)) {
    stop(&quot;`path` must be a non-empty character vector.\n&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Only replace the path guard. Empty path strings remain permitted at this stage. Keep directory expansion, file filtering and the pattern guard, which has different NA behavior.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>815–821</td><td><pre><code class="language-r">  if (
    !is.logical(include_file) ||
      length(include_file) != 1L ||
      is.na(include_file)
  ) {
    stop(&quot;`include_file` must be a single logical value (TRUE/FALSE).\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testFlag(include_file)) {
    stop(&quot;`include_file` must be a single logical value (TRUE/FALSE).\n&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Consolidates the existing logical/type/length/NA contract. Keep the same stop message, call setting, branch position and successful continuation. TRUE and FALSE remain valid; numeric 0/1, NA, NULL and vectors remain invalid. No test-message rewrite is intended.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>887–893</td><td><pre><code class="language-r">  if (
    !is.logical(include_file) ||
      length(include_file) != 1L ||
      is.na(include_file)
  ) {
    stop(&quot;`include_file` must be a single logical value (TRUE/FALSE).\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testFlag(include_file)) {
    stop(&quot;`include_file` must be a single logical value (TRUE/FALSE).\n&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Consolidates the existing logical/type/length/NA contract. Keep the same stop message, call setting, branch position and successful continuation. TRUE and FALSE remain valid; numeric 0/1, NA, NULL and vectors remain invalid. No test-message rewrite is intended.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>988–994</td><td><pre><code class="language-r">  if (
    !is.logical(include_file) ||
      length(include_file) != 1L ||
      is.na(include_file)
  ) {
    stop(&quot;`include_file` must be a single logical value (TRUE/FALSE).\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!checkmate::testFlag(include_file)) {
    stop(&quot;`include_file` must be a single logical value (TRUE/FALSE).\n&quot;)
  }</code></pre></td><td>Candidate — retain diagnostics — Consolidates the existing logical/type/length/NA contract. Keep the same stop message, call setting, branch position and successful continuation. TRUE and FALSE remain valid; numeric 0/1, NA, NULL and vectors remain invalid. No test-message rewrite is intended.</td></tr>
</tbody></table></div>

## Optional assertion substitutions

These preserve the intended type requirement but change failure diagnostics. They are not mandatory recommendations under an exact-behavior policy.

<div class="table-scroll"><table class="validation-table">
<thead><tr><th>File path</th><th>Start–end lines</th><th>Current Code</th><th>Suggested checkmate / retained code</th><th>Decision and compatibility reason</th></tr></thead><tbody>
<tr><td><code>R/process_auxiliary.R</code></td><td>1072–1072</td><td><pre><code class="language-r">  stopifnot(is.character(layer_names))</code></pre></td><td><pre><code class="language-r">  checkmate::assertCharacter(layer_names, any.missing = TRUE,
    typed.missing = TRUE)</code></pre></td><td>Optional — diagnostic change — Type-only check; retain character(0) and NA_character_. Keep all downstream time parsing. Unlike the preferred test* hybrids, this replaces a stopifnot diagnostic and is optional only if diagnostic changes are acceptable.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1502–1502</td><td><pre><code class="language-r">      stopifnot(is.character(scale))</code></pre></td><td><pre><code class="language-r">      checkmate::assertCharacter(scale, any.missing = TRUE,
        typed.missing = TRUE)</code></pre></td><td>Optional — diagnostic change — Keep within !is.null(scale). Do not require a single nonempty expression; the existing guard checks character type only. Failure wording changes.</td></tr>
</tbody></table></div>

## Reviewed cases to keep unchanged

The daily aggregation guards are included individually for GEOS, MERRA2 and GOES. All match.arg calls and length-only stopifnot sites are listed to avoid implying that unlisted repetitions should be migrated. Remaining source-specific stop/error paths are audited below.

<div class="table-scroll"><table class="validation-table">
<thead><tr><th>File path</th><th>Start–end lines</th><th>Current Code</th><th>Suggested checkmate / retained code</th><th>Decision and compatibility reason</th></tr></thead><tbody>
<tr><td><code>R/calculate_covariates.R</code></td><td>124–124</td><td><pre><code class="language-r">    covariate &lt;- match.arg(covariate)</code></pre></td><td><pre><code class="language-r">    covariate &lt;- match.arg(covariate)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>580–580</td><td><pre><code class="language-r">  mode &lt;- match.arg(mode)</code></pre></td><td><pre><code class="language-r">  mode &lt;- match.arg(mode)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1011–1011</td><td><pre><code class="language-r">    colnames &lt;- match.arg(colnames)</code></pre></td><td><pre><code class="language-r">    colnames &lt;- match.arg(colnames)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1382–1382</td><td><pre><code class="language-r">    fusion_method &lt;- match.arg(fusion_method)</code></pre></td><td><pre><code class="language-r">    fusion_method &lt;- match.arg(fusion_method)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2124–2130</td><td><pre><code class="language-r">        if (length(c0_values) != 1L || anyNA(c0_values) ||
              !nzchar(trimws(c0_values))) {
          stop(&quot;`C0` as character must be a single non-empty column name.\n&quot;)
        }
        if (!c0_values %in% names(from)) {
          stop(&quot;`C0` column `&quot;, c0_values, &quot;` was not found in `from`.\n&quot;)
        }</code></pre></td><td><pre><code class="language-r">        if (length(c0_values) != 1L || anyNA(c0_values) ||
              !nzchar(trimws(c0_values))) {
          stop(&quot;`C0` as character must be a single non-empty column name.\n&quot;)
        }
        if (!c0_values %in% names(from)) {
          stop(&quot;`C0` column `&quot;, c0_values, &quot;` was not found in `from`.\n&quot;)
        }</code></pre></td><td>Keep existing — no replacement — Retain the distinct C0 scalar/blank and missing-column messages and the untrimmed column lookup. A combined string/choice assertion changes which diagnostic is emitted.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2357–2363</td><td><pre><code class="language-r">  c0_input &lt;- C0
  if (!is.null(c0_input) &amp;&amp;
        (!is.character(c0_input) || length(c0_input) &lt; 1L ||
           anyNA(c0_input) ||
           any(!nzchar(trimws(c0_input))))) {
    stop(&quot;`C0` must be NULL or a non-empty character vector of column names.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  c0_input &lt;- C0
  if (!is.null(c0_input) &amp;&amp;
        (!is.character(c0_input) || length(c0_input) &lt; 1L ||
           anyNA(c0_input) ||
           any(!nzchar(trimws(c0_input))))) {
    stop(&quot;`C0` must be NULL or a non-empty character vector of column names.\n&quot;)
  }</code></pre></td><td>Keep existing — no replacement — Preserve the composite optional C0/whitespace requirement and its single public message. Splitting assertions changes diagnostic routing; no need to expand the code solely for checkmate.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>3926–3926</td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5124–5127</td><td><pre><code class="language-r">  if (!is.character(locs_id) || length(locs_id) != 1L ||
      anyNA(locs_id) || !locs_id %in% names(locs)) {
    stop(&quot;`locs_id` must name an identifier column in `locs`.&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!is.character(locs_id) || length(locs_id) != 1L ||
      anyNA(locs_id) || !locs_id %in% names(locs)) {
    stop(&quot;`locs_id` must name an identifier column in `locs`.&quot;)
  }</code></pre></td><td>Keep existing — no replacement — Keep explicit type plus membership and public identifier message. Do not replace with a subset check that can accept empty selections.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5158–5160</td><td><pre><code class="language-r">  if (anyNA(parameters) || any(!nzchar(parameters))) {
    stop(&quot;`ParamCode` must contain nonempty, nonmissing parameter codes.&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (anyNA(parameters) || any(!nzchar(parameters))) {
    stop(&quot;`ParamCode` must contain nonempty, nonmissing parameter codes.&quot;)
  }</code></pre></td><td>Keep existing — no replacement — Although parameters has been coerced to character, replacing nzchar with character counting can differ for unusual encodings. Keep the concise source-specific guard; no new whitespace rule.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>44–44</td><td><pre><code class="language-r">  stopifnot(length(time_index) &lt;= 1)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(time_index) &lt;= 1)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>48–48</td><td><pre><code class="language-r">  stopifnot(length(description_index) &lt;= 1)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(description_index) &lt;= 1)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>52–52</td><td><pre><code class="language-r">  stopifnot(length(geometry_index) &lt;= 1)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(geometry_index) &lt;= 1)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>58–58</td><td><pre><code class="language-r">  stopifnot(length(lat_index) &lt;= 1)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(lat_index) &lt;= 1)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>63–63</td><td><pre><code class="language-r">  stopifnot(length(lon_index) &lt;= 1)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(lon_index) &lt;= 1)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>67–67</td><td><pre><code class="language-r">  stopifnot(length(level_index) &lt;= 1)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(level_index) &lt;= 1)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>145–145</td><td><pre><code class="language-r">  stopifnot(length(names_return) == length(unique(names_return)))</code></pre></td><td><pre><code class="language-r">  stopifnot(length(names_return) == length(unique(names_return)))</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>406–411</td><td><pre><code class="language-r">  if (!is.numeric(terra::values(weights_obj)[, 1])) {
    stop(&quot;`weights` raster values must be numeric.&quot;)
  }
  if (any(terra::values(weights_obj)[, 1] &lt; 0, na.rm = TRUE)) {
    stop(&quot;`weights` values must be non-negative.&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!is.numeric(terra::values(weights_obj)[, 1])) {
    stop(&quot;`weights` raster values must be numeric.&quot;)
  }
  if (any(terra::values(weights_obj)[, 1] &lt; 0, na.rm = TRUE)) {
    stop(&quot;`weights` values must be non-negative.&quot;)
  }</code></pre></td><td>Keep existing — no replacement — Keep separate type and nonnegative-value messages. Combining into one assertNumeric loses these diagnostics and changes the number/order of terra::values calls. A single extraction is a separate optimization.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>732–732</td><td><pre><code class="language-r">  time_type &lt;- match.arg(time_type)</code></pre></td><td><pre><code class="language-r">  time_type &lt;- match.arg(time_type)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>998–1000</td><td><pre><code class="language-r">  if (!geom %in% c(FALSE, &quot;sf&quot;, &quot;terra&quot;)) {
    stop(&quot;`geom` must be one of FALSE, &#x27;sf&#x27;, or &#x27;terra&#x27;.&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!geom %in% c(FALSE, &quot;sf&quot;, &quot;terra&quot;)) {
    stop(&quot;`geom` must be one of FALSE, &#x27;sf&#x27;, or &#x27;terra&#x27;.&quot;)
  }</code></pre></td><td>Keep existing — no replacement — Mixed FALSE/sf/terra choices are coerced to character by c(). assertChoice with same-typed choices would reject logical FALSE. Preserve current mixed-type handling.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1169–1171</td><td><pre><code class="language-r">  if (!is.character(unit) || length(unit) != 1L || !unit %in% names(aliases)) {
    stop(&quot;`unit` must be one valid `.by_time` time-unit token.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!is.character(unit) || length(unit) != 1L || !unit %in% names(aliases)) {
    stop(&quot;`unit` must be one valid `.by_time` time-unit token.\n&quot;)
  }</code></pre></td><td>Keep existing — no replacement — Keep exact alias membership and return aliases[[unit]]. Replacing the helper tail with an assertion changes its returned normalized unit.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1254–1259</td><td><pre><code class="language-r">  if (!locs_id %in% names(covar)) {
    stop(sprintf(&quot;`locs_id` column &#x27;%s&#x27; not found in `covar`.\n&quot;, locs_id))
  }
  if (!time_col %in% names(covar)) {
    stop(sprintf(&quot;`time_col` column &#x27;%s&#x27; not found in `covar`.\n&quot;, time_col))
  }</code></pre></td><td><pre><code class="language-r">  if (!locs_id %in% names(covar)) {
    stop(sprintf(&quot;`locs_id` column &#x27;%s&#x27; not found in `covar`.\n&quot;, locs_id))
  }
  if (!time_col %in% names(covar)) {
    stop(sprintf(&quot;`time_col` column &#x27;%s&#x27; not found in `covar`.\n&quot;, time_col))
  }</code></pre></td><td>Keep existing — no replacement — Do not require character selectors where %in% currently coerces compatible values. Preserve existing selector messages and scalar if behavior.</td></tr>
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
  }</code></pre></td><td><pre><code class="language-r">  if (!is.null(group_cols_extra)) {
    missing_extra &lt;- setdiff(group_cols_extra, names(covar))
    if (length(missing_extra) &gt; 0L) {
      stop(
        sprintf(
          &quot;Grouping column(s) not found in `covar`: %s.\n&quot;,
          paste(missing_extra, collapse = &quot;, &quot;)
        )
      )
    }
  }</code></pre></td><td>Keep existing — no replacement — setdiff can coerce selectors and reports the actual missing names. assertCharacter/assertSubset would tighten the contract; retain optional NULL, duplicates and current diagnostic.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1337–1353</td><td><pre><code class="language-r">  if (!locs_id %in% names(covar)) {
    stop(sprintf(&quot;`locs_id` column &#x27;%s&#x27; not found in `covar`.\n&quot;, locs_id))
  }
  if (!time_col %in% names(covar)) {
    stop(sprintf(&quot;`time_col` column &#x27;%s&#x27; not found in `covar`.\n&quot;, time_col))
  }
  if (!is.null(group_cols_extra)) {
    missing_extra &lt;- setdiff(group_cols_extra, names(covar))
    if (length(missing_extra) &gt; 0L) {
      stop(
        sprintf(
          &quot;Grouping column(s) not found in `covar`: %s.\n&quot;,
          paste(missing_extra, collapse = &quot;, &quot;)
        )
      )
    }
  }</code></pre></td><td><pre><code class="language-r">  if (!locs_id %in% names(covar)) {
    stop(sprintf(&quot;`locs_id` column &#x27;%s&#x27; not found in `covar`.\n&quot;, locs_id))
  }
  if (!time_col %in% names(covar)) {
    stop(sprintf(&quot;`time_col` column &#x27;%s&#x27; not found in `covar`.\n&quot;, time_col))
  }
  if (!is.null(group_cols_extra)) {
    missing_extra &lt;- setdiff(group_cols_extra, names(covar))
    if (length(missing_extra) &gt; 0L) {
      stop(
        sprintf(
          &quot;Grouping column(s) not found in `covar`: %s.\n&quot;,
          paste(missing_extra, collapse = &quot;, &quot;)
        )
      )
    }
  }</code></pre></td><td>Keep existing — no replacement — Same selector/grouping compatibility issue in native-time summarization; keep these guards, not just the first occurrence.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1484–1506</td><td><pre><code class="language-r">  if (!locs_id %in% names(covar)) {
    stop(sprintf(
      &quot;`locs_id` column &#x27;%s&#x27; not found in `covar`.&quot;,
      locs_id
    ))
  }
  if (!time_col %in% names(covar)) {
    stop(sprintf(
      &quot;`time_col` column &#x27;%s&#x27; not found in `covar`.&quot;,
      time_col
    ))
  }
  if (!is.null(group_cols_extra)) {
    missing_extra &lt;- setdiff(group_cols_extra, names(covar))
    if (length(missing_extra) &gt; 0L) {
      stop(sprintf(
        paste0(
          &quot;Extra grouping column(s) not found in `covar`: %s.&quot;
        ),
        paste(missing_extra, collapse = &quot;, &quot;)
      ))
    }
  }</code></pre></td><td><pre><code class="language-r">  if (!locs_id %in% names(covar)) {
    stop(sprintf(
      &quot;`locs_id` column &#x27;%s&#x27; not found in `covar`.&quot;,
      locs_id
    ))
  }
  if (!time_col %in% names(covar)) {
    stop(sprintf(
      &quot;`time_col` column &#x27;%s&#x27; not found in `covar`.&quot;,
      time_col
    ))
  }
  if (!is.null(group_cols_extra)) {
    missing_extra &lt;- setdiff(group_cols_extra, names(covar))
    if (length(missing_extra) &gt; 0L) {
      stop(sprintf(
        paste0(
          &quot;Extra grouping column(s) not found in `covar`: %s.&quot;
        ),
        paste(missing_extra, collapse = &quot;, &quot;)
      ))
    }
  }</code></pre></td><td>Keep existing — no replacement — Same selector/grouping compatibility issue in temporal summarization. Preserve extra grouping diagnostics and NULL summary early return.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1507–1510</td><td><pre><code class="language-r">  time_bucket &lt;- match.arg(
    time_bucket,
    c(&quot;day&quot;, &quot;week&quot;, &quot;month&quot;, &quot;year&quot;)
  )</code></pre></td><td><pre><code class="language-r">  time_bucket &lt;- match.arg(
    time_bucket,
    c(&quot;day&quot;, &quot;week&quot;, &quot;month&quot;, &quot;year&quot;)
  )</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1772–1772</td><td><pre><code class="language-r">  mode &lt;- match.arg(mode)</code></pre></td><td><pre><code class="language-r">  mode &lt;- match.arg(mode)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>126–126</td><td><pre><code class="language-r">    dataset_name &lt;- match.arg(dataset_name)</code></pre></td><td><pre><code class="language-r">    dataset_name &lt;- match.arg(dataset_name)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>288–288</td><td><pre><code class="language-r">    stopifnot(length(year) == 2)</code></pre></td><td><pre><code class="language-r">    stopifnot(length(year) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/download.R</code></td><td>617–617</td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/download.R</code></td><td>636–636</td><td><pre><code class="language-r">  collection &lt;- match.arg(collection, several.ok = TRUE)</code></pre></td><td><pre><code class="language-r">  collection &lt;- match.arg(collection, several.ok = TRUE)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>674–676</td><td><pre><code class="language-r">    if (!dir.exists(download_folder)) {
      dir.create(download_folder, recursive = TRUE)
    }</code></pre></td><td><pre><code class="language-r">    if (!dir.exists(download_folder)) {
      dir.create(download_folder, recursive = TRUE)
    }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download.R</code></td><td>830–830</td><td><pre><code class="language-r">  statistic &lt;- match.arg(statistic)</code></pre></td><td><pre><code class="language-r">  statistic &lt;- match.arg(statistic)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>833–833</td><td><pre><code class="language-r">  resolution &lt;- match.arg(resolution)</code></pre></td><td><pre><code class="language-r">  resolution &lt;- match.arg(resolution)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>1028–1028</td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/download.R</code></td><td>1145–1147</td><td><pre><code class="language-r">      if (!dir.exists(download_folder)) {
        dir.create(download_folder, recursive = TRUE)
      }</code></pre></td><td><pre><code class="language-r">      if (!dir.exists(download_folder)) {
        dir.create(download_folder, recursive = TRUE)
      }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download.R</code></td><td>1277–1279</td><td><pre><code class="language-r">          if (!dir.exists(download_folder)) {
            dir.create(download_folder, recursive = TRUE)
          }</code></pre></td><td><pre><code class="language-r">          if (!dir.exists(download_folder)) {
            dir.create(download_folder, recursive = TRUE)
          }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download.R</code></td><td>1283–1285</td><td><pre><code class="language-r">          if (!dir.exists(download_folder_metadata)) {
            dir.create(download_folder_metadata, recursive = TRUE)
          }</code></pre></td><td><pre><code class="language-r">          if (!dir.exists(download_folder_metadata)) {
            dir.create(download_folder_metadata, recursive = TRUE)
          }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download.R</code></td><td>1544–1544</td><td><pre><code class="language-r">  stopifnot(length(year) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(year) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/download.R</code></td><td>1570–1578</td><td><pre><code class="language-r">  if (any(nchar(year[1]) != 4, nchar(year[2]) != 4)) {
    stop(&quot;years should be 4-digit integers.\n&quot;)
  }
  stopifnot(
    all(
      seq(year[1], year[2], 1) %in%
        seq(1979, as.numeric(substr(Sys.Date(), 1, 4)), 1)
    )
  )</code></pre></td><td><pre><code class="language-r">  if (any(nchar(year[1]) != 4, nchar(year[2]) != 4)) {
    stop(&quot;years should be 4-digit integers.\n&quot;)
  }
  stopifnot(
    all(
      seq(year[1], year[2], 1) %in%
        seq(1979, as.numeric(substr(Sys.Date(), 1, 4)), 1)
    )
  )</code></pre></td><td>Keep existing — no replacement — Sequence membership is not identical to validating integer endpoints. seq can coerce character endpoints and omit a fractional upper endpoint. Do not silently enforce integerish years or move this check before directory setup.</td></tr>
<tr><td><code>R/download.R</code></td><td>1596–1598</td><td><pre><code class="language-r">    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }</code></pre></td><td><pre><code class="language-r">    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download.R</code></td><td>1948–1948</td><td><pre><code class="language-r">  data_format &lt;- match.arg(data_format)</code></pre></td><td><pre><code class="language-r">  data_format &lt;- match.arg(data_format)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>1949–1949</td><td><pre><code class="language-r">  data_region &lt;- match.arg(data_region)</code></pre></td><td><pre><code class="language-r">  data_region &lt;- match.arg(data_region)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>2149–2149</td><td><pre><code class="language-r">  data_format &lt;- match.arg(data_format)</code></pre></td><td><pre><code class="language-r">  data_format &lt;- match.arg(data_format)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>2308–2308</td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/download.R</code></td><td>2600–2600</td><td><pre><code class="language-r">  data_resolution &lt;- match.arg(data_resolution)</code></pre></td><td><pre><code class="language-r">  data_resolution &lt;- match.arg(data_resolution)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>2603–2603</td><td><pre><code class="language-r">  time_period &lt;- match.arg(time_period)</code></pre></td><td><pre><code class="language-r">  time_period &lt;- match.arg(time_period)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>2827–2827</td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/download.R</code></td><td>2840–2840</td><td><pre><code class="language-r">  product &lt;- match.arg(product)</code></pre></td><td><pre><code class="language-r">  product &lt;- match.arg(product)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>3121–3121</td><td><pre><code class="language-r">  stopifnot(length(year) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(year) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/download.R</code></td><td>3411–3416</td><td><pre><code class="language-r">      if (file.exists(download_names[fn])) {
        if (!dir.exists(dir_unzip[fn])) {
          dir.create(dir_unzip[fn], recursive = TRUE)
        }
        utils::unzip(zipfile = download_names[fn], exdir = dir_unzip[fn])
      }</code></pre></td><td><pre><code class="language-r">      if (file.exists(download_names[fn])) {
        if (!dir.exists(dir_unzip[fn])) {
          dir.create(dir_unzip[fn], recursive = TRUE)
        }
        utils::unzip(zipfile = download_names[fn], exdir = dir_unzip[fn])
      }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download.R</code></td><td>3412–3414</td><td><pre><code class="language-r">        if (!dir.exists(dir_unzip[fn])) {
          dir.create(dir_unzip[fn], recursive = TRUE)
        }</code></pre></td><td><pre><code class="language-r">        if (!dir.exists(dir_unzip[fn])) {
          dir.create(dir_unzip[fn], recursive = TRUE)
        }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download.R</code></td><td>3423–3425</td><td><pre><code class="language-r">      if (file.exists(fn)) {
        file.remove(fn)
      }</code></pre></td><td><pre><code class="language-r">      if (file.exists(fn)) {
        file.remove(fn)
      }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download.R</code></td><td>3491–3491</td><td><pre><code class="language-r">  stopifnot(length(year) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(year) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/download.R</code></td><td>3539–3541</td><td><pre><code class="language-r">    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }</code></pre></td><td><pre><code class="language-r">    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download.R</code></td><td>3670–3670</td><td><pre><code class="language-r">  stopifnot(length(year) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(year) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/download.R</code></td><td>3721–3723</td><td><pre><code class="language-r">    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }</code></pre></td><td><pre><code class="language-r">    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download.R</code></td><td>3867–3867</td><td><pre><code class="language-r">    region &lt;- match.arg(region)</code></pre></td><td><pre><code class="language-r">    region &lt;- match.arg(region)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>3868–3868</td><td><pre><code class="language-r">    type &lt;- match.arg(type)</code></pre></td><td><pre><code class="language-r">    type &lt;- match.arg(type)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>4542–4542</td><td><pre><code class="language-r">  data_type &lt;- match.arg(data_type)</code></pre></td><td><pre><code class="language-r">  data_type &lt;- match.arg(data_type)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>4543–4543</td><td><pre><code class="language-r">  element &lt;- match.arg(element)</code></pre></td><td><pre><code class="language-r">  element &lt;- match.arg(element)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>4544–4544</td><td><pre><code class="language-r">  format &lt;- match.arg(format)</code></pre></td><td><pre><code class="language-r">  format &lt;- match.arg(format)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>4765–4765</td><td><pre><code class="language-r">  source &lt;- match.arg(source)</code></pre></td><td><pre><code class="language-r">  source &lt;- match.arg(source)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>4838–4840</td><td><pre><code class="language-r">      if (!dir.exists(dir_unzip[fn])) {
        dir.create(dir_unzip[fn], recursive = TRUE)
      }</code></pre></td><td><pre><code class="language-r">      if (!dir.exists(dir_unzip[fn])) {
        dir.create(dir_unzip[fn], recursive = TRUE)
      }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download.R</code></td><td>4940–4943</td><td><pre><code class="language-r">  satellite &lt;- as.character(satellite)
  if (!satellite %in% c(&quot;16&quot;, &quot;18&quot;)) {
    stop(&quot;satellite must be &#x27;16&#x27; or &#x27;18&#x27;.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  satellite &lt;- as.character(satellite)
  if (!satellite %in% c(&quot;16&quot;, &quot;18&quot;)) {
    stop(&quot;satellite must be &#x27;16&#x27; or &#x27;18&#x27;.\n&quot;)
  }</code></pre></td><td>Keep existing — no replacement — Keep as.character normalization and the existing message. assertChoice would change malformed-vector behavior; no gain warrants changing this stable guard.</td></tr>
<tr><td><code>R/download.R</code></td><td>4946–4954</td><td><pre><code class="language-r">  product &lt;- toupper(product)
  valid_products &lt;- c(&quot;ADP-C&quot;, &quot;ADP-F&quot;, &quot;ADP-M&quot;)
  if (!product %in% valid_products) {
    stop(paste0(
      &quot;product must be one of: &quot;,
      paste(valid_products, collapse = &quot;, &quot;),
      &quot;.\n&quot;
    ))
  }</code></pre></td><td><pre><code class="language-r">  product &lt;- toupper(product)
  valid_products &lt;- c(&quot;ADP-C&quot;, &quot;ADP-F&quot;, &quot;ADP-M&quot;)
  if (!product %in% valid_products) {
    stop(paste0(
      &quot;product must be one of: &quot;,
      paste(valid_products, collapse = &quot;, &quot;),
      &quot;.\n&quot;
    ))
  }</code></pre></td><td>Keep existing — no replacement — Keep toupper normalization and original product-choice message. Do not replace match.arg elsewhere with exact membership.</td></tr>
<tr><td><code>R/download.R</code></td><td>4966–4966</td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/download.R</code></td><td>5172–5172</td><td><pre><code class="language-r">  product &lt;- match.arg(product)</code></pre></td><td><pre><code class="language-r">  product &lt;- match.arg(product)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>5182–5182</td><td><pre><code class="language-r">  stopifnot(length(year) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(year) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/download.R</code></td><td>5241–5243</td><td><pre><code class="language-r">      if (!file.exists(zip_file)) {
        next
      }</code></pre></td><td><pre><code class="language-r">      if (!file.exists(zip_file)) {
        next
      }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download.R</code></td><td>5345–5345</td><td><pre><code class="language-r">  source &lt;- match.arg(source)</code></pre></td><td><pre><code class="language-r">  source &lt;- match.arg(source)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download.R</code></td><td>5354–5354</td><td><pre><code class="language-r">  stopifnot(length(date) == 2L)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(date) == 2L)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/download.R</code></td><td>5366–5370</td><td><pre><code class="language-r">  timescale &lt;- as.integer(timescale)
  if (source %in% c(&quot;spei&quot;, &quot;eddi&quot;)) {
    if (is.na(timescale) || timescale &lt; 1L) {
      stop(&quot;`timescale` must be a positive integer (months).\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">  timescale &lt;- as.integer(timescale)
  if (source %in% c(&quot;spei&quot;, &quot;eddi&quot;)) {
    if (is.na(timescale) || timescale &lt; 1L) {
      stop(&quot;`timescale` must be a positive integer (months).\n&quot;)
    }</code></pre></td><td>Keep existing — no replacement — Preserve as.integer coercion, branch scope, and existing short-circuit behavior. assertInt would add an explicit scalar restriction and change diagnostics for malformed vectors. No unconditional replacement under strict compatibility.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>17–19</td><td><pre><code class="language-r">    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }</code></pre></td><td><pre><code class="language-r">    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>27–29</td><td><pre><code class="language-r">      if (!dir.exists(directory_zip)) {
        dir.create(directory_zip, recursive = TRUE)
      }</code></pre></td><td><pre><code class="language-r">      if (!dir.exists(directory_zip)) {
        dir.create(directory_zip, recursive = TRUE)
      }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>36–38</td><td><pre><code class="language-r">      if (!dir.exists(directory_data)) {
        dir.create(directory_data, recursive = TRUE)
      }</code></pre></td><td><pre><code class="language-r">      if (!dir.exists(directory_data)) {
        dir.create(directory_data, recursive = TRUE)
      }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>126–126</td><td><pre><code class="language-r">  stopifnot(length(key) == length(scale))</code></pre></td><td><pre><code class="language-r">  stopifnot(length(key) == length(scale))</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>204–211</td><td><pre><code class="language-r">    if (!acknowledgement) {
      stop(paste0(
        &quot;Data download acknowledgement is set to FALSE. &quot;,
        &quot;Please acknowledge that the data downloaded using this &quot;,
        &quot;function may be very large and use lots of machine storage &quot;,
        &quot;and memory.\n&quot;
      ))
    }</code></pre></td><td><pre><code class="language-r">    if (!acknowledgement) {
      stop(paste0(
        &quot;Data download acknowledgement is set to FALSE. &quot;,
        &quot;Please acknowledge that the data downloaded using this &quot;,
        &quot;function may be very large and use lots of machine storage &quot;,
        &quot;and memory.\n&quot;
      ))
    }</code></pre></td><td>Keep existing — no replacement — Do not add assertFlag: !acknowledgement currently accepts nonzero numeric truthy values. Keep consent behavior and message before side effects.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>324–329</td><td><pre><code class="language-r">  if (is.null(urls) || length(urls) == 0) {
    stop(&quot;No URLs provided for download.\n&quot;)
  }
  if (is.null(destfiles) || length(destfiles) != length(urls)) {
    stop(&quot;destfiles must have same length as urls.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (is.null(urls) || length(urls) == 0) {
    stop(&quot;No URLs provided for download.\n&quot;)
  }
  if (is.null(destfiles) || length(destfiles) != length(urls)) {
    stop(&quot;destfiles must have same length as urls.\n&quot;)
  }</code></pre></td><td>Keep existing — no replacement — Only NULL/length is checked. Character, nonmissing and nonempty-string assertions add new restrictions. Keep filtering and downloader behavior.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>359–361</td><td><pre><code class="language-r">    if (!dir.exists(destdir)) {
      dir.create(destdir, recursive = TRUE)
    }</code></pre></td><td><pre><code class="language-r">    if (!dir.exists(destdir)) {
      dir.create(destdir, recursive = TRUE)
    }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>442–444</td><td><pre><code class="language-r">          if (file.exists(destfile)) {
            file.remove(destfile)
          }</code></pre></td><td><pre><code class="language-r">          if (file.exists(destfile)) {
            file.remove(destfile)
          }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>481–483</td><td><pre><code class="language-r">        if (file.exists(destfile)) {
          file.remove(destfile)
        }</code></pre></td><td><pre><code class="language-r">        if (file.exists(destfile)) {
          file.remove(destfile)
        }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>620–622</td><td><pre><code class="language-r">    if (file.exists(command_txt)) {
      file.remove(command_txt)
    }</code></pre></td><td><pre><code class="language-r">    if (file.exists(command_txt)) {
      file.remove(command_txt)
    }</code></pre></td><td>Keep existing — no replacement — Operational control flow: absence triggers creation, skipping or optional cleanup. It is not an invalid-input assertion. Keep side effects and their order; checkmate cannot replace them.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>907–907</td><td><pre><code class="language-r">    method &lt;- match.arg(method)</code></pre></td><td><pre><code class="language-r">    method &lt;- match.arg(method)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>1070–1070</td><td><pre><code class="language-r">  stopifnot(length(variable) == 1)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(variable) == 1)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>1246–1246</td><td><pre><code class="language-r">  method &lt;- match.arg(method)</code></pre></td><td><pre><code class="language-r">  method &lt;- match.arg(method)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>1356–1363</td><td><pre><code class="language-r">  stopifnot(
    &quot;extent must be numeric(4)&quot; = is.numeric(extent) &amp;&amp; length(extent) == 4,
    &quot;extent[1] (xmin) must be &gt;= -180&quot; = extent[1] &gt;= -180,
    &quot;extent[3] (xmax) must be &lt;= 180&quot; = extent[3] &lt;= 180,
    &quot;extent[2] (ymin) must be &gt;= -90&quot; = extent[2] &gt;= -90,
    &quot;extent[4] (ymax) must be &lt;= 90&quot; = extent[4] &lt;= 90,
    &quot;xmin must be &lt; xmax&quot; = extent[1] &lt; extent[3],
    &quot;ymin must be &lt; ymax&quot; = extent[2] &lt; extent[4]</code></pre></td><td><pre><code class="language-r">  stopifnot(
    &quot;extent must be numeric(4)&quot; = is.numeric(extent) &amp;&amp; length(extent) == 4,
    &quot;extent[1] (xmin) must be &gt;= -180&quot; = extent[1] &gt;= -180,
    &quot;extent[3] (xmax) must be &lt;= 180&quot; = extent[3] &lt;= 180,
    &quot;extent[2] (ymin) must be &gt;= -90&quot; = extent[2] &gt;= -90,
    &quot;extent[4] (ymax) must be &lt;= 90&quot; = extent[4] &lt;= 90,
    &quot;xmin must be &lt; xmax&quot; = extent[1] &lt; extent[3],
    &quot;ymin must be &lt; ymax&quot; = extent[2] &lt; extent[4]</code></pre></td><td>Keep existing — no replacement — Named extent inequalities preserve xmin/xmax/ymin/ymax and ordering diagnostics. Generic numeric bounds cannot express all cross-element relations or preserve failure order.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>1356–1364</td><td><pre><code class="language-r">  stopifnot(
    &quot;extent must be numeric(4)&quot; = is.numeric(extent) &amp;&amp; length(extent) == 4,
    &quot;extent[1] (xmin) must be &gt;= -180&quot; = extent[1] &gt;= -180,
    &quot;extent[3] (xmax) must be &lt;= 180&quot; = extent[3] &lt;= 180,
    &quot;extent[2] (ymin) must be &gt;= -90&quot; = extent[2] &gt;= -90,
    &quot;extent[4] (ymax) must be &lt;= 90&quot; = extent[4] &lt;= 90,
    &quot;xmin must be &lt; xmax&quot; = extent[1] &lt; extent[3],
    &quot;ymin must be &lt; ymax&quot; = extent[2] &lt; extent[4]
  )</code></pre></td><td><pre><code class="language-r">  stopifnot(
    &quot;extent must be numeric(4)&quot; = is.numeric(extent) &amp;&amp; length(extent) == 4,
    &quot;extent[1] (xmin) must be &gt;= -180&quot; = extent[1] &gt;= -180,
    &quot;extent[3] (xmax) must be &lt;= 180&quot; = extent[3] &lt;= 180,
    &quot;extent[2] (ymin) must be &gt;= -90&quot; = extent[2] &gt;= -90,
    &quot;extent[4] (ymax) must be &lt;= 90&quot; = extent[4] &lt;= 90,
    &quot;xmin must be &lt; xmax&quot; = extent[1] &lt; extent[3],
    &quot;ymin must be &lt; ymax&quot; = extent[2] &lt; extent[4]
  )</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>13–23</td><td><pre><code class="language-r">  stopifnot(
    &quot;x is not a sftime&quot; = class(x)[1] == &quot;sftime&quot;,
    &quot;x is not inherited from a data.table&quot; =
      class(x)[3] == &quot;data.table&quot;,
    &quot;time column should be called time&quot; =
      attributes(x)$time_column == &quot;time&quot;,
    &quot;geometry column should be called geometry&quot; =
      attributes(x)$sf_column == &quot;geometry&quot;,
    &quot;geometry is not a sfc_POINT&quot; = class(x$geometry)[1] == &quot;sfc_POINT&quot;
  )
}</code></pre></td><td><pre><code class="language-r">  stopifnot(
    &quot;x is not a sftime&quot; = class(x)[1] == &quot;sftime&quot;,
    &quot;x is not inherited from a data.table&quot; =
      class(x)[3] == &quot;data.table&quot;,
    &quot;time column should be called time&quot; =
      attributes(x)$time_column == &quot;time&quot;,
    &quot;geometry column should be called geometry&quot; =
      attributes(x)$sf_column == &quot;geometry&quot;,
    &quot;geometry is not a sfc_POINT&quot; = class(x$geometry)[1] == &quot;sfc_POINT&quot;
  )
}</code></pre></td><td>Keep existing — no replacement — Positional class and attribute checks are stronger/different than inherits or generic class assertions. Preserve ordering and named diagnostics.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>37–45</td><td><pre><code class="language-r">  stopifnot(
    &quot;x is not a sf&quot; = class(x)[1] == &quot;sf&quot;,
    &quot;x is not inherited from a data.table&quot; =
      class(x)[2] == &quot;data.table&quot;,
    &quot;geometry column should be called geometry&quot; =
      attributes(x)$sf_column == &quot;geometry&quot;,
    &quot;geometry is not a sfc_POINT&quot; = class(x$geometry)[1] == &quot;sfc_POINT&quot;
  )
}</code></pre></td><td><pre><code class="language-r">  stopifnot(
    &quot;x is not a sf&quot; = class(x)[1] == &quot;sf&quot;,
    &quot;x is not inherited from a data.table&quot; =
      class(x)[2] == &quot;data.table&quot;,
    &quot;geometry column should be called geometry&quot; =
      attributes(x)$sf_column == &quot;geometry&quot;,
    &quot;geometry is not a sfc_POINT&quot; = class(x$geometry)[1] == &quot;sfc_POINT&quot;
  )
}</code></pre></td><td>Keep existing — no replacement — Same positional class/geometry contract; broad S3/S4 assertions would change accepted subclasses or class order.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>85–87</td><td><pre><code class="language-r">  if (any(!(c(lonname, latname, timename) %in% colnames(x)))) {
    stop(&quot;Some of lon, lat, time columns missing or mispelled&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (any(!(c(lonname, latname, timename) %in% colnames(x)))) {
    stop(&quot;Some of lon, lat, time columns missing or mispelled&quot;)
  }</code></pre></td><td>Keep existing — no replacement — Column-selector %in% uses coercion. assertSubset imposes compatible types and changes the named missing-column error.</td></tr>
<tr><td><code>R/process.R</code></td><td>106–106</td><td><pre><code class="language-r">    covariate &lt;- match.arg(covariate)</code></pre></td><td><pre><code class="language-r">    covariate &lt;- match.arg(covariate)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/process.R</code></td><td>244–244</td><td><pre><code class="language-r">      product &lt;- match.arg(product)</code></pre></td><td><pre><code class="language-r">      product &lt;- match.arg(product)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/process.R</code></td><td>337–342</td><td><pre><code class="language-r">      suppressWarnings(terra::is.rotated(terra::rast(path)))
    if (any(status_curv)) {
      stop(
        &quot;The raster is curvilinear. Please rectify or warp
the input then flatten it manually.&quot;
      )</code></pre></td><td><pre><code class="language-r">      suppressWarnings(terra::is.rotated(terra::rast(path)))
    if (any(status_curv)) {
      stop(
        &quot;The raster is curvilinear. Please rectify or warp
the input then flatten it manually.&quot;
      )</code></pre></td><td>Keep existing — no replacement — Domain-specific curvilinear restriction provides corrective instructions. A boolean assertion loses those instructions and can change NA failure behavior.</td></tr>
<tr><td><code>R/process.R</code></td><td>455–455</td><td><pre><code class="language-r">  fusion_method &lt;- match.arg(fusion_method)</code></pre></td><td><pre><code class="language-r">  fusion_method &lt;- match.arg(fusion_method)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/process.R</code></td><td>598–598</td><td><pre><code class="language-r">  return_type &lt;- match.arg(return_type)</code></pre></td><td><pre><code class="language-r">  return_type &lt;- match.arg(return_type)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/process.R</code></td><td>1482–1484</td><td><pre><code class="language-r">  if (length(variables) &gt; 0 &amp;&amp; any(!nzchar(trimws(variables)))) {
    stop(&quot;`variables` cannot include empty patterns.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (length(variables) &gt; 0 &amp;&amp; any(!nzchar(trimws(variables)))) {
    stop(&quot;`variables` cannot include empty patterns.\n&quot;)
  }</code></pre></td><td>Keep existing — no replacement — Keep explicit whitespace-only pattern error. min.chars on untrimmed text would admit whitespace; assertTRUE with .var.name does not communicate the pattern requirement.</td></tr>
<tr><td><code>R/process.R</code></td><td>1489–1491</td><td><pre><code class="language-r">    if (any(!nzchar(trimws(chemical)))) {
      stop(&quot;`chemical` cannot include empty patterns.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (any(!nzchar(trimws(chemical)))) {
      stop(&quot;`chemical` cannot include empty patterns.\n&quot;)
    }</code></pre></td><td>Keep existing — no replacement — Keep optional chemical blank-pattern validation and original untrimmed regex text.</td></tr>
<tr><td><code>R/process.R</code></td><td>1498–1498</td><td><pre><code class="language-r">  industry_group &lt;- match.arg(industry_group)</code></pre></td><td><pre><code class="language-r">  industry_group &lt;- match.arg(industry_group)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/process.R</code></td><td>1908–1910</td><td><pre><code class="language-r">    resolution_temporal &lt;- match.arg(
      resolution_temporal, c(&quot;daily&quot;, &quot;hourly&quot;)
    )</code></pre></td><td><pre><code class="language-r">    resolution_temporal &lt;- match.arg(
      resolution_temporal, c(&quot;daily&quot;, &quot;hourly&quot;)
    )</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/process.R</code></td><td>1915–1915</td><td><pre><code class="language-r">    mode &lt;- match.arg(mode)</code></pre></td><td><pre><code class="language-r">    mode &lt;- match.arg(mode)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/process.R</code></td><td>1916–1916</td><td><pre><code class="language-r">    return_format &lt;- match.arg(return_format)</code></pre></td><td><pre><code class="language-r">    return_format &lt;- match.arg(return_format)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/process.R</code></td><td>2440–2440</td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/process.R</code></td><td>2777–2777</td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/process.R</code></td><td>3120–3122</td><td><pre><code class="language-r">    if (!is.character(variable) || length(variable) != 1 || !nzchar(variable)) {
      stop(&quot;`variable` must be a single non-empty character string.\n&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!is.character(variable) || length(variable) != 1 || !nzchar(variable)) {
      stop(&quot;`variable` must be a single non-empty character string.\n&quot;)
    }</code></pre></td><td>Keep existing — no replacement — nzchar(NA_character_) is TRUE by default; assertString(..., min.chars=1) would reject NA earlier. Keep trimming order and existing error routing.</td></tr>
<tr><td><code>R/process.R</code></td><td>3128–3128</td><td><pre><code class="language-r">    stopifnot(length(date) == 2)</code></pre></td><td><pre><code class="language-r">    stopifnot(length(date) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/process.R</code></td><td>3300–3302</td><td><pre><code class="language-r">        if (length(date_str) != terra::nlyr(data_return)) {
          stop(&quot;daily_agg: cannot determine dates from layer times or names.\n&quot;)
        }</code></pre></td><td><pre><code class="language-r">        if (length(date_str) != terra::nlyr(data_return)) {
          stop(&quot;daily_agg: cannot determine dates from layer times or names.\n&quot;)
        }</code></pre></td><td>Keep existing — no replacement — Keep daily_agg fallback invariant. It is a relationship between parsed dates and raster layer count, not a character-input contract. No need to add type or missingness checks or discard the source-specific diagnostic.</td></tr>
<tr><td><code>R/process.R</code></td><td>3426–3426</td><td><pre><code class="language-r">    stopifnot(length(date) == 2)</code></pre></td><td><pre><code class="language-r">    stopifnot(length(date) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/process.R</code></td><td>3610–3612</td><td><pre><code class="language-r">        if (length(date_str) != terra::nlyr(data_return)) {
          stop(&quot;daily_agg: cannot determine dates from layer times or names.\n&quot;)
        }</code></pre></td><td><pre><code class="language-r">        if (length(date_str) != terra::nlyr(data_return)) {
          stop(&quot;daily_agg: cannot determine dates from layer times or names.\n&quot;)
        }</code></pre></td><td>Keep existing — no replacement — Same daily_agg fallback invariant for MERRA2: keep within the fallback branch after attempting layer-time extraction.</td></tr>
<tr><td><code>R/process.R</code></td><td>3712–3712</td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/process.R</code></td><td>3917–3917</td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/process.R</code></td><td>4413–4413</td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(date) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/process.R</code></td><td>4566–4568</td><td><pre><code class="language-r">      if (length(date_str) != terra::nlyr(data_return)) {
        stop(&quot;daily_agg: cannot determine dates from layer times or names.\n&quot;)
      }</code></pre></td><td><pre><code class="language-r">      if (length(date_str) != terra::nlyr(data_return)) {
        stop(&quot;daily_agg: cannot determine dates from layer times or names.\n&quot;)
      }</code></pre></td><td>Keep existing — no replacement — Same daily_agg fallback invariant for GOES: preserve date recovery and its specific failure message.</td></tr>
<tr><td><code>R/process.R</code></td><td>4715–4715</td><td><pre><code class="language-r">  product &lt;- match.arg(product)</code></pre></td><td><pre><code class="language-r">  product &lt;- match.arg(product)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/process.R</code></td><td>4716–4716</td><td><pre><code class="language-r">  return_format &lt;- match.arg(return_format)</code></pre></td><td><pre><code class="language-r">  return_format &lt;- match.arg(return_format)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/process.R</code></td><td>4719–4721</td><td><pre><code class="language-r">  if (is.null(path) || !dir.exists(path)) {
    stop(&quot;`path` must be a valid directory path.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (is.null(path) || !dir.exists(path)) {
    stop(&quot;`path` must be a valid directory path.\n&quot;)
  }</code></pre></td><td>Keep existing — no replacement — Keep directory-only check and its message. assertString/assertDirectoryExists may change coercion and vector handling; do not add access permission checks.</td></tr>
<tr><td><code>R/process.R</code></td><td>4767–4767</td><td><pre><code class="language-r">    stopifnot(length(date) == 2)</code></pre></td><td><pre><code class="language-r">    stopifnot(length(date) == 2)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/process.R</code></td><td>4981–4981</td><td><pre><code class="language-r">  source &lt;- match.arg(source)</code></pre></td><td><pre><code class="language-r">  source &lt;- match.arg(source)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/process.R</code></td><td>4990–4990</td><td><pre><code class="language-r">  stopifnot(length(date) == 2L)</code></pre></td><td><pre><code class="language-r">  stopifnot(length(date) == 2L)</code></pre></td><td>Keep existing — no replacement — Length-only invariant: assertVector/Character/Date would add type or shape restrictions. Keep scalar duplication, sorting and coercion in their existing positions. A boolean wrapper offers no substantive improvement and changes stopifnot diagnostics.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>654–654</td><td><pre><code class="language-r">  type &lt;- match.arg(type)</code></pre></td><td><pre><code class="language-r">  type &lt;- match.arg(type)</code></pre></td><td>Keep existing — no replacement — Argument normalization, default selection and permitted partial matching must remain. checkmate choice assertions do not reproduce match.arg; retain any explicit several.ok setting.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>753–755</td><td><pre><code class="language-r">  if (!is.character(pattern) || length(pattern) != 1L || !nzchar(pattern)) {
    stop(&quot;`pattern` must be a single non-empty character string.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (!is.character(pattern) || length(pattern) != 1L || !nzchar(pattern)) {
    stop(&quot;`pattern` must be a single non-empty character string.\n&quot;)
  }</code></pre></td><td>Keep existing — no replacement — The pattern check does not explicitly reject NA_character_. A nonmissing checkmate string validator tightens the guard. Keep as written.</td></tr>
</tbody></table></div>

## Remaining source failure paths

This register covers each remaining explicit stop/stopifnot invocation, rather than silently omitting files with no useful checkmate migration. Ranges here quote the failure call; its surrounding control flow is retained. No migration is proposed for date parsing, missing upstream data, spatial geometry/CRS/overlap, conversion errors, legacy-option notices or mixed-input dispatch.

<div class="table-scroll"><table class="validation-table">
<thead><tr><th>File path</th><th>Start–end lines</th><th>Current Code</th><th>Suggested checkmate / retained code</th><th>Decision and compatibility reason</th></tr></thead><tbody>
<tr><td><code>R/calculate_covariates.R</code></td><td>185–194</td><td><pre><code class="language-r">          stop(
            paste0(
              e,
              &quot;\n&quot;,
              paste0(deparse(args(what_to_run)), collapse = &quot;\n&quot;),
              &quot;\n&quot;,
              &quot;Please refer to the argument list and &quot;,
              &quot;the error message above to rectify the error.\n&quot;
            )
          )</code></pre></td><td><pre><code class="language-r">          stop(
            paste0(
              e,
              &quot;\n&quot;,
              paste0(deparse(args(what_to_run)), collapse = &quot;\n&quot;),
              &quot;\n&quot;,
              &quot;Please refer to the argument list and &quot;,
              &quot;the error message above to rectify the error.\n&quot;
            )
          )</code></pre></td><td>Keep existing — Existing error propagation/context wrapping, not an input validator; retain the underlying condition and wrapper diagnostics.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>274–274</td><td><pre><code class="language-r">      stop(&quot;`radius` should be greater than or equal to 0.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;`radius` should be greater than or equal to 0.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>585–585</td><td><pre><code class="language-r">    stop(&quot;radius has not a likely value.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;radius has not a likely value.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>591–591</td><td><pre><code class="language-r">    stop(&quot;from is not a SpatRaster.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;from is not a SpatRaster.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>596–601</td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;`from` contains more than one data layer. Current version &quot;,
        &quot;only processes one year worth of NLCD data.&quot;
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;`from` contains more than one data layer. Current version &quot;,
        &quot;only processes one year worth of NLCD data.&quot;
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>626–626</td><td><pre><code class="language-r">  stopifnot(year %in% 1985:2024L)</code></pre></td><td><pre><code class="language-r">  stopifnot(year %in% 1985:2024L)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>993–996</td><td><pre><code class="language-r">        stop(
          &quot;Required ecoregion field missing from intersection output: &quot;,
          field
        )</code></pre></td><td><pre><code class="language-r">        stop(
          &quot;Required ecoregion field missing from intersection output: &quot;,
          field
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1019–1019</td><td><pre><code class="language-r">      stop(&quot;`radius` should be greater than or equal to 0.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;`radius` should be greater than or equal to 0.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1387–1392</td><td><pre><code class="language-r">      stop(
        paste0(
          &quot;from should be a character vector of paths, SpatRaster, &quot;,
          &quot;or SpatVector.\n&quot;
        )
      )</code></pre></td><td><pre><code class="language-r">      stop(
        paste0(
          &quot;from should be a character vector of paths, SpatRaster, &quot;,
          &quot;or SpatVector.\n&quot;
        )
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1400–1400</td><td><pre><code class="language-r">        stop(&quot;from_secondary should be SpatRaster when from is SpatRaster.\n&quot;)</code></pre></td><td><pre><code class="language-r">        stop(&quot;from_secondary should be SpatRaster when from is SpatRaster.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1403–1408</td><td><pre><code class="language-r">      stop(
        paste0(
          &quot;from_secondary is only supported for character &quot;,
          &quot;or SpatRaster inputs.\n&quot;
        )
      )</code></pre></td><td><pre><code class="language-r">      stop(
        paste0(
          &quot;from_secondary is only supported for character &quot;,
          &quot;or SpatRaster inputs.\n&quot;
        )
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1434–1436</td><td><pre><code class="language-r">        stop(
          &quot;MODIS input files contain mixed or unsupported temporal patterns.\n&quot;
        )</code></pre></td><td><pre><code class="language-r">        stop(
          &quot;MODIS input files contain mixed or unsupported temporal patterns.\n&quot;
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1480–1483</td><td><pre><code class="language-r">      stop(
        &quot;locs cannot be convertible to sf.
      Please convert locs into a sf object to proceed.\n&quot;
      )</code></pre></td><td><pre><code class="language-r">      stop(
        &quot;locs cannot be convertible to sf.
      Please convert locs into a sf object to proceed.\n&quot;
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1575–1575</td><td><pre><code class="language-r">                stop(&quot;No MODIS files found for selected fusion date.\n&quot;)</code></pre></td><td><pre><code class="language-r">                stop(&quot;No MODIS files found for selected fusion date.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1607–1610</td><td><pre><code class="language-r">                  stop(
                    &quot;Primary and secondary MODIS rasters have incompatible &quot;,
                    &quot;geometry.\n&quot;
                  )</code></pre></td><td><pre><code class="language-r">                  stop(
                    &quot;Primary and secondary MODIS rasters have incompatible &quot;,
                    &quot;geometry.\n&quot;
                  )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1615–1618</td><td><pre><code class="language-r">                  stop(
                    &quot;Primary and secondary MODIS rasters have different &quot;,
                    &quot;layer counts.\n&quot;
                  )</code></pre></td><td><pre><code class="language-r">                  stop(
                    &quot;Primary and secondary MODIS rasters have different &quot;,
                    &quot;layer counts.\n&quot;
                  )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1652–1655</td><td><pre><code class="language-r">                stop(
                  &quot;Primary and secondary MODIS rasters have incompatible &quot;,
                  &quot;geometry.\n&quot;
                )</code></pre></td><td><pre><code class="language-r">                stop(
                  &quot;Primary and secondary MODIS rasters have incompatible &quot;,
                  &quot;geometry.\n&quot;
                )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1660–1663</td><td><pre><code class="language-r">                stop(
                  &quot;Primary and secondary MODIS rasters have different &quot;,
                  &quot;layer counts.\n&quot;
                )</code></pre></td><td><pre><code class="language-r">                stop(
                  &quot;Primary and secondary MODIS rasters have different &quot;,
                  &quot;layer counts.\n&quot;
                )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1779–1779</td><td><pre><code class="language-r">    stop(&quot;from should be a SpatVector returned by process_mcd14ml.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;from should be a SpatVector returned by process_mcd14ml.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1782–1782</td><td><pre><code class="language-r">    stop(&quot;from is missing required MCD14ML fields.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;from is missing required MCD14ML fields.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1936–1936</td><td><pre><code class="language-r">      stop(&quot;Argument locs is not a data.frame.\n&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;Argument locs is not a data.frame.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>1939–1939</td><td><pre><code class="language-r">      stop(&quot;A mandatory field &#x27;time&#x27; does not exist in locs.\n&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;A mandatory field &#x27;time&#x27; does not exist in locs.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2113–2117</td><td><pre><code class="language-r">      stop(
        &quot;The following `target_fields` are missing in `from`: &quot;,
        paste(missing_targets, collapse = &quot;, &quot;),
        &quot;\n&quot;
      )</code></pre></td><td><pre><code class="language-r">      stop(
        &quot;The following `target_fields` are missing in `from`: &quot;,
        paste(missing_targets, collapse = &quot;, &quot;),
        &quot;\n&quot;
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2143–2143</td><td><pre><code class="language-r">        stop(&quot;When `C0` is provided, `target_fields` must have length 1.\n&quot;)</code></pre></td><td><pre><code class="language-r">        stop(&quot;When `C0` is provided, `target_fields` must have length 1.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2374–2377</td><td><pre><code class="language-r">    stop(
      &quot;No TRI target fields found in `from`. &quot;,
      &quot;Process TRI data using `process_tri()` before calculation.\n&quot;
    )</code></pre></td><td><pre><code class="language-r">    stop(
      &quot;No TRI target fields found in `from`. &quot;,
      &quot;Process TRI data using `process_tri()` before calculation.\n&quot;
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2390–2394</td><td><pre><code class="language-r">      stop(
        &quot;The following `C0` columns are missing in `from`: &quot;,
        paste(missing_c0_cols, collapse = &quot;, &quot;),
        &quot;\n&quot;
      )</code></pre></td><td><pre><code class="language-r">      stop(
        &quot;The following `C0` columns are missing in `from`: &quot;,
        paste(missing_c0_cols, collapse = &quot;, &quot;),
        &quot;\n&quot;
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2401–2405</td><td><pre><code class="language-r">      stop(
        &quot;`C0` must have length 1 or match the number of TRI target fields (&quot;,
        length(tri_cols),
        &quot;).\n&quot;
      )</code></pre></td><td><pre><code class="language-r">      stop(
        &quot;`C0` must have length 1 or match the number of TRI target fields (&quot;,
        length(tri_cols),
        &quot;).\n&quot;
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2449–2449</td><td><pre><code class="language-r">    stop(&quot;`locs_id` was not found in `locs`.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`locs_id` was not found in `locs`.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2454–2454</td><td><pre><code class="language-r">      stop(&quot;`locs_id` was not found in `locs` after CRS transformation.\n&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;`locs_id` was not found in `locs` after CRS transformation.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>2523–2523</td><td><pre><code class="language-r">      stop(&quot;locs is unable to be converted to SpatVector.\n&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;locs is unable to be converted to SpatVector.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>3411–3411</td><td><pre><code class="language-r">    stop(&quot;radius should be greater than 0.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;radius should be greater than 0.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>3930–3934</td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;To return with geometry, `from` must be a `SpatVector` object.\n&quot;
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;To return with geometry, `from` must be a `SpatVector` object.\n&quot;
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>3942–3942</td><td><pre><code class="language-r">  stopifnot(methods::is(from, &quot;data.frame&quot;))</code></pre></td><td><pre><code class="language-r">  stopifnot(methods::is(from, &quot;data.frame&quot;))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>3944–3944</td><td><pre><code class="language-r">  stopifnot(!is.null(time_id))</code></pre></td><td><pre><code class="language-r">  stopifnot(!is.null(time_id))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>3957–3959</td><td><pre><code class="language-r">    stop(
      &quot;Dates requested in `date` do not align with data available in `from`.&quot;
    )</code></pre></td><td><pre><code class="language-r">    stop(
      &quot;Dates requested in `date` do not align with data available in `from`.&quot;
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>4067–4067</td><td><pre><code class="language-r">    stop(&quot;`from` must be a SpatRaster object.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`from` must be a SpatRaster object.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>4156–4159</td><td><pre><code class="language-r">        stop(
          &quot;PRISM `.by_time` summarization requires a single covariate column &quot;,
          &quot;or an existing `time` column.\n&quot;
        )</code></pre></td><td><pre><code class="language-r">        stop(
          &quot;PRISM `.by_time` summarization requires a single covariate column &quot;,
          &quot;or an existing `time` column.\n&quot;
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>4191–4194</td><td><pre><code class="language-r">        stop(
          &quot;Could not derive PRISM time for `.by_time` summarization. &quot;,
          &quot;Provide data with explicit time in layer metadata.\n&quot;
        )</code></pre></td><td><pre><code class="language-r">        stop(
          &quot;Could not derive PRISM time for `.by_time` summarization. &quot;,
          &quot;Provide data with explicit time in layer metadata.\n&quot;
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>4273–4273</td><td><pre><code class="language-r">    stop(&quot;`from` must be a SpatRaster object.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`from` must be a SpatRaster object.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>4348–4351</td><td><pre><code class="language-r">        stop(
          &quot;EDGAR `.by_time` summarization requires a single covariate column &quot;,
          &quot;or an existing `time` column.\n&quot;
        )</code></pre></td><td><pre><code class="language-r">        stop(
          &quot;EDGAR `.by_time` summarization requires a single covariate column &quot;,
          &quot;or an existing `time` column.\n&quot;
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>4359–4362</td><td><pre><code class="language-r">        stop(
          &quot;Could not derive EDGAR time for `.by_time` summarization. &quot;,
          &quot;Provide data with explicit time in layer metadata.\n&quot;
        )</code></pre></td><td><pre><code class="language-r">        stop(
          &quot;Could not derive EDGAR time for `.by_time` summarization. &quot;,
          &quot;Provide data with explicit time in layer metadata.\n&quot;
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>4449–4449</td><td><pre><code class="language-r">    stop(&quot;`from` must be a SpatRaster object.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`from` must be a SpatRaster object.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>4563–4563</td><td><pre><code class="language-r">    stop(&quot;`from` must be the output of process_huc().&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`from` must be the output of process_huc().&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5036–5036</td><td><pre><code class="language-r">    stop(&quot;`from` must be a SpatRaster (SPEI/EDDI) or SpatVector (USDM).\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`from` must be a SpatRaster (SPEI/EDDI) or SpatVector (USDM).\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5117–5117</td><td><pre><code class="language-r">    stop(&quot;`geom` must be FALSE, TRUE, &#x27;sf&#x27;, or &#x27;terra&#x27;.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`geom` must be FALSE, TRUE, &#x27;sf&#x27;, or &#x27;terra&#x27;.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5130–5130</td><td><pre><code class="language-r">    stop(&quot;`locs_id` values must be unique and nonmissing.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`locs_id` values must be unique and nonmissing.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5134–5134</td><td><pre><code class="language-r">    stop(&quot;`locs_id` conflicts with a calculation output column.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`locs_id` conflicts with a calculation output column.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5139–5139</td><td><pre><code class="language-r">    stop(&quot;`from` must be processed IMPROVE spatial data or a data.frame.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`from` must be processed IMPROVE spatial data or a data.frame.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5155–5155</td><td><pre><code class="language-r">    stop(&quot;`FactDate` must contain valid, nonmissing dates.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`FactDate` must contain valid, nonmissing dates.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5166–5166</td><td><pre><code class="language-r">      stop(&quot;`Units` must be consistent within each ParamCode.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;`Units` must be consistent within each ParamCode.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5174–5174</td><td><pre><code class="language-r">      stop(&quot;`from` must have a CRS and point geometry.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;`from` must have a CRS and point geometry.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5178–5178</td><td><pre><code class="language-r">      stop(&quot;Tabular `from` must contain Longitude and Latitude.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;Tabular `from` must contain Longitude and Latitude.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5183–5183</td><td><pre><code class="language-r">      stop(&quot;Longitude and Latitude must be finite numeric coordinates.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;Longitude and Latitude must be finite numeric coordinates.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>5195–5195</td><td><pre><code class="language-r">    stop(&quot;`locs` must contain point or polygon geometry.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`locs` must contain point or polygon geometry.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>32–32</td><td><pre><code class="language-r">  stopifnot(class(from) %in% c(&quot;data.frame&quot;, &quot;SpatVector&quot;))</code></pre></td><td><pre><code class="language-r">  stopifnot(class(from) %in% c(&quot;data.frame&quot;, &quot;SpatVector&quot;))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>88–88</td><td><pre><code class="language-r">  stopifnot(dataset %in% datasets)</code></pre></td><td><pre><code class="language-r">  stopifnot(dataset %in% datasets)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>259–259</td><td><pre><code class="language-r">    stop(sprintf(&quot;locs should include columns named %s.\n&quot;, locs_id))</code></pre></td><td><pre><code class="language-r">    stop(sprintf(&quot;locs should include columns named %s.\n&quot;, locs_id))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>281–286</td><td><pre><code class="language-r">      stop(
        paste0(
          &quot;`locs_id` was not retained in prepared locations and could not &quot;,
          &quot;be reconstructed because row counts differ.&quot;
        )
      )</code></pre></td><td><pre><code class="language-r">      stop(
        paste0(
          &quot;`locs_id` was not retained in prepared locations and could not &quot;,
          &quot;be reconstructed because row counts differ.&quot;
        )
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>331–331</td><td><pre><code class="language-r">    stop(&quot;`from` must be a SpatRaster when `weights` are supplied.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`from` must be a SpatRaster when `weights` are supplied.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>336–339</td><td><pre><code class="language-r">      stop(
        &quot;`weights` vector input must contain polygons when supplied as &quot;,
        &quot;SpatVector/sf.&quot;
      )</code></pre></td><td><pre><code class="language-r">      stop(
        &quot;`weights` vector input must contain polygons when supplied as &quot;,
        &quot;SpatVector/sf.&quot;
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>343–343</td><td><pre><code class="language-r">      stop(&quot;`from` is missing CRS; cannot validate weighted extraction CRS.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;`from` is missing CRS; cannot validate weighted extraction CRS.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>364–364</td><td><pre><code class="language-r">      stop(&quot;`weights` values must be non-negative.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;`weights` values must be non-negative.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>374–374</td><td><pre><code class="language-r">      stop(&quot;`weights` polygons do not overlap `from` extent.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;`weights` polygons do not overlap `from` extent.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>385–385</td><td><pre><code class="language-r">        stop(&quot;`weights` path could not be read as raster or vector data.&quot;)</code></pre></td><td><pre><code class="language-r">        stop(&quot;`weights` path could not be read as raster or vector data.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>398–401</td><td><pre><code class="language-r">    stop(
      &quot;`weights` must be NULL, SpatRaster, polygon SpatVector/sf, &quot;,
      &quot;or a file path to one of those.&quot;
    )</code></pre></td><td><pre><code class="language-r">    stop(
      &quot;`weights` must be NULL, SpatRaster, polygon SpatVector/sf, &quot;,
      &quot;or a file path to one of those.&quot;
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>404–404</td><td><pre><code class="language-r">    stop(&quot;`weights` raster must have exactly one layer.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`weights` raster must have exactly one layer.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>416–416</td><td><pre><code class="language-r">    stop(&quot;`from` is missing CRS; cannot validate weighted extraction CRS.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`from` is missing CRS; cannot validate weighted extraction CRS.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>419–419</td><td><pre><code class="language-r">    stop(&quot;`weights` is missing CRS; cannot validate weighted extraction CRS.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`weights` is missing CRS; cannot validate weighted extraction CRS.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>424–424</td><td><pre><code class="language-r">    stop(&quot;`weights` extent does not overlap `from` extent.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`weights` extent does not overlap `from` extent.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>441–441</td><td><pre><code class="language-r">    stop(&quot;Unsupported location geometry for weighted extraction.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;Unsupported location geometry for weighted extraction.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>555–562</td><td><pre><code class="language-r">    stop(
      sprintf(
        &quot;Unable to parse date for dataset &#x27;%s&#x27; from token &#x27;%s&#x27; (layer &#x27;%s&#x27;).\n&quot;,
        ifelse(is.null(dataset), &quot;unknown&quot;, dataset),
        paste(time, collapse = &quot;_&quot;),
        ifelse(is.null(layer_name), &quot;unknown&quot;, layer_name)
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      sprintf(
        &quot;Unable to parse date for dataset &#x27;%s&#x27; from token &#x27;%s&#x27; (layer &#x27;%s&#x27;).\n&quot;,
        ifelse(is.null(dataset), &quot;unknown&quot;, dataset),
        paste(time, collapse = &quot;_&quot;),
        ifelse(is.null(layer_name), &quot;unknown&quot;, layer_name)
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>601–611</td><td><pre><code class="language-r">    stop(
      sprintf(
        paste0(
          &quot;Unable to parse datetime for dataset &#x27;%s&#x27; from token &#x27;%s&#x27; &quot;,
          &quot;(layer &#x27;%s&#x27;).\n&quot;
        ),
        ifelse(is.null(dataset), &quot;unknown&quot;, dataset),
        paste(time, collapse = &quot;_&quot;),
        ifelse(is.null(layer_name), &quot;unknown&quot;, layer_name)
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      sprintf(
        paste0(
          &quot;Unable to parse datetime for dataset &#x27;%s&#x27; from token &#x27;%s&#x27; &quot;,
          &quot;(layer &#x27;%s&#x27;).\n&quot;
        ),
        ifelse(is.null(dataset), &quot;unknown&quot;, dataset),
        paste(time, collapse = &quot;_&quot;),
        ifelse(is.null(layer_name), &quot;unknown&quot;, layer_name)
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>619–629</td><td><pre><code class="language-r">    stop(
      sprintf(
        paste0(
          &quot;Unable to parse year-month for dataset &#x27;%s&#x27; from token &#x27;%s&#x27; &quot;,
          &quot;(layer &#x27;%s&#x27;).\n&quot;
        ),
        ifelse(is.null(dataset), &quot;unknown&quot;, dataset),
        paste(time, collapse = &quot;_&quot;),
        ifelse(is.null(layer_name), &quot;unknown&quot;, layer_name)
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      sprintf(
        paste0(
          &quot;Unable to parse year-month for dataset &#x27;%s&#x27; from token &#x27;%s&#x27; &quot;,
          &quot;(layer &#x27;%s&#x27;).\n&quot;
        ),
        ifelse(is.null(dataset), &quot;unknown&quot;, dataset),
        paste(time, collapse = &quot;_&quot;),
        ifelse(is.null(layer_name), &quot;unknown&quot;, layer_name)
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>637–644</td><td><pre><code class="language-r">    stop(
      sprintf(
        &quot;Unable to parse year for dataset &#x27;%s&#x27; from token &#x27;%s&#x27; (layer &#x27;%s&#x27;).\n&quot;,
        ifelse(is.null(dataset), &quot;unknown&quot;, dataset),
        paste(time, collapse = &quot;_&quot;),
        ifelse(is.null(layer_name), &quot;unknown&quot;, layer_name)
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      sprintf(
        &quot;Unable to parse year for dataset &#x27;%s&#x27; from token &#x27;%s&#x27; (layer &#x27;%s&#x27;).\n&quot;,
        ifelse(is.null(dataset), &quot;unknown&quot;, dataset),
        paste(time, collapse = &quot;_&quot;),
        ifelse(is.null(layer_name), &quot;unknown&quot;, layer_name)
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>647–647</td><td><pre><code class="language-r">  stop(&quot;Unsupported time format.\n&quot;)</code></pre></td><td><pre><code class="language-r">  stop(&quot;Unsupported time format.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>665–665</td><td><pre><code class="language-r">  stopifnot(methods::is(covar, &quot;data.frame&quot;))</code></pre></td><td><pre><code class="language-r">  stopifnot(methods::is(covar, &quot;data.frame&quot;))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>668–668</td><td><pre><code class="language-r">      stopifnot(all(sapply(covar$time, methods::is, &quot;POSIXt&quot;)))</code></pre></td><td><pre><code class="language-r">      stopifnot(all(sapply(covar$time, methods::is, &quot;POSIXt&quot;)))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>670–670</td><td><pre><code class="language-r">      stopifnot(all(sapply(covar$time, methods::is, &quot;integer&quot;)))</code></pre></td><td><pre><code class="language-r">      stopifnot(all(sapply(covar$time, methods::is, &quot;integer&quot;)))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1037–1044</td><td><pre><code class="language-r">    stop(
      sprintf(
        paste0(
          &quot;`fun_temporal` must be one of: %s.\n&quot;
        ),
        paste(allowed, collapse = &quot;, &quot;)
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      sprintf(
        paste0(
          &quot;`fun_temporal` must be one of: %s.\n&quot;
        ),
        paste(allowed, collapse = &quot;, &quot;)
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1054–1060</td><td><pre><code class="language-r">  stop(
    paste0(
      &quot;`.by` is no longer supported in calculate APIs. &quot;,
      &quot;Use `.by_time` for temporal summarization (e.g., &#x27;day&#x27;, &quot;,
      &quot;&#x27;week&#x27;, &#x27;month&#x27;, or &#x27;year&#x27;).\n&quot;
    )
  )</code></pre></td><td><pre><code class="language-r">  stop(
    paste0(
      &quot;`.by` is no longer supported in calculate APIs. &quot;,
      &quot;Use `.by_time` for temporal summarization (e.g., &#x27;day&#x27;, &quot;,
      &quot;&#x27;week&#x27;, &#x27;month&#x27;, or &#x27;year&#x27;).\n&quot;
    )
  )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1132–1139</td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;`.by_time` must be one of: &quot;,
        paste(allowed, collapse = &quot;, &quot;),
        &quot;.
&quot;
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;`.by_time` must be one of: &quot;,
        paste(allowed, collapse = &quot;, &quot;),
        &quot;.
&quot;
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1202–1205</td><td><pre><code class="language-r">      stop(
        &quot;Unable to bucket time values. Provide parseable Date/POSIXct values &quot;,
        &quot;or recognized numeric encodings (YYYYDDD, YYYYMMDD, YYYYMM, YYYY).\n&quot;
      )</code></pre></td><td><pre><code class="language-r">      stop(
        &quot;Unable to bucket time values. Provide parseable Date/POSIXct values &quot;,
        &quot;or recognized numeric encodings (YYYYDDD, YYYYMMDD, YYYYMM, YYYY).\n&quot;
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1251–1251</td><td><pre><code class="language-r">  stopifnot(is.data.frame(covar))</code></pre></td><td><pre><code class="language-r">  stopifnot(is.data.frame(covar))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1274–1274</td><td><pre><code class="language-r">      stop(&quot;`fun_summary` must be a single function name.\n&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;`fun_summary` must be a single function name.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1280–1280</td><td><pre><code class="language-r">    stop(&quot;`fun_summary` must be a character string or function.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`fun_summary` must be a character string or function.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1291–1291</td><td><pre><code class="language-r">    stop(&quot;No numeric covariate columns found to summarize.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;No numeric covariate columns found to summarize.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1336–1336</td><td><pre><code class="language-r">  stopifnot(is.data.frame(covar))</code></pre></td><td><pre><code class="language-r">  stopifnot(is.data.frame(covar))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1356–1356</td><td><pre><code class="language-r">      stop(&quot;`fun_summary` must be a single function name.\n&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;`fun_summary` must be a single function name.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1362–1362</td><td><pre><code class="language-r">    stop(&quot;`fun_summary` must be a character string or function.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`fun_summary` must be a character string or function.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1369–1369</td><td><pre><code class="language-r">    stop(&quot;No numeric covariate columns found to summarize.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;No numeric covariate columns found to summarize.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1483–1483</td><td><pre><code class="language-r">  stopifnot(is.data.frame(covar))</code></pre></td><td><pre><code class="language-r">  stopifnot(is.data.frame(covar))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1516–1518</td><td><pre><code class="language-r">    stop(paste0(
      &quot;No covariate columns found in `covar` to summarize.&quot;
    ))</code></pre></td><td><pre><code class="language-r">    stop(paste0(
      &quot;No covariate columns found in `covar` to summarize.&quot;
    ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1647–1647</td><td><pre><code class="language-r">      stop(&quot;locs should be a SpatVector or convertible object.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;locs should be a SpatVector or convertible object.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1651–1651</td><td><pre><code class="language-r">    stop(sprintf(&quot;locs should include columns named %s.\n&quot;, locs_id))</code></pre></td><td><pre><code class="language-r">    stop(sprintf(&quot;locs should include columns named %s.\n&quot;, locs_id))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1803–1803</td><td><pre><code class="language-r">    stopifnot(!is.null(locs))</code></pre></td><td><pre><code class="language-r">    stopifnot(!is.null(locs))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>188–197</td><td><pre><code class="language-r">        stop(
          paste0(
            e,
            &quot;\n&quot;,
            paste0(deparse(args(what_to_run)), collapse = &quot;\n&quot;),
            &quot;\n&quot;,
            &quot;Please refer to the argument list and &quot;,
            &quot;the error message above to rectify the error.\n&quot;
          )
        )</code></pre></td><td><pre><code class="language-r">        stop(
          paste0(
            e,
            &quot;\n&quot;,
            paste0(deparse(args(what_to_run)), collapse = &quot;\n&quot;),
            &quot;\n&quot;,
            &quot;Please refer to the argument list and &quot;,
            &quot;the error message above to rectify the error.\n&quot;
          )
        )</code></pre></td><td>Keep existing — Existing error propagation/context wrapping, not an input validator; retain the underlying condition and wrapper diagnostics.</td></tr>
<tr><td><code>R/download.R</code></td><td>356–359</td><td><pre><code class="language-r">      stop(paste0(
        &quot;Invalid year returns HTTP code 404. &quot;,
        &quot;Check `year` parameter.\n&quot;
      ))</code></pre></td><td><pre><code class="language-r">      stop(paste0(
        &quot;Invalid year returns HTTP code 404. &quot;,
        &quot;Check `year` parameter.\n&quot;
      ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>710–714</td><td><pre><code class="language-r">            stop(paste0(
              download_url,
              &quot; Invalid date returns HTTP code 404. &quot;,
              &quot;Check `date` parameter.\n&quot;
            ))</code></pre></td><td><pre><code class="language-r">            stop(paste0(
              download_url,
              &quot; Invalid date returns HTTP code 404. &quot;,
              &quot;Check `date` parameter.\n&quot;
            ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>1116–1119</td><td><pre><code class="language-r">    stop(paste0(
      &quot;Requested collection is not recognized.\n&quot;,
      &quot;Please refer to the table above to find a proper collection.\n&quot;
    ))</code></pre></td><td><pre><code class="language-r">    stop(paste0(
      &quot;Requested collection is not recognized.\n&quot;,
      &quot;Please refer to the table above to find a proper collection.\n&quot;
    ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>1240–1243</td><td><pre><code class="language-r">          stop(paste0(
            &quot;Invalid date returns HTTP code 404. &quot;,
            &quot;Check `date` parameter.\n&quot;
          ))</code></pre></td><td><pre><code class="language-r">          stop(paste0(
            &quot;Invalid date returns HTTP code 404. &quot;,
            &quot;Check `date` parameter.\n&quot;
          ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>1764–1764</td><td><pre><code class="language-r">    stop(paste0(&quot;Requested year is not recognized.\n&quot;))</code></pre></td><td><pre><code class="language-r">    stop(paste0(&quot;Requested year is not recognized.\n&quot;))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>2311–2311</td><td><pre><code class="language-r">    stop(&quot;NOAA HMS wildfire smoke data begins at August 05, 2005.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;NOAA HMS wildfire smoke data begins at August 05, 2005.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>2337–2340</td><td><pre><code class="language-r">    stop(paste0(
      &quot;Arguments unzip = FALSE and remove_zip = TRUE are not &quot;,
      &quot;acceptable together. Please change one.\n&quot;
    ))</code></pre></td><td><pre><code class="language-r">    stop(paste0(
      &quot;Arguments unzip = FALSE and remove_zip = TRUE are not &quot;,
      &quot;acceptable together. Please change one.\n&quot;
    ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>2406–2416</td><td><pre><code class="language-r">        stop(
          sprintf(
            paste0(
              &quot;Failed to check HMS data availability for %s. &quot;,
              &quot;%s\n&quot;
            ),
            date_sequence[f],
            availability$error
          ),
          call. = FALSE
        )</code></pre></td><td><pre><code class="language-r">        stop(
          sprintf(
            paste0(
              &quot;Failed to check HMS data availability for %s. &quot;,
              &quot;%s\n&quot;
            ),
            date_sequence[f],
            availability$error
          ),
          call. = FALSE
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>2425–2431</td><td><pre><code class="language-r">      stop(
        sprintf(
          &quot;No HMS data are available for the requested date(s): %s.\n&quot;,
          paste(unavailable_dates, collapse = &quot;, &quot;)
        ),
        call. = FALSE
      )</code></pre></td><td><pre><code class="language-r">      stop(
        sprintf(
          &quot;No HMS data are available for the requested date(s): %s.\n&quot;,
          paste(unavailable_dates, collapse = &quot;, &quot;)
        ),
        call. = FALSE
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>2855–2855</td><td><pre><code class="language-r">      stop(&quot;dates should be in the same year.\n&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;dates should be in the same year.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>2922–2927</td><td><pre><code class="language-r">        stop(
          &quot;Failed to query NASA CMR (cmr.earthdata.nasa.gov). &quot;,
          &quot;Check network connectivity and NASA EarthData status at &quot;,
          &quot;https://status.earthdata.nasa.gov. Original error: &quot;,
          conditionMessage(e)
        )</code></pre></td><td><pre><code class="language-r">        stop(
          &quot;Failed to query NASA CMR (cmr.earthdata.nasa.gov). &quot;,
          &quot;Check network connectivity and NASA EarthData status at &quot;,
          &quot;https://status.earthdata.nasa.gov. Original error: &quot;,
          conditionMessage(e)
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>2978–2978</td><td><pre><code class="language-r">    stop(&quot;No granules found for the specified query parameters.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;No granules found for the specified query parameters.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>2984–2984</td><td><pre><code class="language-r">    stop(&quot;No granules matched the requested date range.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;No granules matched the requested date range.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>3138–3141</td><td><pre><code class="language-r">    stop(
      &quot;`jurisdiction` must be \&quot;US\&quot;, a two-letter state code, or \&quot;tbl\&quot;.\n&quot;,
      call. = FALSE
    )</code></pre></td><td><pre><code class="language-r">    stop(
      &quot;`jurisdiction` must be \&quot;US\&quot;, a two-letter state code, or \&quot;tbl\&quot;.\n&quot;,
      call. = FALSE
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>3158–3162</td><td><pre><code class="language-r">    stop(
      &quot;`jurisdiction` must be \&quot;US\&quot;, a two-letter state code such as &quot;,
      &quot;\&quot;AZ\&quot;, or \&quot;tbl\&quot;.\n&quot;,
      call. = FALSE
    )</code></pre></td><td><pre><code class="language-r">    stop(
      &quot;`jurisdiction` must be \&quot;US\&quot;, a two-letter state code such as &quot;,
      &quot;\&quot;AZ\&quot;, or \&quot;tbl\&quot;.\n&quot;,
      call. = FALSE
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>3351–3357</td><td><pre><code class="language-r">    stop(paste0(
      &quot;NEI data is not available for year(s): &quot;,
      paste(unknown, collapse = &quot;, &quot;),
      &quot;. Available years: &quot;,
      paste(names(nei_url_map), collapse = &quot;, &quot;),
      &quot;.\n&quot;
    ))</code></pre></td><td><pre><code class="language-r">    stop(paste0(
      &quot;NEI data is not available for year(s): &quot;,
      paste(unknown, collapse = &quot;, &quot;),
      &quot;. Available years: &quot;,
      paste(names(nei_url_map), collapse = &quot;, &quot;),
      &quot;.\n&quot;
    ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>3518–3518</td><td><pre><code class="language-r">    stop(&quot;years should be 4-digit integers.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;years should be 4-digit integers.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>3697–3697</td><td><pre><code class="language-r">    stop(&quot;years should be 4-digit integers.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;years should be 4-digit integers.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>4105–4105</td><td><pre><code class="language-r">    stop(&quot;Output &#x27;flux&#x27; is only supported for format &#x27;nc&#x27;.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;Output &#x27;flux&#x27; is only supported for format &#x27;nc&#x27;.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>4108–4108</td><td><pre><code class="language-r">    stop(&quot;Monthly resolution is only supported for format &#x27;nc&#x27;.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;Monthly resolution is only supported for format &#x27;nc&#x27;.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>4117–4117</td><td><pre><code class="language-r">      stop(&quot;year_range must be of length 1 or 2&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;year_range must be of length 1 or 2&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>4276–4276</td><td><pre><code class="language-r">      stop(&quot;Unsupported temp_res value&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;Unsupported temp_res value&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>4337–4337</td><td><pre><code class="language-r">    stop(&quot;Unsupported version&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;Unsupported version&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>4384–4384</td><td><pre><code class="language-r">    stop(&quot;No valid URLs were constructed.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;No valid URLs were constructed.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>4548–4548</td><td><pre><code class="language-r">      stop(&quot;sol* elements are not available for &#x27;ts&#x27; data type.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;sol* elements are not available for &#x27;ts&#x27; data type.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>4648–4648</td><td><pre><code class="language-r">            stop(conditionMessage(w), call. = FALSE)</code></pre></td><td><pre><code class="language-r">            stop(conditionMessage(w), call. = FALSE)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>4662–4673</td><td><pre><code class="language-r">      stop(
        sprintf(
          &quot;Downloaded PRISM archive &#x27;%s&#x27; is not a valid zip file.%s&quot;,
          basename(zip_file),
          if (nzchar(preview)) {
            paste0(&quot; Response preview: &quot;, preview)
          } else {
            &quot;&quot;
          }
        ),
        call. = FALSE
      )</code></pre></td><td><pre><code class="language-r">      stop(
        sprintf(
          &quot;Downloaded PRISM archive &#x27;%s&#x27; is not a valid zip file.%s&quot;,
          basename(zip_file),
          if (nzchar(preview)) {
            paste0(&quot; Response preview: &quot;, preview)
          } else {
            &quot;&quot;
          }
        ),
        call. = FALSE
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>4767–4767</td><td><pre><code class="language-r">    stop(&quot;Year should be equal to or greater than 1997.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;Year should be equal to or greater than 1997.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>4770–4770</td><td><pre><code class="language-r">    stop(&quot;Year should be equal to or greater than 2008.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;Year should be equal to or greater than 2008.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>5359–5362</td><td><pre><code class="language-r">    stop(
      &quot;Arguments unzip = FALSE and remove_zip = TRUE are not &quot;,
      &quot;acceptable together. Please change one.\n&quot;
    )</code></pre></td><td><pre><code class="language-r">    stop(
      &quot;Arguments unzip = FALSE and remove_zip = TRUE are not &quot;,
      &quot;acceptable together. Please change one.\n&quot;
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>5451–5458</td><td><pre><code class="language-r">      stop(sprintf(
        paste0(
          &quot;SPEI timescale %s could not be downloaded. Check upstream &quot;,
          &quot;availability and `timescale` parameter.\nLast error: %s\n&quot;
        ),
        ts_str,
        if (is.null(last_error)) &quot;none&quot; else last_error
      ))</code></pre></td><td><pre><code class="language-r">      stop(sprintf(
        paste0(
          &quot;SPEI timescale %s could not be downloaded. Check upstream &quot;,
          &quot;availability and `timescale` parameter.\nLast error: %s\n&quot;
        ),
        ts_str,
        if (is.null(last_error)) &quot;none&quot; else last_error
      ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>5477–5480</td><td><pre><code class="language-r">      stop(
        &quot;No Tuesday dates found in the specified date range. &quot;,
        &quot;EDDI is a weekly (Tuesday) product.\n&quot;
      )</code></pre></td><td><pre><code class="language-r">      stop(
        &quot;No Tuesday dates found in the specified date range. &quot;,
        &quot;EDDI is a weekly (Tuesday) product.\n&quot;
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>5548–5551</td><td><pre><code class="language-r">      stop(
        &quot;No Tuesday dates found in the specified date range. &quot;,
        &quot;USDM is a weekly (Tuesday) product.\n&quot;
      )</code></pre></td><td><pre><code class="language-r">      stop(
        &quot;No Tuesday dates found in the specified date range. &quot;,
        &quot;USDM is a weekly (Tuesday) product.\n&quot;
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download.R</code></td><td>5579–5582</td><td><pre><code class="language-r">      stop(
        &quot;First USDM URL returned HTTP 404. &quot;,
        &quot;Check `date` parameter.\n&quot;
      )</code></pre></td><td><pre><code class="language-r">      stop(
        &quot;First USDM URL returned HTTP 404. &quot;,
        &quot;Check `date` parameter.\n&quot;
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>139–139</td><td><pre><code class="language-r">      stop(&quot;Unsupported MODIS temporal scale.\n&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;Unsupported MODIS temporal scale.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>171–171</td><td><pre><code class="language-r">    stop(&quot;MODIS paths contain mixed or unsupported temporal patterns.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;MODIS paths contain mixed or unsupported temporal patterns.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>244–244</td><td><pre><code class="language-r">        stop(sprintf(&quot;Token file &#x27;%s&#x27; is empty.\n&quot;, token))</code></pre></td><td><pre><code class="language-r">        stop(sprintf(&quot;Token file &#x27;%s&#x27; is empty.\n&quot;, token))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>259–280</td><td><pre><code class="language-r">  stop(
    &quot;No authentication token found. Please provide a token using one of:\n&quot;,
    sprintf(
      &quot;  1. Set environment variable: Sys.setenv(%s = &#x27;your_token&#x27;)\n&quot;,
      env_var
    ),
    &quot;  2. Create ~/.nasa_earthdata_token file with your token\n&quot;,
    paste0(
      &quot;  3. Pass token file path:&quot;,
      &quot; nasa_earth_data_token = &#x27;~/.nasa_earthdata_token&#x27;\n&quot;
    ),
    &quot;  4. Pass token directly:&quot;,
    &quot; nasa_earth_data_token = &#x27;your_token&#x27; (not recommended)\n&quot;,
    sprintf(
      paste0(
        &quot;\nTo set up for all R sessions,&quot;,
        &quot; add to ~/.Renviron:\n  %s=your_token_here\n&quot;
      ),
      env_var
    ),
    call. = FALSE
  )</code></pre></td><td><pre><code class="language-r">  stop(
    &quot;No authentication token found. Please provide a token using one of:\n&quot;,
    sprintf(
      &quot;  1. Set environment variable: Sys.setenv(%s = &#x27;your_token&#x27;)\n&quot;,
      env_var
    ),
    &quot;  2. Create ~/.nasa_earthdata_token file with your token\n&quot;,
    paste0(
      &quot;  3. Pass token file path:&quot;,
      &quot; nasa_earth_data_token = &#x27;~/.nasa_earthdata_token&#x27;\n&quot;
    ),
    &quot;  4. Pass token directly:&quot;,
    &quot; nasa_earth_data_token = &#x27;your_token&#x27; (not recommended)\n&quot;,
    sprintf(
      paste0(
        &quot;\nTo set up for all R sessions,&quot;,
        &quot; add to ~/.Renviron:\n  %s=your_token_here\n&quot;
      ),
      env_var
    ),
    call. = FALSE
  )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>650–650</td><td><pre><code class="language-r">      stop(&quot;Unsupported archive format. Expected .zip or .7z.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;Unsupported archive format. Expected .zip or .7z.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>704–707</td><td><pre><code class="language-r">        stop(sprintf(
          &quot;Failed to normalize AQS unzip layout for %s.&quot;,
          basename(nested_file)
        ))</code></pre></td><td><pre><code class="language-r">        stop(sprintf(
          &quot;Failed to normalize AQS unzip layout for %s.&quot;,
          basename(nested_file)
        ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>765–765</td><td><pre><code class="language-r">      stop(paste0(&quot;One or more parameters are NULL\n&quot;))</code></pre></td><td><pre><code class="language-r">      stop(paste0(&quot;One or more parameters are NULL\n&quot;))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>1163–1169</td><td><pre><code class="language-r">      stop(
        paste0(
          &quot;Selected variable \&quot;&quot;,
          variable,
          &quot;\&quot; is not available.\n&quot;
        )
      )</code></pre></td><td><pre><code class="language-r">      stop(
        paste0(
          &quot;Selected variable \&quot;&quot;,
          variable,
          &quot;\&quot; is not available.\n&quot;
        )
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>1255–1255</td><td><pre><code class="language-r">      stop(&quot;Token must be provided in non-interactive mode.\n&quot;, call. = FALSE)</code></pre></td><td><pre><code class="language-r">      stop(&quot;Token must be provided in non-interactive mode.\n&quot;, call. = FALSE)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>1260–1260</td><td><pre><code class="language-r">    stop(&quot;Token cannot be empty.\n&quot;, call. = FALSE)</code></pre></td><td><pre><code class="language-r">    stop(&quot;Token cannot be empty.\n&quot;, call. = FALSE)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>1369–1371</td><td><pre><code class="language-r">  stopifnot(
    &quot;sn_bound_10deg.txt not found in inst/extdata/&quot; = nzchar(bounds_file)
  )</code></pre></td><td><pre><code class="language-r">  stopifnot(
    &quot;sn_bound_10deg.txt not found in inst/extdata/&quot; = nzchar(bounds_file)
  )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>58–58</td><td><pre><code class="language-r">  stopifnot(&quot;x is not a sftime&quot; = class(x)[1] == &quot;sftime&quot;)</code></pre></td><td><pre><code class="language-r">  stopifnot(&quot;x is not a sftime&quot; = class(x)[1] == &quot;sftime&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>84–84</td><td><pre><code class="language-r">  stopifnot(&quot;x is not a data.table&quot; = class(x)[1] == &quot;data.table&quot;)</code></pre></td><td><pre><code class="language-r">  stopifnot(&quot;x is not a data.table&quot; = class(x)[1] == &quot;data.table&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>113–113</td><td><pre><code class="language-r">    stop(&quot;time column missing or mispelled&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;time column missing or mispelled&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>134–134</td><td><pre><code class="language-r">  stopifnot(&quot;timename column missing or mispelled&quot; = timename %in% names(x))</code></pre></td><td><pre><code class="language-r">  stopifnot(&quot;timename column missing or mispelled&quot; = timename %in% names(x))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>166–166</td><td><pre><code class="language-r">  stopifnot(&quot;x layers might not be time&quot; = date_correct)</code></pre></td><td><pre><code class="language-r">  stopifnot(&quot;x layers might not be time&quot; = date_correct)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>198–201</td><td><pre><code class="language-r">  stopifnot(
    &quot;x is not a SpatRasterDataset&quot; =
      class(x)[1] == &quot;SpatRasterDataset&quot;
  )</code></pre></td><td><pre><code class="language-r">  stopifnot(
    &quot;x is not a SpatRasterDataset&quot; =
      class(x)[1] == &quot;SpatRasterDataset&quot;
  )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>272–272</td><td><pre><code class="language-r">        stop(&quot;time series differ from 1 variable to the other&quot;)</code></pre></td><td><pre><code class="language-r">        stop(&quot;time series differ from 1 variable to the other&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>290–290</td><td><pre><code class="language-r">    stop(&quot;x class not accepted&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;x class not accepted&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>308–308</td><td><pre><code class="language-r">  stopifnot(&quot;x is not a sftime&quot; = class(x)[1] == &quot;sftime&quot;)</code></pre></td><td><pre><code class="language-r">  stopifnot(&quot;x is not a sftime&quot; = class(x)[1] == &quot;sftime&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>334–334</td><td><pre><code class="language-r">    stop(&quot;time column missing or mispelled&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;time column missing or mispelled&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>352–352</td><td><pre><code class="language-r">  stopifnot(&quot;x is not a sftime&quot; = class(x)[1] == &quot;sftime&quot;)</code></pre></td><td><pre><code class="language-r">  stopifnot(&quot;x is not a sftime&quot; = class(x)[1] == &quot;sftime&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>377–377</td><td><pre><code class="language-r">  stopifnot(&quot;varname missing or mispelled&quot; = varname %in% colnames(x))</code></pre></td><td><pre><code class="language-r">  stopifnot(&quot;varname missing or mispelled&quot; = varname %in% colnames(x))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>403–403</td><td><pre><code class="language-r">  stopifnot(&quot;x is not a sftime&quot; = class(x)[1] == &quot;sftime&quot;)</code></pre></td><td><pre><code class="language-r">  stopifnot(&quot;x is not a sftime&quot; = class(x)[1] == &quot;sftime&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>168–177</td><td><pre><code class="language-r">          stop(
            paste0(
              e,
              &quot;\n&quot;,
              paste0(deparse(args(what_to_run)), collapse = &quot;\n&quot;),
              &quot;\n&quot;,
              &quot;Please refer to the argument list and &quot;,
              &quot;the error message above to rectify the error.\n&quot;
            )
          )</code></pre></td><td><pre><code class="language-r">          stop(
            paste0(
              e,
              &quot;\n&quot;,
              paste0(deparse(args(what_to_run)), collapse = &quot;\n&quot;),
              &quot;\n&quot;,
              &quot;Please refer to the argument list and &quot;,
              &quot;the error message above to rectify the error.\n&quot;
            )
          )</code></pre></td><td>Keep existing — Existing error propagation/context wrapping, not an input validator; retain the underlying condition and wrapper diagnostics.</td></tr>
<tr><td><code>R/process.R</code></td><td>363–373</td><td><pre><code class="language-r">        stop(
          sprintf(
            paste0(
              &quot;No MODIS subdatasets matched `subdataset = \&quot;%s\&quot;` &quot;,
              &quot;in `%s`.\nAvailable subdatasets: %s\n&quot;
            ),
            subdataset,
            path,
            available_sds
          )
        )</code></pre></td><td><pre><code class="language-r">        stop(
          sprintf(
            paste0(
              &quot;No MODIS subdatasets matched `subdataset = \&quot;%s\&quot;` &quot;,
              &quot;in `%s`.\nAvailable subdatasets: %s\n&quot;
            ),
            subdataset,
            path,
            available_sds
          )
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>463–466</td><td><pre><code class="language-r">    stop(
      &quot;Argument fun_agg should be a function or name of a function
         that is accepted in terra::tapp.\n&quot;
    )</code></pre></td><td><pre><code class="language-r">    stop(
      &quot;Argument fun_agg should be a function or name of a function
         that is accepted in terra::tapp.\n&quot;
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>474–474</td><td><pre><code class="language-r">    stop(&quot;No MODIS files matched the requested date.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;No MODIS files matched the requested date.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>520–522</td><td><pre><code class="language-r">        stop(
          &quot;Primary and secondary MODIS rasters have incompatible geometry.\n&quot;
        )</code></pre></td><td><pre><code class="language-r">        stop(
          &quot;Primary and secondary MODIS rasters have incompatible geometry.\n&quot;
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>525–527</td><td><pre><code class="language-r">        stop(
          &quot;Primary and secondary MODIS rasters have different layer counts.\n&quot;
        )</code></pre></td><td><pre><code class="language-r">        stop(
          &quot;Primary and secondary MODIS rasters have different layer counts.\n&quot;
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>634–634</td><td><pre><code class="language-r">        stop(e)</code></pre></td><td><pre><code class="language-r">        stop(e)</code></pre></td><td>Keep existing — Existing error propagation/context wrapping, not an input validator; retain the underlying condition and wrapper diagnostics.</td></tr>
<tr><td><code>R/process.R</code></td><td>641–641</td><td><pre><code class="language-r">    stop(&quot;No MODIS files matched any day in the requested date range.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;No MODIS files matched any day in the requested date range.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>670–670</td><td><pre><code class="language-r">    stop(&quot;path is required.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;path is required.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>684–684</td><td><pre><code class="language-r">    stop(&quot;No MCD14ML text files were found.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;No MCD14ML text files were found.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>757–757</td><td><pre><code class="language-r">      stop(&quot;hrange or vrange are out of range.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;hrange or vrange are out of range.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1203–1203</td><td><pre><code class="language-r">      stop(&quot;path does not exist.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;path does not exist.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1259–1259</td><td><pre><code class="language-r">      stop(&quot;Duplicated NLCD files are detected. Please remove duplicates.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;Duplicated NLCD files are detected. Please remove duplicates.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1262–1262</td><td><pre><code class="language-r">      stop(&quot;NLCD data not available for this year.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;NLCD data not available for this year.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1511–1515</td><td><pre><code class="language-r">    stop(
      &quot;TRI input is missing required columns: &quot;,
      paste(missing_required, collapse = &quot;, &quot;),
      &quot;\n&quot;
    )</code></pre></td><td><pre><code class="language-r">    stop(
      &quot;TRI input is missing required columns: &quot;,
      paste(missing_required, collapse = &quot;, &quot;),
      &quot;\n&quot;
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1569–1569</td><td><pre><code class="language-r">    stop(&quot;`variables` did not match any TRI variable columns.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;`variables` did not match any TRI variable columns.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1581–1585</td><td><pre><code class="language-r">    stop(
      &quot;TRI input is missing industry grouping columns: &quot;,
      paste(missing_industry, collapse = &quot;, &quot;),
      &quot;\n&quot;
    )</code></pre></td><td><pre><code class="language-r">    stop(
      &quot;TRI input is missing industry grouping columns: &quot;,
      paste(missing_industry, collapse = &quot;, &quot;),
      &quot;\n&quot;
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1602–1602</td><td><pre><code class="language-r">    stop(&quot;No TRI rows found for requested `year`.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;No TRI rows found for requested `year`.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1620–1620</td><td><pre><code class="language-r">      stop(&quot;`chemical` did not match any TRI rows for requested year.\n&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;`chemical` did not match any TRI rows for requested year.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1628–1628</td><td><pre><code class="language-r">      stop(&quot;Selected TRI variable column `&quot;, col_nm, &quot;` is not numeric.\n&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;Selected TRI variable column `&quot;, col_nm, &quot;` is not numeric.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1694–1694</td><td><pre><code class="language-r">    stop(&quot;No TRI sites found after filtering missing/zero source values.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;No TRI sites found after filtering missing/zero source values.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1768–1768</td><td><pre><code class="language-r">    stop(&quot;county argument is required.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;county argument is required.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1773–1773</td><td><pre><code class="language-r">      stop(&quot;county is unable to be converted to SpatVector.\n&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;county is unable to be converted to SpatVector.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1777–1777</td><td><pre><code class="language-r">    stop(&quot;county should include a field named \&quot;GEOID\&quot;&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;county should include a field named \&quot;GEOID\&quot;&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1780–1780</td><td><pre><code class="language-r">    stop(&quot;year should be one of 2017 or 2020.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;year should be one of 2017 or 2020.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1792–1792</td><td><pre><code class="language-r">    stop(&quot;No files found for the year. The file names should include the year&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;No files found for the year. The file names should include the year&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1920–1920</td><td><pre><code class="language-r">        stop(&quot;date has invalid format(s). Please check the values.&quot;)</code></pre></td><td><pre><code class="language-r">        stop(&quot;date has invalid format(s). Please check the values.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1926–1926</td><td><pre><code class="language-r">          stop(&quot;date should be a character vector of length 1 or 2.&quot;)</code></pre></td><td><pre><code class="language-r">          stop(&quot;date should be a character vector of length 1 or 2.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1930–1930</td><td><pre><code class="language-r">      stop(&quot;date should be defined.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;date should be defined.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1941–1941</td><td><pre><code class="language-r">      stop(&quot;path does not contain csv files.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;path does not contain csv files.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1949–1949</td><td><pre><code class="language-r">      stop(&quot;No &quot;, resolution_temporal, &quot; AQS CSV files found in path.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;No &quot;, resolution_temporal, &quot; AQS CSV files found in path.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>1991–1991</td><td><pre><code class="language-r">        stop(&quot;Hourly AQS data contain invalid Date.Local or Time.Local.&quot;)</code></pre></td><td><pre><code class="language-r">        stop(&quot;Hourly AQS data contain invalid Date.Local or Time.Local.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>2178–2178</td><td><pre><code class="language-r">    stop(&quot;path does not contain files.&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;path does not contain files.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>2191–2194</td><td><pre><code class="language-r">      stop(
        &quot;process_edgar() currently supports gridded raster files only. &quot;,
        &quot;Re-download EDGAR with format = \&quot;nc\&quot; or provide extracted raster files.\n&quot;
      )</code></pre></td><td><pre><code class="language-r">      stop(
        &quot;process_edgar() currently supports gridded raster files only. &quot;,
        &quot;Re-download EDGAR with format = \&quot;nc\&quot; or provide extracted raster files.\n&quot;
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>2196–2196</td><td><pre><code class="language-r">    stop(&quot;path does not contain supported EDGAR raster files.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;path does not contain supported EDGAR raster files.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>2372–2377</td><td><pre><code class="language-r">    stop(
      sprintf(
        &quot;`path` does not exist: %s&quot;,
        path
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      sprintf(
        &quot;`path` does not exist: %s&quot;,
        path
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>2380–2385</td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;`path` must point to a .shp or .gdb file. Received: &quot;,
        path
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;`path` must point to a .shp or .gdb file. Received: &quot;,
        path
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>2655–2659</td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;Please provide a vector with the statistic and resolution.\n&quot;
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;Please provide a vector with the statistic and resolution.\n&quot;
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>3210–3221</td><td><pre><code class="language-r">        stop(
          paste0(
            &quot;Variable &#x27;&quot;,
            variable,
            &quot;&#x27; was not found in collection &quot;,
            collection,
            &quot;.\n&quot;,
            &quot;Available case-sensitive variables include: &quot;,
            paste(available_base, collapse = &quot;, &quot;),
            &quot;.\n&quot;
          )
        )</code></pre></td><td><pre><code class="language-r">        stop(
          paste0(
            &quot;Variable &#x27;&quot;,
            variable,
            &quot;&#x27; was not found in collection &quot;,
            collection,
            &quot;.\n&quot;,
            &quot;Available case-sensitive variables include: &quot;,
            paste(available_base, collapse = &quot;, &quot;),
            &quot;.\n&quot;
          )
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>3456–3456</td><td><pre><code class="language-r">      stop(&quot;No MERRA2 files matching the requested date were found.\n&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;No MERRA2 files matching the requested date were found.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>3506–3514</td><td><pre><code class="language-r">        stop(
          paste0(
            &quot;Requested variable &quot;,
            variable,
            &quot; was not found in &quot;,
            basename(data_paths[p]),
            &quot;.\n&quot;
          )
        )</code></pre></td><td><pre><code class="language-r">        stop(
          paste0(
            &quot;Requested variable &quot;,
            variable,
            &quot; was not found in &quot;,
            basename(data_paths[p]),
            &quot;.\n&quot;
          )
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>3760–3768</td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;No gridMET NetCDF files for variable &#x27;&quot;,
        variable_checked,
        &quot;&#x27; and requested year(s) were found in `path`: &quot;,
        path
      ),
      call. = FALSE
    )</code></pre></td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;No gridMET NetCDF files for variable &#x27;&quot;,
        variable_checked,
        &quot;&#x27; and requested year(s) were found in `path`: &quot;,
        path
      ),
      call. = FALSE
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>3835–3844</td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;No gridMET layers fall within the requested date range &quot;,
        date[1],
        &quot; to &quot;,
        date[2],
        &quot;.&quot;
      ),
      call. = FALSE
    )</code></pre></td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;No gridMET layers fall within the requested date range &quot;,
        date[1],
        &quot; to &quot;,
        date[2],
        &quot;.&quot;
      ),
      call. = FALSE
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>4130–4130</td><td><pre><code class="language-r">      stop(&quot;element is not a valid PRISM element.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;element is not a valid PRISM element.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>4136–4136</td><td><pre><code class="language-r">      stop(&quot;time does not have valid length.&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;time does not have valid length.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>4275–4279</td><td><pre><code class="language-r">        stop(
          &quot;Package &#x27;nhdplusTools&#x27; is required when fetching HUC data &quot;,
          &quot;remotely. &quot;,
          &quot;Please install it and try again.&quot;
        )</code></pre></td><td><pre><code class="language-r">        stop(
          &quot;Package &#x27;nhdplusTools&#x27; is required when fetching HUC data &quot;,
          &quot;remotely. &quot;,
          &quot;Please install it and try again.&quot;
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>4285–4285</td><td><pre><code class="language-r">        stop(&quot;HUC data was not found.&quot;)</code></pre></td><td><pre><code class="language-r">        stop(&quot;HUC data was not found.&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>4304–4311</td><td><pre><code class="language-r">        stop(
          paste0(
            &quot;Layer &quot;,
            layer_name,
            &quot; not found in &quot;,
            path
          )
        )</code></pre></td><td><pre><code class="language-r">        stop(
          paste0(
            &quot;Layer &quot;,
            layer_name,
            &quot; not found in &quot;,
            path
          )
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>4418–4426</td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;No GOES ADP NetCDF files found in `path`.\n&quot;,
        &quot;When `path` is a directory, files must match &quot;,
        &quot;&#x27;^OR_(ADP|ABI-L2-ADP).*\\.nc$&#x27;.\n&quot;,
        &quot;When `path` is a vector, provide full paths to matching GOES ADP &quot;,
        &quot;NetCDF files.\n&quot;
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;No GOES ADP NetCDF files found in `path`.\n&quot;,
        &quot;When `path` is a directory, files must match &quot;,
        &quot;&#x27;^OR_(ADP|ABI-L2-ADP).*\\.nc$&#x27;.\n&quot;,
        &quot;When `path` is a vector, provide full paths to matching GOES ADP &quot;,
        &quot;NetCDF files.\n&quot;
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>4430–4436</td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;Some GOES paths do not exist:\n&quot;,
        paste0(&quot;  - &quot;, missing_paths, collapse = &quot;\n&quot;),
        &quot;\nFiles must match pattern &#x27;^OR_(ADP|ABI-L2-ADP).*\\.nc$&#x27;.\n&quot;
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;Some GOES paths do not exist:\n&quot;,
        paste0(&quot;  - &quot;, missing_paths, collapse = &quot;\n&quot;),
        &quot;\nFiles must match pattern &#x27;^OR_(ADP|ABI-L2-ADP).*\\.nc$&#x27;.\n&quot;
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>4461–4468</td><td><pre><code class="language-r">    stop(paste0(
      &quot;No GOES ADP files matching the requested date range were found.\n&quot;,
      &quot;Date range: &quot;,
      date[1],
      &quot; to &quot;,
      date[2],
      &quot;\n&quot;
    ))</code></pre></td><td><pre><code class="language-r">    stop(paste0(
      &quot;No GOES ADP files matching the requested date range were found.\n&quot;,
      &quot;Date range: &quot;,
      date[1],
      &quot; to &quot;,
      date[2],
      &quot;\n&quot;
    ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>4480–4486</td><td><pre><code class="language-r">    stop(paste0(
      &quot;Requested variable &#x27;&quot;,
      variable,
      &quot;&#x27; was not found in &quot;,
      basename(data_paths[1]),
      &quot;.\n&quot;
    ))</code></pre></td><td><pre><code class="language-r">    stop(paste0(
      &quot;Requested variable &#x27;&quot;,
      variable,
      &quot;&#x27; was not found in &quot;,
      basename(data_paths[1]),
      &quot;.\n&quot;
    ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>4505–4510</td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;GOES files are not structurally homogeneous; &quot;,
        &quot;unable to map layers to files.\n&quot;
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;GOES files are not structurally homogeneous; &quot;,
        &quot;unable to map layers to files.\n&quot;
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>4514–4519</td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;GOES variable layer index exceeds per-file layer count; &quot;,
        &quot;files appear inconsistent.\n&quot;
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      paste0(
        &quot;GOES variable layer index exceeds per-file layer count; &quot;,
        &quot;files appear inconsistent.\n&quot;
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>4622–4626</td><td><pre><code class="language-r">    stop(paste0(
      &quot;Cannot parse GOES start datetime from filename: &quot;,
      fname,
      &quot;\n&quot;
    ))</code></pre></td><td><pre><code class="language-r">    stop(paste0(
      &quot;Cannot parse GOES start datetime from filename: &quot;,
      fname,
      &quot;\n&quot;
    ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>4735–4739</td><td><pre><code class="language-r">    stop(sprintf(
      &quot;No %s_YYYY.txt files found in &#x27;%s&#x27;.\n&quot;,
      prefix,
      path
    ))</code></pre></td><td><pre><code class="language-r">    stop(sprintf(
      &quot;No %s_YYYY.txt files found in &#x27;%s&#x27;.\n&quot;,
      prefix,
      path
    ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>5034–5038</td><td><pre><code class="language-r">      stop(sprintf(
        &quot;No SPEI file matching &#x27;%s&#x27; found in: %s&quot;,
        nc_pattern,
        path
      ))</code></pre></td><td><pre><code class="language-r">      stop(sprintf(
        &quot;No SPEI file matching &#x27;%s&#x27; found in: %s&quot;,
        nc_pattern,
        path
      ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>5050–5055</td><td><pre><code class="language-r">      stop(sprintf(
        &quot;No EDDI files matching &#x27;%s&#x27; or &#x27;%s&#x27; found in: %s&quot;,
        nc_pattern,
        asc_pattern,
        path
      ))</code></pre></td><td><pre><code class="language-r">      stop(sprintf(
        &quot;No EDDI files matching &#x27;%s&#x27; or &#x27;%s&#x27; found in: %s&quot;,
        nc_pattern,
        asc_pattern,
        path
      ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>5094–5099</td><td><pre><code class="language-r">    stop(sprintf(
      &quot;No %s data found in date range %s to %s.&quot;,
      toupper(source),
      date[1],
      date[2]
    ))</code></pre></td><td><pre><code class="language-r">    stop(sprintf(
      &quot;No %s data found in date range %s to %s.&quot;,
      toupper(source),
      date[1],
      date[2]
    ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>5132–5135</td><td><pre><code class="language-r">      stop(sprintf(
        &quot;Cannot determine time coordinates from file: %s&quot;,
        basename(filepath)
      ))</code></pre></td><td><pre><code class="language-r">      stop(sprintf(
        &quot;Cannot determine time coordinates from file: %s&quot;,
        basename(filepath)
      ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>5177–5180</td><td><pre><code class="language-r">    stop(sprintf(
      &quot;No USDM shapefiles matching &#x27;USDM_YYYYMMDD.shp&#x27; found in: %s&quot;,
      path
    ))</code></pre></td><td><pre><code class="language-r">    stop(sprintf(
      &quot;No USDM shapefiles matching &#x27;USDM_YYYYMMDD.shp&#x27; found in: %s&quot;,
      path
    ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process.R</code></td><td>5196–5200</td><td><pre><code class="language-r">    stop(sprintf(
      &quot;No USDM files found in date range %s to %s.&quot;,
      date[1],
      date[2]
    ))</code></pre></td><td><pre><code class="language-r">    stop(sprintf(
      &quot;No USDM files found in date range %s to %s.&quot;,
      date[1],
      date[2]
    ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>73–77</td><td><pre><code class="language-r">      stop(
        paste0(
          &quot;Select one of &#x27;collection&#x27;, &#x27;date&#x27;, or &#x27;datetime&#x27;.\n&quot;
        )
      )</code></pre></td><td><pre><code class="language-r">      stop(
        paste0(
          &quot;Select one of &#x27;collection&#x27;, &#x27;date&#x27;, or &#x27;datetime&#x27;.\n&quot;
        )
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>404–406</td><td><pre><code class="language-r">        stop(paste0(
          &quot;`locs` is missing &#x27;lon&#x27;, &#x27;lat&#x27;, or both.\n&quot;
        ))</code></pre></td><td><pre><code class="language-r">        stop(paste0(
          &quot;`locs` is missing &#x27;lon&#x27;, &#x27;lat&#x27;, or both.\n&quot;
        ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>415–419</td><td><pre><code class="language-r">      stop(
        paste0(
          &quot;`locs` is not a `SpatVector`, `sf`, or `data.frame` object.\n&quot;
        )
      )</code></pre></td><td><pre><code class="language-r">      stop(
        paste0(
          &quot;`locs` is not a `SpatVector`, `sf`, or `data.frame` object.\n&quot;
        )
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>596–600</td><td><pre><code class="language-r">        stop(
          paste0(
            &quot;Unable to identify requested variables.\n&quot;
          )
        )</code></pre></td><td><pre><code class="language-r">        stop(
          paste0(
            &quot;Unable to identify requested variables.\n&quot;
          )
        )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>610–610</td><td><pre><code class="language-r">    stop(&quot;No TRI CSV files found in `path`.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;No TRI CSV files found in `path`.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>661–661</td><td><pre><code class="language-r">      stop(&quot;TRI input is missing `YEAR` column needed for filtering.\n&quot;)</code></pre></td><td><pre><code class="language-r">      stop(&quot;TRI input is missing `YEAR` column needed for filtering.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>670–674</td><td><pre><code class="language-r">      stop(
        &quot;TRI input is missing required chemical lookup columns: &quot;,
        paste(missing_cols, collapse = &quot;, &quot;),
        &quot;\n&quot;
      )</code></pre></td><td><pre><code class="language-r">      stop(
        &quot;TRI input is missing required chemical lookup columns: &quot;,
        paste(missing_cols, collapse = &quot;, &quot;),
        &quot;\n&quot;
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>713–717</td><td><pre><code class="language-r">      stop(
        &quot;TRI input is missing required industry lookup columns: &quot;,
        paste(missing_cols, collapse = &quot;, &quot;),
        &quot;\n&quot;
      )</code></pre></td><td><pre><code class="language-r">      stop(
        &quot;TRI input is missing required industry lookup columns: &quot;,
        paste(missing_cols, collapse = &quot;, &quot;),
        &quot;\n&quot;
      )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>776–776</td><td><pre><code class="language-r">    stop(sprintf(&quot;No %s files were found in `path`.\n&quot;, source_name))</code></pre></td><td><pre><code class="language-r">    stop(sprintf(&quot;No %s files were found in `path`.\n&quot;, source_name))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>850–850</td><td><pre><code class="language-r">    stop(&quot;No GEOS collection-variable metadata could be derived from `path`.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;No GEOS collection-variable metadata could be derived from `path`.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>925–927</td><td><pre><code class="language-r">    stop(
      &quot;No MERRA2 collection-variable metadata could be derived from `path`.\n&quot;
    )</code></pre></td><td><pre><code class="language-r">    stop(
      &quot;No MERRA2 collection-variable metadata could be derived from `path`.\n&quot;
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>1016–1016</td><td><pre><code class="language-r">    stop(&quot;No MODIS product-subdataset metadata could be derived from `path`.\n&quot;)</code></pre></td><td><pre><code class="language-r">    stop(&quot;No MODIS product-subdataset metadata could be derived from `path`.\n&quot;)</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>1050–1054</td><td><pre><code class="language-r">    stop(sprintf(
      &quot;%s does not conform to the required format
         \&quot;YYYY-MM-DD\&quot;.\n&quot;,
      names(argnames)[2]
    ))</code></pre></td><td><pre><code class="language-r">    stop(sprintf(
      &quot;%s does not conform to the required format
         \&quot;YYYY-MM-DD\&quot;.\n&quot;,
      names(argnames)[2]
    ))</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>1077–1083</td><td><pre><code class="language-r">    stop(
      sprintf(
        &quot;Unable to parse %s layer time from: %s.\n&quot;,
        source,
        bad_layers
      )
    )</code></pre></td><td><pre><code class="language-r">    stop(
      sprintf(
        &quot;Unable to parse %s layer time from: %s.\n&quot;,
        source,
        bad_layers
      )
    )</code></pre></td><td>Keep existing — Retain this existing domain/invariant failure and its surrounding branch. A generic assertion would discard context or alter accepted types, coercion, failure routing, or the helper return value. No additional input constraint is justified here.</td></tr>
</tbody></table></div>

# test_R

Keep package-behavior tests calling the package function, not the checkmate function. Preferred production predicates preserve the existing stop messages, so existing error regexes stay unchanged. Do not weaken them to unqualified expect_error(). Live-test gates, credential skips, mocks and synthetic network failures remain as written.

## Optional test-helper substitution

<div class="table-scroll"><table class="validation-table">
<thead><tr><th>File path</th><th>Start–end lines</th><th>Current Code</th><th>Suggested checkmate / retained code</th><th>Decision and compatibility reason</th></tr></thead><tbody>
<tr><td><code>tests/testthat/helper-skips.R</code></td><td>32–32</td><td><pre><code class="language-r">  stopifnot(length(var) == 1L, is.character(var))</code></pre></td><td><pre><code class="language-r">  checkmate::assertCharacter(var, len = 1L,
    any.missing = TRUE, typed.missing = TRUE)</code></pre></td><td>Optional — diagnostic change — Keep NA_character_ acceptance at this guard. Sys.getenv and testthat::skip stay afterward, and invisible(TRUE) stays at the end. Assertion wording changes; this is optional, not necessary for live-test gating.</td></tr>
</tbody></table></div>

## Output expectations with a concrete benefit

These are optional combinations of existing requirements, not new validation. They preserve intended pass/fail rules for ordinary outputs but not the number/text of reported failures. [Directory checks](https://mllg.github.io/checkmate/reference/checkDirectoryExists.html) and [double checks](https://mllg.github.io/checkmate/reference/checkDouble.html) document the relevant APIs.

<div class="table-scroll"><table class="validation-table">
<thead><tr><th>File path</th><th>Start–end lines</th><th>Current Code</th><th>Suggested checkmate / retained code</th><th>Decision and compatibility reason</th></tr></thead><tbody>
<tr><td><code>tests/testthat/test-cropscape.R</code></td><td>415–417</td><td><pre><code class="language-r">      testthat::expect_type(fractions, &quot;double&quot;)
      testthat::expect_false(anyNA(fractions))
      testthat::expect_true(all(fractions &gt;= 0 &amp; fractions &lt;= 1))</code></pre></td><td><pre><code class="language-r">  checkmate::expect_double(fractions, lower = 0, upper = 1,
    any.missing = FALSE, typed.missing = TRUE)</code></pre></td><td>Optional — test diagnostics change — Combines double type, no missing values and closed [0,1] bounds for ordinary numeric outputs. Keep sum tolerance at line 418 and prior column-count expectation. Expectation count/failure formatting changes; retain native assertions if exact reporter behavior is required.</td></tr>
<tr><td><code>tests/testthat/test-download.R</code></td><td>681–682</td><td><pre><code class="language-r">    testthat::expect_true(is.character(result))
    testthat::expect_true(nchar(result) &gt; 0)</code></pre></td><td><pre><code class="language-r">    checkmate::expect_string(result, min.chars = 1L)</code></pre></td><td>Optional — test diagnostics change — Preserves the intended scalar nonempty hash result; do not impose a fixed MD5 length. Diagnostic count changes; verify attributed/dimensioned outputs before adopting.</td></tr>
<tr><td><code>tests/testthat/test-download.R</code></td><td>733–735</td><td><pre><code class="language-r">    testthat::expect_type(result, &quot;character&quot;)
    testthat::expect_true(nchar(result) &gt; 0)
    testthat::expect_true(grepl(&quot;^[a-f0-9]+$&quot;, result)) # MD5 hash format</code></pre></td><td><pre><code class="language-r">    checkmate::expect_string(result, min.chars = 1L,
      pattern = &quot;^[a-f0-9]+$&quot;)</code></pre></td><td>Optional — test diagnostics change — Preserve the existing lowercase hexadecimal requirement without adding a 32-character length constraint. This is an output expectation, not a production assert.</td></tr>
<tr><td><code>tests/testthat/test-download.R</code></td><td>2676–2679</td><td><pre><code class="language-r">  testthat::expect_true(is.character(tiles))
  testthat::expect_true(length(tiles) &gt; 0)
  testthat::expect_true(all(grepl(&quot;^h[0-9]{2}v[0-9]{2}$&quot;, tiles)))
  testthat::expect_true(length(unique(tiles)) &gt;= 10)</code></pre></td><td><pre><code class="language-r">  checkmate::expect_character(tiles, min.len = 1L,
    any.missing = FALSE, typed.missing = TRUE,
    pattern = &quot;^h[0-9]{2}v[0-9]{2}$&quot;)
  testthat::expect_true(length(unique(tiles)) &gt;= 10)</code></pre></td><td>Optional — test diagnostics change — Preserve nonempty character output, anchored format and at least ten distinct tiles. Duplicates remain permitted; unique=TRUE would change the test. Keep the distinct-count expectation literally unchanged.</td></tr>
<tr><td><code>tests/testthat/test-aqs.R</code></td><td>98–99</td><td><pre><code class="language-r">    testthat::expect_true(dir.exists(&quot;zip_files&quot;))
    testthat::expect_true(dir.exists(&quot;data_files&quot;))</code></pre></td><td><pre><code class="language-r">    checkmate::expect_directory_exists(&quot;zip_files&quot;)
    checkmate::expect_directory_exists(&quot;data_files&quot;)</code></pre></td><td>Optional — test diagnostics change — Literal scalar paths avoid adding vector-length constraints. Preserve directory existence only; do not impose read/write permissions. Failure diagnostics change. See checkmate directory documentation.</td></tr>
</tbody></table></div>

## Test changes not recommended under this scope

<div class="table-scroll"><table class="validation-table">
<thead><tr><th>File path</th><th>Start–end lines</th><th>Current Code</th><th>Suggested checkmate / retained code</th><th>Decision and compatibility reason</th></tr></thead><tbody>
<tr><td><code>tests/testthat/helper-download-tests.R</code></td><td>7–11</td><td><pre><code class="language-r">  testthat::expect_true(dir.exists(directory_to_save))
  testthat::expect_true(file.exists(commands_path))
  if (!is.null(url_status)) {
    testthat::expect_true(all(url_status))
  }</code></pre></td><td><pre><code class="language-r">  testthat::expect_true(dir.exists(directory_to_save))
  testthat::expect_true(file.exists(commands_path))
  if (!is.null(url_status)) {
    testthat::expect_true(all(url_status))
  }</code></pre></td><td>Keep existing — Keep shared helper scalar directory/file existence and all(url_status) behavior. Directory validators can accept multiple paths; file validators can differ on directories. Do not substitute an assertion that aborts tests.</td></tr>
<tr><td><code>tests/testthat/test-ecoregion.R</code></td><td>592–600</td><td><pre><code class="language-r">  testthat::expect_true(
    all(vapply(ecor_frac[, frc_cols, drop = FALSE], is.numeric, logical(1)))
  )
  testthat::expect_true(
    all(as.matrix(ecor_frac[, frc_cols, drop = FALSE]) &gt;= 0, na.rm = TRUE)
  )
  testthat::expect_true(
    all(as.matrix(ecor_frac[, frc_cols, drop = FALSE]) &lt;= 1, na.rm = TRUE)
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_true(
    all(vapply(ecor_frac[, frc_cols, drop = FALSE], is.numeric, logical(1)))
  )
  testthat::expect_true(
    all(as.matrix(ecor_frac[, frc_cols, drop = FALSE]) &gt;= 0, na.rm = TRUE)
  )
  testthat::expect_true(
    all(as.matrix(ecor_frac[, frc_cols, drop = FALSE]) &lt;= 1, na.rm = TRUE)
  )</code></pre></td><td>Keep existing — Keep column type and matrix bounds checks together. A new per-column helper changes failure aggregation and shape/drop handling; no behavior-neutral simplification is established.</td></tr>
<tr><td><code>tests/testthat/test-hms.R</code></td><td>687–695</td><td><pre><code class="language-r">  testthat::expect_true(
    all(vapply(hms_frac[, smoke_cols], is.numeric, logical(1)))
  )
  testthat::expect_true(
    all(as.matrix(hms_frac[, smoke_cols]) &gt;= 0, na.rm = TRUE)
  )
  testthat::expect_true(
    all(as.matrix(hms_frac[, smoke_cols]) &lt;= 1, na.rm = TRUE)
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_true(
    all(vapply(hms_frac[, smoke_cols], is.numeric, logical(1)))
  )
  testthat::expect_true(
    all(as.matrix(hms_frac[, smoke_cols]) &gt;= 0, na.rm = TRUE)
  )
  testthat::expect_true(
    all(as.matrix(hms_frac[, smoke_cols]) &lt;= 1, na.rm = TRUE)
  )</code></pre></td><td>Keep existing — Keep na.rm=TRUE and original column-selection/drop semantics. Do not silently reject environmental missing values or replace output expectations with production assertions.</td></tr>
</tbody></table></div>

## Existing expectations directly or indirectly affected

All rows retain current expectations for the preferred message-preserving proposals. This includes jurisdiction tests in test-coverage-followup.R, EDGAR numeric(1) diagnostics, include_file tests, TRI validation and shared calculation helpers. An assertion-only alternative would need a separate diagnostic decision; it is not silently substituted here.

<div class="table-scroll"><table class="validation-table">
<thead><tr><th>File path</th><th>Start–end lines</th><th>Current Code</th><th>Suggested checkmate / retained code</th><th>Decision and compatibility reason</th></tr></thead><tbody>
<tr><td><code>tests/testthat/test-calc.R</code></td><td>73–75</td><td><pre><code class="language-r">  testthat::expect_no_error(
    tri_r &lt;- process_tri(path = path_tri, year = 2018)
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_no_error(
    tri_r &lt;- process_tri(path = path_tri, year = 2018)
  )</code></pre></td><td>Indirect regression coverage — no edit expected — process_tri uses valid default selectors and ignore_case. Keep this successful call; a type-check migration should not change downstream calculation inputs.</td></tr>
<tr><td><code>tests/testthat/test-calc.R</code></td><td>195–206</td><td><pre><code class="language-r">    testthat::expect_error(
      calculate_modis(
        from = fire_points,
        locs = locs_sf,
        locs_id = &quot;site_id&quot;,
        radius = 0L,
        geom = &quot;sf&quot;,
        fun_summary = &quot;sum&quot;,
        .by_time = &quot;day&quot;
      ),
      &quot;POSIXt&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      calculate_modis(
        from = fire_points,
        locs = locs_sf,
        locs_id = &quot;site_id&quot;,
        radius = 0L,
        geom = &quot;sf&quot;,
        fun_summary = &quot;sum&quot;,
        .by_time = &quot;day&quot;
      ),
      &quot;POSIXt&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-calc.R</code></td><td>221–224</td><td><pre><code class="language-r">    testthat::expect_error(
      calculate_modis(from = list(), locs = locs_sf),
      &quot;character vector of paths, SpatRaster, or SpatVector&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      calculate_modis(from = list(), locs = locs_sf),
      &quot;character vector of paths, SpatRaster, or SpatVector&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-calc.R</code></td><td>225–232</td><td><pre><code class="language-r">    testthat::expect_error(
      calculate_modis(
        from = fire_points[, c(&quot;time&quot;, &quot;fire_count&quot;)],
        locs = locs_sf,
        fun_summary = &quot;sum&quot;
      ),
      &quot;missing required MCD14ML fields&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      calculate_modis(
        from = fire_points[, c(&quot;time&quot;, &quot;fire_count&quot;)],
        locs = locs_sf,
        fun_summary = &quot;sum&quot;
      ),
      &quot;missing required MCD14ML fields&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-calc.R</code></td><td>233–236</td><td><pre><code class="language-r">    testthat::expect_error(
      calculate_modis(from = fire_points, locs = list(), fun_summary = &quot;sum&quot;),
      &quot;convertible to sf&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      calculate_modis(from = fire_points, locs = list(), fun_summary = &quot;sum&quot;),
      &quot;convertible to sf&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-calc.R</code></td><td>698–701</td><td><pre><code class="language-r">  testthat::expect_error(
    check_fun_temporal(42),
    regexp = &quot;single&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    check_fun_temporal(42),
    regexp = &quot;single&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-calc.R</code></td><td>702–705</td><td><pre><code class="language-r">  testthat::expect_error(
    check_fun_temporal(c(&quot;mean&quot;, &quot;sum&quot;)),
    regexp = &quot;single&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    check_fun_temporal(c(&quot;mean&quot;, &quot;sum&quot;)),
    regexp = &quot;single&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-calc.R</code></td><td>709–712</td><td><pre><code class="language-r">  testthat::expect_error(
    check_fun_temporal(&quot;variance&quot;),
    regexp = &quot;must be one of&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    check_fun_temporal(&quot;variance&quot;),
    regexp = &quot;must be one of&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-calc.R</code></td><td>720–720</td><td><pre><code class="language-r">  testthat::expect_error(check_by_time(&quot;site_id&quot;), regexp = &quot;must be one of&quot;)</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(check_by_time(&quot;site_id&quot;), regexp = &quot;must be one of&quot;)</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-calc.R</code></td><td>721–721</td><td><pre><code class="language-r">  testthat::expect_error(check_by_time(1), regexp = &quot;single character&quot;)</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(check_by_time(1), regexp = &quot;single character&quot;)</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-coverage-followup.R</code></td><td>198–206</td><td><pre><code class="language-r">  testthat::expect_error(
    amadeus::download_tri(
      year = 2020,
      directory_to_save = temp_dir,
      acknowledgement = TRUE,
      jurisdiction = NA_character_
    ),
    &quot;single character value&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    amadeus::download_tri(
      year = 2020,
      directory_to_save = temp_dir,
      acknowledgement = TRUE,
      jurisdiction = NA_character_
    ),
    &quot;single character value&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-coverage-followup.R</code></td><td>207–215</td><td><pre><code class="language-r">  testthat::expect_error(
    amadeus::download_tri(
      year = 2020,
      directory_to_save = temp_dir,
      acknowledgement = TRUE,
      jurisdiction = &quot;   &quot;
    ),
    &quot;must be \&quot;US\&quot;, a two-letter state code, or \&quot;tbl\&quot;&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    amadeus::download_tri(
      year = 2020,
      directory_to_save = temp_dir,
      acknowledgement = TRUE,
      jurisdiction = &quot;   &quot;
    ),
    &quot;must be \&quot;US\&quot;, a two-letter state code, or \&quot;tbl\&quot;&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-coverage-followup.R</code></td><td>329–336</td><td><pre><code class="language-r">  testthat::expect_error(
    amadeus::calculate_edgar(
      from = list(),
      locs = point_locs,
      locs_id = &quot;site_id&quot;
    ),
    &quot;`from` must be a SpatRaster object&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    amadeus::calculate_edgar(
      from = list(),
      locs = point_locs,
      locs_id = &quot;site_id&quot;
    ),
    &quot;`from` must be a SpatRaster object&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-coverage-followup.R</code></td><td>337–345</td><td><pre><code class="language-r">  testthat::expect_error(
    amadeus::calculate_edgar(
      from = prism_rast,
      locs = point_locs,
      locs_id = &quot;site_id&quot;,
      radius = c(0, 1)
    ),
    &quot;`radius` must be numeric\\(1\\)&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    amadeus::calculate_edgar(
      from = prism_rast,
      locs = point_locs,
      locs_id = &quot;site_id&quot;,
      radius = c(0, 1)
    ),
    &quot;`radius` must be numeric\\(1\\)&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-coverage-followup.R</code></td><td>406–416</td><td><pre><code class="language-r">  testthat::expect_error(
    amadeus::calculate_modis(
      from = fire_points,
      from_secondary = modis_raster,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0L,
      fun_summary = &quot;sum&quot;
    ),
    &quot;from_secondary is only supported for character or SpatRaster inputs&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    amadeus::calculate_modis(
      from = fire_points,
      from_secondary = modis_raster,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0L,
      fun_summary = &quot;sum&quot;
    ),
    &quot;from_secondary is only supported for character or SpatRaster inputs&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-cropscape.R</code></td><td>193–193</td><td><pre><code class="language-r">  testthat::expect_error(process_cropscape(path = 0, year = &quot;MILLENNIUM&quot;))</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(process_cropscape(path = 0, year = &quot;MILLENNIUM&quot;))</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-cropscape.R</code></td><td>194–196</td><td><pre><code class="language-r">  testthat::expect_error(
    process_cropscape(path = &quot;/home/some/path&quot;, year = &quot;MILLENNIUM&quot;)
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_cropscape(path = &quot;/home/some/path&quot;, year = &quot;MILLENNIUM&quot;)
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-ecoregion.R</code></td><td>279–286</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = &quot;site_id&quot;,
      colnames = &quot;unsupported&quot;
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = &quot;site_id&quot;,
      colnames = &quot;unsupported&quot;
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-ecoregion.R</code></td><td>288–295</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = &quot;site_id&quot;,
      geom = TRUE
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = &quot;site_id&quot;,
      geom = TRUE
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-ecoregion.R</code></td><td>296–303</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = &quot;site_id&quot;,
      frac = NA
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = &quot;site_id&quot;,
      frac = NA
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-ecoregion.R</code></td><td>304–311</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = &quot;site_id&quot;,
      drop = NA
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = &quot;site_id&quot;,
      drop = NA
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-ecoregion.R</code></td><td>389–395</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = &quot;not a spatial object&quot;,
      locs_id = &quot;site_id&quot;
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = &quot;not a spatial object&quot;,
      locs_id = &quot;site_id&quot;
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-ecoregion.R</code></td><td>408–414</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = &quot;nonexistent_id&quot;
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_faux,
      locs_id = &quot;nonexistent_id&quot;
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-ecoregion.R</code></td><td>995–1002</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_vect,
      locs_id = &quot;site_id&quot;
    ),
    &quot;Required ecoregion field missing&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_ecoregion(
      from = erras,
      locs = site_vect,
      locs_id = &quot;site_id&quot;
    ),
    &quot;Required ecoregion field missing&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-edgar.R</code></td><td>967–976</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_edgar(
      from = from,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0,
      .by = &quot;day&quot;
    ),
    regexp = &quot;no longer supported&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_edgar(
      from = from,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0,
      .by = &quot;day&quot;
    ),
    regexp = &quot;no longer supported&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-edgar.R</code></td><td>1005–1014</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_edgar(
      from = from_multi,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0,
      .by_time = &quot;day&quot;
    ),
    regexp = &quot;single covariate column&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_edgar(
      from = from_multi,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0,
      .by_time = &quot;day&quot;
    ),
    regexp = &quot;single covariate column&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-edgar.R</code></td><td>1021–1030</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_edgar(
      from = from,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0,
      .by_time = &quot;day&quot;
    ),
    regexp = &quot;Could not derive EDGAR time&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_edgar(
      from = from,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0,
      .by_time = &quot;day&quot;
    ),
    regexp = &quot;Could not derive EDGAR time&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-gridmet.R</code></td><td>523–526</td><td><pre><code class="language-r">    testthat::expect_error(
      process_parse_ncdf_day_codes(&quot;precipitation_amount_day=bad&quot;),
      regexp = &quot;Unable to parse gridmet layer time.*day=bad&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      process_parse_ncdf_day_codes(&quot;precipitation_amount_day=bad&quot;),
      regexp = &quot;Unable to parse gridmet layer time.*day=bad&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-groads.R</code></td><td>144–147</td><td><pre><code class="language-r">  testthat::expect_error(
    process_groads(path = 1L),
    regexp = &quot;must be a single file path to a \\.shp or \\.gdb roads file&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_groads(path = 1L),
    regexp = &quot;must be a single file path to a \\.shp or \\.gdb roads file&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-groads.R</code></td><td>148–151</td><td><pre><code class="language-r">  testthat::expect_error(
    process_groads(path = &quot;does/not/exist.shp&quot;),
    regexp = &quot;`path` does not exist&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_groads(path = &quot;does/not/exist.shp&quot;),
    regexp = &quot;`path` does not exist&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-groads.R</code></td><td>155–158</td><td><pre><code class="language-r">    testthat::expect_error(
      process_groads(path = bad_path),
      regexp = &quot;must point to a \\.shp or \\.gdb file&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      process_groads(path = bad_path),
      regexp = &quot;must point to a \\.shp or \\.gdb file&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-groads.R</code></td><td>160–163</td><td><pre><code class="language-r">  testthat::expect_error(
    process_groads(path = NA_character_),
    regexp = &quot;must be a single file path to a \\.shp or \\.gdb roads file&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_groads(path = NA_character_),
    regexp = &quot;must be a single file path to a \\.shp or \\.gdb roads file&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-groads.R</code></td><td>205–212</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_groads(
      from = groads,
      locs = ncp,
      locs_id = &quot;site_id&quot;,
      radius = 0
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_groads(
      from = groads,
      locs = ncp,
      locs_id = &quot;site_id&quot;,
      radius = 0
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-groads.R</code></td><td>296–304</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_groads(
      from = groads,
      locs = ncp,
      locs_id = &quot;site_id&quot;,
      radius = 5000,
      geom = TRUE
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_groads(
      from = groads,
      locs = ncp,
      locs_id = &quot;site_id&quot;,
      radius = 5000,
      geom = TRUE
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-groads.R</code></td><td>305–313</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_groads(
      from = groads,
      locs = ncp,
      locs_id = &quot;site_id&quot;,
      radius = 5000,
      drop = NA
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_groads(
      from = groads,
      locs = ncp,
      locs_id = &quot;site_id&quot;,
      radius = 5000,
      drop = NA
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-hms.R</code></td><td>495–503</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_hms(
      from = hms,
      locs = ncp,
      locs_id = &quot;site_id&quot;,
      radius = 0,
      geom = TRUE
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_hms(
      from = hms,
      locs = ncp,
      locs_id = &quot;site_id&quot;,
      radius = 0,
      geom = TRUE
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-hms.R</code></td><td>504–512</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_hms(
      from = hms,
      locs = ncp,
      locs_id = &quot;site_id&quot;,
      radius = 0,
      frac = NA
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_hms(
      from = hms,
      locs = ncp,
      locs_id = &quot;site_id&quot;,
      radius = 0,
      frac = NA
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-improve.R</code></td><td>505–506</td><td><pre><code class="language-r">    testthat::expect_error(amadeus::calculate_improve(from, locs, radius = -1),
                           &quot;radius&quot;)</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(amadeus::calculate_improve(from, locs, radius = -1),
                           &quot;radius&quot;)</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-improve.R</code></td><td>507–508</td><td><pre><code class="language-r">    testthat::expect_error(amadeus::calculate_improve(from, locs, geom = NA),
                           &quot;geom&quot;)</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(amadeus::calculate_improve(from, locs, geom = NA),
                           &quot;geom&quot;)</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-improve.R</code></td><td>509–511</td><td><pre><code class="language-r">    testthat::expect_error(
      amadeus::calculate_improve(from, locs, locs_id = &quot;x&quot;), &quot;identifier&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      amadeus::calculate_improve(from, locs, locs_id = &quot;x&quot;), &quot;identifier&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-improve.R</code></td><td>513–513</td><td><pre><code class="language-r">    testthat::expect_error(amadeus::calculate_improve(from, locs), &quot;unique&quot;)</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(amadeus::calculate_improve(from, locs), &quot;unique&quot;)</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-improve.R</code></td><td>517–517</td><td><pre><code class="language-r">    testthat::expect_error(amadeus::calculate_improve(bad, locs), &quot;FactValue&quot;)</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(amadeus::calculate_improve(bad, locs), &quot;FactValue&quot;)</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-improve.R</code></td><td>520–520</td><td><pre><code class="language-r">    testthat::expect_error(amadeus::calculate_improve(bad, locs), &quot;Longitude&quot;)</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(amadeus::calculate_improve(bad, locs), &quot;Longitude&quot;)</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-improve.R</code></td><td>523–523</td><td><pre><code class="language-r">    testthat::expect_error(amadeus::calculate_improve(bad, locs), &quot;FactDate&quot;)</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(amadeus::calculate_improve(bad, locs), &quot;FactDate&quot;)</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-improve.R</code></td><td>526–526</td><td><pre><code class="language-r">    testthat::expect_error(amadeus::calculate_improve(bad, locs), &quot;Units&quot;)</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(amadeus::calculate_improve(bad, locs), &quot;Units&quot;)</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-improve.R</code></td><td>527–529</td><td><pre><code class="language-r">    testthat::expect_error(
      amadeus::calculate_improve(from, locs, .by = &quot;month&quot;), &quot;by&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      amadeus::calculate_improve(from, locs, .by = &quot;month&quot;), &quot;by&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>67–70</td><td><pre><code class="language-r">      testthat::expect_error(
        get_geos_info(path = &quot;.&quot;),
        regexp = &quot;No GEOS-CF \\.nc4 files&quot;
      )</code></pre></td><td><pre><code class="language-r">      testthat::expect_error(
        get_geos_info(path = &quot;.&quot;),
        regexp = &quot;No GEOS-CF \\.nc4 files&quot;
      )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>71–74</td><td><pre><code class="language-r">      testthat::expect_error(
        get_merra2_info(path = &quot;.&quot;),
        regexp = &quot;No MERRA2 netCDF files&quot;
      )</code></pre></td><td><pre><code class="language-r">      testthat::expect_error(
        get_merra2_info(path = &quot;.&quot;),
        regexp = &quot;No MERRA2 netCDF files&quot;
      )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>75–78</td><td><pre><code class="language-r">      testthat::expect_error(
        get_modis_info(path = &quot;.&quot;),
        regexp = &quot;No MODIS HDF/H5 files&quot;
      )</code></pre></td><td><pre><code class="language-r">      testthat::expect_error(
        get_modis_info(path = &quot;.&quot;),
        regexp = &quot;No MODIS HDF/H5 files&quot;
      )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>90–93</td><td><pre><code class="language-r">    testthat::expect_error(
      get_geos_info(path = geos_path, include_file = &quot;yes&quot;),
      regexp = &quot;single logical value&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      get_geos_info(path = geos_path, include_file = &quot;yes&quot;),
      regexp = &quot;single logical value&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>94–97</td><td><pre><code class="language-r">    testthat::expect_error(
      get_merra2_info(path = merra_path, include_file = NA),
      regexp = &quot;single logical value&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      get_merra2_info(path = merra_path, include_file = NA),
      regexp = &quot;single logical value&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>98–101</td><td><pre><code class="language-r">    testthat::expect_error(
      get_modis_info(path = modis_path, include_file = c(TRUE, FALSE)),
      regexp = &quot;single logical value&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      get_modis_info(path = modis_path, include_file = c(TRUE, FALSE)),
      regexp = &quot;single logical value&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>108–111</td><td><pre><code class="language-r">    testthat::expect_error(
      get_geos_info(path = 1),
      regexp = &quot;non-empty character vector&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      get_geos_info(path = 1),
      regexp = &quot;non-empty character vector&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>112–115</td><td><pre><code class="language-r">    testthat::expect_error(
      get_merra2_info(path = NA_character_),
      regexp = &quot;non-empty character vector&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      get_merra2_info(path = NA_character_),
      regexp = &quot;non-empty character vector&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>116–119</td><td><pre><code class="language-r">    testthat::expect_error(
      get_modis_info(path = list(&quot;.&quot;)),
      regexp = &quot;non-empty character vector&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      get_modis_info(path = list(&quot;.&quot;)),
      regexp = &quot;non-empty character vector&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>128–131</td><td><pre><code class="language-r">      testthat::expect_error(
        suppressWarnings(get_modis_info(path = &quot;.&quot;)),
        regexp = &quot;No MODIS product-subdataset metadata could be derived&quot;
      )</code></pre></td><td><pre><code class="language-r">      testthat::expect_error(
        suppressWarnings(get_modis_info(path = &quot;.&quot;)),
        regexp = &quot;No MODIS product-subdataset metadata could be derived&quot;
      )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>1157–1169</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = c(
        &quot;MOD09GA.A2021001.h10v05.061.2021001000000.hdf&quot;,
        &quot;MOD14CM1.200101.005.01.hdf&quot;
      ),
      locs = locs,
      locs_id = &quot;site_id&quot;,
      name_covariates = &quot;mock_cov_&quot;,
      preprocess = function(...) terra::rast()
    ),
    &quot;mixed or unsupported temporal patterns&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = c(
        &quot;MOD09GA.A2021001.h10v05.061.2021001000000.hdf&quot;,
        &quot;MOD14CM1.200101.005.01.hdf&quot;
      ),
      locs = locs,
      locs_id = &quot;site_id&quot;,
      name_covariates = &quot;mock_cov_&quot;,
      preprocess = function(...) terra::rast()
    ),
    &quot;mixed or unsupported temporal patterns&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>1274–1288</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = from_primary,
      from_secondary = from_secondary,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0L,
      preprocess = mock_preprocess,
      name_covariates = &quot;cov_&quot;,
      subdataset = &quot;mock&quot;,
      scale = &quot;* 1&quot;,
      fusion_method = &quot;mean&quot;
    ),
    &quot;incompatible geometry&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = from_primary,
      from_secondary = from_secondary,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0L,
      preprocess = mock_preprocess,
      name_covariates = &quot;cov_&quot;,
      subdataset = &quot;mock&quot;,
      scale = &quot;* 1&quot;,
      fusion_method = &quot;mean&quot;
    ),
    &quot;incompatible geometry&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>1315–1329</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = from_primary,
      from_secondary = from_secondary,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0L,
      preprocess = mock_preprocess,
      name_covariates = &quot;cov_&quot;,
      subdataset = &quot;mock&quot;,
      scale = &quot;* 1&quot;,
      fusion_method = &quot;mean&quot;
    ),
    &quot;different layer counts&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = from_primary,
      from_secondary = from_secondary,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0L,
      preprocess = mock_preprocess,
      name_covariates = &quot;cov_&quot;,
      subdataset = &quot;mock&quot;,
      scale = &quot;* 1&quot;,
      fusion_method = &quot;mean&quot;
    ),
    &quot;different layer counts&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>1339–1350</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = &quot;MOD09GA.A2021001.h10v05.061.2021001000000.hdf&quot;,
      from_secondary = 1L,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      preprocess = function(...) terra::rast(),
      name_covariates = &quot;cov_&quot;,
      scale = &quot;* 1&quot;
    ),
    &quot;from_secondary should be a character vector&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = &quot;MOD09GA.A2021001.h10v05.061.2021001000000.hdf&quot;,
      from_secondary = 1L,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      preprocess = function(...) terra::rast(),
      name_covariates = &quot;cov_&quot;,
      scale = &quot;* 1&quot;
    ),
    &quot;from_secondary should be a character vector&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>1502–1517</td><td><pre><code class="language-r">  testthat::expect_error(
    suppressMessages(
      calculate_modis(
        from = from_files,
        locs = locs,
        locs_id = &quot;site_id&quot;,
        radius = 0L,
        preprocess = mock_preprocess,
        name_covariates = &quot;cov_&quot;,
        subdataset = &quot;mock&quot;,
        scale = &quot;* 1&quot;,
        .by = &quot;day&quot;
      )
    ),
    regexp = &quot;no longer supported&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    suppressMessages(
      calculate_modis(
        from = from_files,
        locs = locs,
        locs_id = &quot;site_id&quot;,
        radius = 0L,
        preprocess = mock_preprocess,
        name_covariates = &quot;cov_&quot;,
        subdataset = &quot;mock&quot;,
        scale = &quot;* 1&quot;,
        .by = &quot;day&quot;
      )
    ),
    regexp = &quot;no longer supported&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>1629–1637</td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(
      path = &quot;primary.hdf&quot;,
      path_secondary = 1L,
      date = &quot;2023-01-01&quot;,
      subdataset = &quot;mock&quot;
    ),
    &quot;path_secondary&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(
      path = &quot;primary.hdf&quot;,
      path_secondary = 1L,
      date = &quot;2023-01-01&quot;,
      subdataset = &quot;mock&quot;
    ),
    &quot;path_secondary&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>1653–1662</td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(
      path = &quot;primary.hdf&quot;,
      path_secondary = &quot;secondary.hdf&quot;,
      date = &quot;2023-01-01&quot;,
      subdataset = &quot;mock&quot;,
      fusion_method = &quot;mean&quot;
    ),
    &quot;incompatible geometry&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(
      path = &quot;primary.hdf&quot;,
      path_secondary = &quot;secondary.hdf&quot;,
      date = &quot;2023-01-01&quot;,
      subdataset = &quot;mock&quot;,
      fusion_method = &quot;mean&quot;
    ),
    &quot;incompatible geometry&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>1683–1692</td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(
      path = &quot;primary.hdf&quot;,
      path_secondary = &quot;secondary.hdf&quot;,
      date = &quot;2023-01-01&quot;,
      subdataset = &quot;mock&quot;,
      fusion_method = &quot;mean&quot;
    ),
    &quot;different layer counts&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(
      path = &quot;primary.hdf&quot;,
      path_secondary = &quot;secondary.hdf&quot;,
      date = &quot;2023-01-01&quot;,
      subdataset = &quot;mock&quot;,
      fusion_method = &quot;mean&quot;
    ),
    &quot;different layer counts&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>1701–1708</td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(
      path = &quot;primary.hdf&quot;,
      date = &quot;2023-01-01&quot;,
      subdataset = &quot;mock&quot;
    ),
    &quot;No MODIS files matched the requested date&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(
      path = &quot;primary.hdf&quot;,
      date = &quot;2023-01-01&quot;,
      subdataset = &quot;mock&quot;
    ),
    &quot;No MODIS files matched the requested date&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>1954–1961</td><td><pre><code class="language-r">    testthat::expect_error(
      process_flatten_sds(
        path = path_mod11,
        subdataset = &quot;not_a_subdataset&quot;,
        fun_agg = &quot;mean&quot;
      ),
      regexp = &quot;No MODIS subdatasets matched&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      process_flatten_sds(
        path = path_mod11,
        subdataset = &quot;not_a_subdataset&quot;,
        fun_agg = &quot;mean&quot;
      ),
      regexp = &quot;No MODIS subdatasets matched&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>2045–2052</td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(
      path = paths_mod13,
      date = &quot;2021-08-13&quot;,
      subdataset = &quot;(NDVI)&quot;,
      fun_agg = 3L
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(
      path = paths_mod13,
      date = &quot;2021-08-13&quot;,
      subdataset = &quot;(NDVI)&quot;,
      fun_agg = 3L
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>2101–2101</td><td><pre><code class="language-r">  testthat::expect_error(amadeus:::process_mcd14ml(), &quot;path is required&quot;)</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(amadeus:::process_mcd14ml(), &quot;path is required&quot;)</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>2141–2144</td><td><pre><code class="language-r">    testthat::expect_error(
      amadeus:::process_mcd14ml(path = bad_txt),
      &quot;missing one or more required columns&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      amadeus:::process_mcd14ml(path = bad_txt),
      &quot;missing one or more required columns&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>2154–2157</td><td><pre><code class="language-r">    testthat::expect_error(
      amadeus:::process_mcd14ml(path = &quot;.&quot;),
      &quot;No MCD14ML text files were found&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      amadeus:::process_mcd14ml(path = &quot;.&quot;),
      &quot;No MCD14ML text files were found&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>2441–2453</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = path_mod11,
      locs = sf::st_as_sf(site_faux),
      preprocess = process_modis_merge,
      package_list_add = c(&quot;MASS&quot;),
      export_list_add = c(&quot;aux&quot;),
      name_covariates = c(&quot;MOD_LSTNT_0_&quot;, &quot;MOD_LSTDY_0_&quot;),
      subdataset = &quot;(LST_)&quot;,
      geom = TRUE,
      scale = &quot;* 1&quot;
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = path_mod11,
      locs = sf::st_as_sf(site_faux),
      preprocess = process_modis_merge,
      package_list_add = c(&quot;MASS&quot;),
      export_list_add = c(&quot;aux&quot;),
      name_covariates = c(&quot;MOD_LSTNT_0_&quot;, &quot;MOD_LSTDY_0_&quot;),
      subdataset = &quot;(LST_)&quot;,
      geom = TRUE,
      scale = &quot;* 1&quot;
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>2557–2566</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = path_mod06,
      locs = site_faux,
      subdataset = c(&quot;Cloud_Fraction_Day&quot;, &quot;Cloud_Fraction_Night&quot;),
      preprocess = process_modis_swath,
      name_covariates = c(&quot;MOD_CLFRN_0_&quot;, &quot;MOD_CLFRD_0_&quot;),
      geom = TRUE
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = path_mod06,
      locs = site_faux,
      subdataset = c(&quot;Cloud_Fraction_Day&quot;, &quot;Cloud_Fraction_Night&quot;),
      preprocess = process_modis_swath,
      name_covariates = c(&quot;MOD_CLFRN_0_&quot;, &quot;MOD_CLFRD_0_&quot;),
      geom = TRUE
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>2631–2642</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = path_vnp46,
      locs = sf::st_as_sf(site_faux),
      preprocess = process_blackmarble,
      name_covariates = c(&quot;MOD_NITLT_0_&quot;),
      subdataset = 3L,
      tile_df = process_blackmarble_corners(c(9, 10), c(5, 5)),
      geom = TRUE,
      scale = &quot;* 1&quot;
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = path_vnp46,
      locs = sf::st_as_sf(site_faux),
      preprocess = process_blackmarble,
      name_covariates = c(&quot;MOD_NITLT_0_&quot;),
      subdataset = 3L,
      tile_df = process_blackmarble_corners(c(9, 10), c(5, 5)),
      geom = TRUE,
      scale = &quot;* 1&quot;
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>2645–2647</td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(path = site_faux)
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(path = site_faux)
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>2648–2654</td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(
      path = path_mod11,
      date = &quot;2021-08-15&quot;,
      fun_agg = 3L
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(
      path = path_mod11,
      date = &quot;2021-08-15&quot;,
      fun_agg = 3L
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>2655–2661</td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(
      path = path_mod11,
      date = &quot;2021~08~15&quot;,
      fun_agg = &quot;mean&quot;
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_modis_merge(
      path = path_mod11,
      date = &quot;2021~08~15&quot;,
      fun_agg = &quot;mean&quot;
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>2817–2819</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(from = site_faux, scale = &quot;* 1&quot;)
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(from = site_faux, scale = &quot;* 1&quot;)
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>2820–2827</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = path_mod11,
      product = &quot;MOD11A1&quot;,
      locs = list(1, 2, 3),
      scale = &quot;* 1&quot;
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = path_mod11,
      product = &quot;MOD11A1&quot;,
      locs = list(1, 2, 3),
      scale = &quot;* 1&quot;
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>2828–2836</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = path_vnp46,
      locs = site_faux,
      preprocess = &quot;fountain&quot;,
      name_covariates = c(&quot;MOD_NITLT_0_&quot;, &quot;MOD_K1_&quot;),
      subdataset = 3L
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = path_vnp46,
      locs = site_faux,
      preprocess = &quot;fountain&quot;,
      name_covariates = c(&quot;MOD_NITLT_0_&quot;, &quot;MOD_K1_&quot;),
      subdataset = 3L
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>2866–2876</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = path_vnp46,
      locs = site_faux,
      name_covariates = c(&quot;MOD_NITLT_0_&quot;),
      preprocess = process_blackmarble,
      subdataset = 3L,
      radius = c(-1000, 0L),
      scale = 0.01
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = path_vnp46,
      locs = site_faux,
      name_covariates = c(&quot;MOD_NITLT_0_&quot;),
      preprocess = process_blackmarble,
      subdataset = 3L,
      radius = c(-1000, 0L),
      scale = 0.01
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>3000–3007</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = r_primary,
      from_secondary = &quot;MOD11A1.A2021001.h11v05.061.1234567890123.hdf&quot;,
      scale = &quot;* 1&quot;
    ),
    &quot;from_secondary should be SpatRaster&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = r_primary,
      from_secondary = &quot;MOD11A1.A2021001.h11v05.061.1234567890123.hdf&quot;,
      scale = &quot;* 1&quot;
    ),
    &quot;from_secondary should be SpatRaster&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>3008–3015</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = &quot;MOD11A1.A2021001.h11v05.061.1234567890123.hdf&quot;,
      from_secondary = r_secondary,
      scale = &quot;* 1&quot;
    ),
    &quot;from_secondary should be a character vector&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = &quot;MOD11A1.A2021001.h11v05.061.1234567890123.hdf&quot;,
      from_secondary = r_secondary,
      scale = &quot;* 1&quot;
    ),
    &quot;from_secondary should be a character vector&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>3023–3034</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = r_primary,
      from_secondary = r_mismatch_geom,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0L,
      name_covariates = &quot;mock_&quot;,
      scale = &quot;* 1&quot;
    ),
    &quot;incompatible geometry&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = r_primary,
      from_secondary = r_mismatch_geom,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0L,
      name_covariates = &quot;mock_&quot;,
      scale = &quot;* 1&quot;
    ),
    &quot;incompatible geometry&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>3038–3049</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = r_primary,
      from_secondary = r_two_layers,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0L,
      name_covariates = &quot;mock_&quot;,
      scale = &quot;* 1&quot;
    ),
    &quot;different layer counts&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = r_primary,
      from_secondary = r_two_layers,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0L,
      name_covariates = &quot;mock_&quot;,
      scale = &quot;* 1&quot;
    ),
    &quot;different layer counts&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>3675–3689</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = from_primary,
      from_secondary = from_secondary,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0L,
      preprocess = function(...) terra::rast(),
      name_covariates = &quot;cov_&quot;,
      subdataset = &quot;mock&quot;,
      scale = &quot;* 1&quot;,
      fusion_method = &quot;mean&quot;
    ),
    &quot;No MODIS files found&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_modis(
      from = from_primary,
      from_secondary = from_secondary,
      locs = locs,
      locs_id = &quot;site_id&quot;,
      radius = 0L,
      preprocess = function(...) terra::rast(),
      name_covariates = &quot;cov_&quot;,
      subdataset = &quot;mock&quot;,
      scale = &quot;* 1&quot;,
      fusion_method = &quot;mean&quot;
    ),
    &quot;No MODIS files found&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>202–205</td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = 1L),
    &quot;path is not a character&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = 1L),
    &quot;path is not a character&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>206–209</td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = &quot;/universe/galaxy/solarsys/earth/usa.nc&quot;),
    &quot;path does not exist&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = &quot;/universe/galaxy/solarsys/earth/usa.nc&quot;),
    &quot;path does not exist&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>210–213</td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path_nlcd21, &quot;nineteen eighty-four&quot;),
    &quot;year is not a numeric&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path_nlcd21, &quot;nineteen eighty-four&quot;),
    &quot;year is not a numeric&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>214–217</td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path_nlcd21, year = 2020),
    &quot;NLCD data not available for this year&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path_nlcd21, year = 2020),
    &quot;NLCD data not available for this year&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>230–233</td><td><pre><code class="language-r">    testthat::expect_error(
      process_nlcd(path = test_dup_dir, year = 2021),
      regexp = &quot;Duplicated NLCD files are detected&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      process_nlcd(path = test_dup_dir, year = 2021),
      regexp = &quot;Duplicated NLCD files are detected&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>281–288</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      radius = &quot;1000&quot;
    ),
    &quot;radius is not a numeric.&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      radius = &quot;1000&quot;
    ),
    &quot;radius is not a numeric.&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>289–296</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      mode = &quot;whatnot&quot;,
      radius = 1000
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      mode = &quot;whatnot&quot;,
      radius = 1000
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>298–305</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      radius = -3
    ),
    &quot;radius has not a likely value.&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      radius = -3
    ),
    &quot;radius has not a likely value.&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>306–312</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      drop = NA
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = nlcdras,
      drop = NA
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>333–336</td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = path_testdata, year = &quot;2021&quot;),
    &quot;year is not a numeric.&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = path_testdata, year = &quot;2021&quot;),
    &quot;year is not a numeric.&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>338–341</td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = path_testdata, year = 2032),
    &quot;NLCD data not available for this year.&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = path_testdata, year = 2032),
    &quot;NLCD data not available for this year.&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>342–345</td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = path_testdata, year = 1789),
    &quot;NLCD data not available for this year.&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = path_testdata, year = 1789),
    &quot;NLCD data not available for this year.&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>346–352</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = 12,
      locs_id = &quot;site_id&quot;,
      from = nlcdras
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = 12,
      locs_id = &quot;site_id&quot;,
      from = nlcdras
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>353–358</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = 12
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      from = 12
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>360–363</td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = 3, year = 2),
    &quot;path is not a character.&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(path = 3, year = 2),
    &quot;path is not a character.&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>366–371</td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(
      path = nice_sentence
    ),
    &quot;path does not exist.&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_nlcd(
      path = nice_sentence
    ),
    &quot;path does not exist.&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>454–462</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      locs_id = &quot;site_id&quot;,
      radius = buf_radius,
      from = nlcdras,
      geom = TRUE
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      locs_id = &quot;site_id&quot;,
      radius = buf_radius,
      from = nlcdras,
      geom = TRUE
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>592–601</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      locs_id = &quot;SI&quot;,
      from = terra::rast(c(nlcdras, nlcdras)),
      radius = 0,
      mode = &quot;exact&quot;,
      geom = FALSE
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_nlcd(
      locs = eg_data,
      locs_id = &quot;SI&quot;,
      from = terra::rast(c(nlcdras, nlcdras)),
      radius = 0,
      mode = &quot;exact&quot;,
      geom = FALSE
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-prism.R</code></td><td>256–256</td><td><pre><code class="language-r">  testthat::expect_error(process_prism(NULL, element, time))</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(process_prism(NULL, element, time))</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-prism.R</code></td><td>257–261</td><td><pre><code class="language-r">  testthat::expect_error(
    testthat::expect_warning(
      process_prism(path_bad, element, time)
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    testthat::expect_warning(
      process_prism(path_bad, element, time)
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-prism.R</code></td><td>262–262</td><td><pre><code class="language-r">  testthat::expect_error(process_prism(path_dir, element_bad, time))</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(process_prism(path_dir, element_bad, time))</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-prism.R</code></td><td>263–263</td><td><pre><code class="language-r">  testthat::expect_error(process_prism(path_dir, element, time_bad))</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(process_prism(path_dir, element, time_bad))</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-process.R</code></td><td>224–226</td><td><pre><code class="language-r">  testthat::expect_error(
    process_conformity(locs = dfe, check_time = TRUE)
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_conformity(locs = dfe, check_time = TRUE)
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-process.R</code></td><td>276–279</td><td><pre><code class="language-r">  testthat::expect_error(
    process_parse_ncdf_day_codes(&quot;precipitation_amount_day=bad&quot;),
    regexp = &quot;Unable to parse&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_parse_ncdf_day_codes(&quot;precipitation_amount_day=bad&quot;),
    regexp = &quot;Unable to parse&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-sedc.R</code></td><td>21–23</td><td><pre><code class="language-r">  testthat::expect_no_error(
    tri_r &lt;- process_tri(path = path_tri, year = 2018)
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_no_error(
    tri_r &lt;- process_tri(path = path_tri, year = 2018)
  )</code></pre></td><td>Indirect regression coverage — no edit expected — process_tri uses valid default selectors and ignore_case. Keep this successful call; a type-check migration should not change downstream calculation inputs.</td></tr>
<tr><td><code>tests/testthat/test-sedc.R</code></td><td>76–85</td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols,
      geom = TRUE
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols,
      geom = TRUE
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-sedc.R</code></td><td>123–132</td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = &quot;NOT_A_COLUMN&quot;
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = &quot;NOT_A_COLUMN&quot;
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-sedc.R</code></td><td>133–142</td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = &quot;&quot;
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = &quot;&quot;
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-sedc.R</code></td><td>143–152</td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = rep(1, nrow(tri_r) - 1)
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      C0 = rep(1, nrow(tri_r) - 1)
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-sedc.R</code></td><td>153–161</td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = 1
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = 1
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-sedc.R</code></td><td>162–171</td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      use_threshold = NA
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols[1],
      use_threshold = NA
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-sedc.R</code></td><td>172–180</td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = -10,
      target_fields = targcols[1]
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = -10,
      target_fields = targcols[1]
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>87–96</td><td><pre><code class="language-r">    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = &quot;.&quot;,
        acknowledgement = TRUE,
        jurisdiction = &quot;northcarolina&quot;,
        download = FALSE
      ),
      regexp = &quot;jurisdiction&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = &quot;.&quot;,
        acknowledgement = TRUE,
        jurisdiction = &quot;northcarolina&quot;,
        download = FALSE
      ),
      regexp = &quot;jurisdiction&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>102–111</td><td><pre><code class="language-r">    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = &quot;.&quot;,
        acknowledgement = TRUE,
        jurisdiction = &quot;&quot;,
        download = FALSE
      ),
      regexp = &quot;jurisdiction&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = &quot;.&quot;,
        acknowledgement = TRUE,
        jurisdiction = &quot;&quot;,
        download = FALSE
      ),
      regexp = &quot;jurisdiction&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>112–121</td><td><pre><code class="language-r">    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = &quot;.&quot;,
        acknowledgement = TRUE,
        jurisdiction = NA_character_,
        download = FALSE
      ),
      regexp = &quot;jurisdiction&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = &quot;.&quot;,
        acknowledgement = TRUE,
        jurisdiction = NA_character_,
        download = FALSE
      ),
      regexp = &quot;jurisdiction&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>122–131</td><td><pre><code class="language-r">    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = &quot;.&quot;,
        acknowledgement = TRUE,
        jurisdiction = c(&quot;US&quot;, &quot;AZ&quot;),
        download = FALSE
      ),
      regexp = &quot;jurisdiction&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = &quot;.&quot;,
        acknowledgement = TRUE,
        jurisdiction = c(&quot;US&quot;, &quot;AZ&quot;),
        download = FALSE
      ),
      regexp = &quot;jurisdiction&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>132–141</td><td><pre><code class="language-r">    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = &quot;.&quot;,
        acknowledgement = TRUE,
        jurisdiction = 42,
        download = FALSE
      ),
      regexp = &quot;jurisdiction&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = &quot;.&quot;,
        acknowledgement = TRUE,
        jurisdiction = 42,
        download = FALSE
      ),
      regexp = &quot;jurisdiction&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>297–300</td><td><pre><code class="language-r">  testthat::expect_error(
    get_tri_info(path = path_tri, year = c(2018, 2019)),
    regexp = &quot;`year`&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    get_tri_info(path = path_tri, year = c(2018, 2019)),
    regexp = &quot;`year`&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>301–304</td><td><pre><code class="language-r">  testthat::expect_error(
    get_tri_info(path = path_tri, type = &quot;industries&quot;, year = NA_real_),
    regexp = &quot;`year`&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    get_tri_info(path = path_tri, type = &quot;industries&quot;, year = NA_real_),
    regexp = &quot;`year`&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>309–312</td><td><pre><code class="language-r">    testthat::expect_error(
      get_tri_info(path = &quot;.&quot;),
      regexp = &quot;No TRI CSV files&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      get_tri_info(path = &quot;.&quot;),
      regexp = &quot;No TRI CSV files&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>317–320</td><td><pre><code class="language-r">    testthat::expect_error(
      get_tri_info(path = &quot;.&quot;, year = 2018),
      regexp = &quot;missing `YEAR`&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      get_tri_info(path = &quot;.&quot;, year = 2018),
      regexp = &quot;missing `YEAR`&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>325–328</td><td><pre><code class="language-r">    testthat::expect_error(
      get_tri_info(path = &quot;.&quot;),
      regexp = &quot;required chemical lookup columns&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      get_tri_info(path = &quot;.&quot;),
      regexp = &quot;required chemical lookup columns&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>354–357</td><td><pre><code class="language-r">    testthat::expect_error(
      get_tri_info(path = &quot;.&quot;, type = &quot;industries&quot;),
      regexp = &quot;required industry lookup columns&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      get_tri_info(path = &quot;.&quot;, type = &quot;industries&quot;),
      regexp = &quot;required industry lookup columns&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>460–467</td><td><pre><code class="language-r">  testthat::expect_error(
    process_tri(
      path = path_tri,
      year = 2018,
      variables = 49
    ),
    regexp = &quot;`variables`&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_tri(
      path = path_tri,
      year = 2018,
      variables = 49
    ),
    regexp = &quot;`variables`&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review. This already covers a non-character variables value (49); do not report that type coverage as absent.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>468–476</td><td><pre><code class="language-r">  testthat::expect_error(
    process_tri(
      path = path_tri,
      year = 2018,
      variables = &quot;STACK_AIR&quot;,
      chemical = &quot;this-will-not-match&quot;
    ),
    regexp = &quot;`chemical` did not match&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_tri(
      path = path_tri,
      year = 2018,
      variables = &quot;STACK_AIR&quot;,
      chemical = &quot;this-will-not-match&quot;
    ),
    regexp = &quot;`chemical` did not match&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>489–492</td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = c(&quot;STACK_AIR&quot;, &quot; &quot;)),
      regexp = &quot;empty patterns&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = c(&quot;STACK_AIR&quot;, &quot; &quot;)),
      regexp = &quot;empty patterns&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>493–496</td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = &quot;STACK_AIR&quot;, chemical = 1),
      regexp = &quot;`chemical` must be NULL or a non-empty character vector&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = &quot;STACK_AIR&quot;, chemical = 1),
      regexp = &quot;`chemical` must be NULL or a non-empty character vector&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>497–500</td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = &quot;STACK_AIR&quot;, chemical = &quot;&quot;),
      regexp = &quot;`chemical` cannot include empty patterns&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = &quot;STACK_AIR&quot;, chemical = &quot;&quot;),
      regexp = &quot;`chemical` cannot include empty patterns&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>501–504</td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = &quot;STACK_AIR&quot;, ignore_case = NA),
      regexp = &quot;`ignore_case` must be TRUE or FALSE&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = &quot;STACK_AIR&quot;, ignore_case = NA),
      regexp = &quot;`ignore_case` must be TRUE or FALSE&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>505–508</td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = &quot;NOT_A_TRI_VARIABLE&quot;),
      regexp = &quot;`variables` did not match any TRI variable columns&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = &quot;NOT_A_TRI_VARIABLE&quot;),
      regexp = &quot;`variables` did not match any TRI variable columns&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>509–512</td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, year = 2030, variables = &quot;STACK_AIR&quot;),
      regexp = &quot;No TRI rows found for requested `year`&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, year = 2030, variables = &quot;STACK_AIR&quot;),
      regexp = &quot;No TRI rows found for requested `year`&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>523–526</td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = &quot;STACK_AIR&quot;),
      regexp = &quot;missing required columns&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = &quot;STACK_AIR&quot;),
      regexp = &quot;missing required columns&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>538–541</td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = &quot;STACK_AIR&quot;, industry_group = &quot;industry_sector&quot;),
      regexp = &quot;missing industry grouping columns&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = &quot;STACK_AIR&quot;, industry_group = &quot;industry_sector&quot;),
      regexp = &quot;missing industry grouping columns&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>553–556</td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = &quot;STACK_AIR&quot;),
      regexp = &quot;is not numeric&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = &quot;STACK_AIR&quot;),
      regexp = &quot;is not numeric&quot;
    )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>564–572</td><td><pre><code class="language-r">  testthat::expect_error(
    process_tri(
      path = path_tri,
      year = 2018,
      variables = &quot;stack_air&quot;,
      ignore_case = FALSE
    ),
    regexp = &quot;`variables` did not match any TRI variable columns&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_tri(
      path = path_tri,
      year = 2018,
      variables = &quot;stack_air&quot;,
      ignore_case = FALSE
    ),
    regexp = &quot;`variables` did not match any TRI variable columns&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>779–786</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      decay_range = c(1500L, 50000L),
      geom = TRUE
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      decay_range = c(1500L, 50000L),
      geom = TRUE
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>821–827</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tempdir(),
      locs = ncpt,
      decay_range = 50000L
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tempdir(),
      locs = ncpt,
      decay_range = 50000L
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>828–834</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tempdir(),
      locs = ncpt[, 1:2],
      decay_range = 50000L
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tempdir(),
      locs = ncpt[, 1:2],
      decay_range = 50000L
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>835–841</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tempdir(),
      locs = ncpt,
      decay_range = &quot;As far as the Earth&#x27;s radius&quot;
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tempdir(),
      locs = ncpt,
      decay_range = &quot;As far as the Earth&#x27;s radius&quot;
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>842–850</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      decay_range = 50000L,
      use_threshold = NA
    ),
    regexp = &quot;`use_threshold` must be TRUE or FALSE&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      decay_range = 50000L,
      use_threshold = NA
    ),
    regexp = &quot;`use_threshold` must be TRUE or FALSE&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>851–858</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      decay_range = 50000L,
      C0 = &quot;NOT_A_COLUMN&quot;
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      decay_range = 50000L,
      C0 = &quot;NOT_A_COLUMN&quot;
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>859–867</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      decay_range = 50000L,
      C0 = &quot;&quot;
    ),
    regexp = &quot;`C0` must be NULL or a non-empty character vector of column names&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      decay_range = 50000L,
      C0 = &quot;&quot;
    ),
    regexp = &quot;`C0` must be NULL or a non-empty character vector of column names&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>868–875</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      locs_id = &quot;NOT_A_SITE_ID&quot;,
      decay_range = 50000L
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      locs_id = &quot;NOT_A_SITE_ID&quot;,
      decay_range = 50000L
    )
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>876–884</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      decay_range = 50000L,
      C0 = c(&quot;STACK_AIR_100&quot;, &quot;STACK_AIR_200&quot;)
    ),
    regexp = &quot;`C0` must have length 1 or match the number of TRI target fields&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r,
      locs = ncpt,
      decay_range = 50000L,
      C0 = c(&quot;STACK_AIR_100&quot;, &quot;STACK_AIR_200&quot;)
    ),
    regexp = &quot;`C0` must have length 1 or match the number of TRI target fields&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>888–896</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r_bad_c0,
      locs = ncpt,
      decay_range = 50000L,
      C0 = &quot;BAD_C0&quot;
    ),
    regexp = &quot;is not numeric&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(
      from = tri_r_bad_c0,
      locs = ncpt,
      decay_range = 50000L,
      C0 = &quot;BAD_C0&quot;
    ),
    regexp = &quot;is not numeric&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>917–920</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(from = empty_tri, locs = locs, decay_range = 1000L),
    regexp = &quot;No TRI target fields found&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_tri(from = empty_tri, locs = locs, decay_range = 1000L),
    regexp = &quot;No TRI target fields found&quot;
  )</code></pre></td><td>Regression coverage — no edit expected — Keep this package call, invalid input and expected error unchanged: the preferred predicate replacement retains the original stop body. It remains regression coverage for validation order and downstream errors; no test code is implemented by this review.</td></tr>
</tbody></table></div>

## Additional coverage to consider if implementing

This row inserts two separate tests immediately before the existing test-block header; it does not replace the existing test body. Tests preserve current diagnostics. No test files are written.

<div class="table-scroll"><table class="validation-table">
<thead><tr><th>File path</th><th>Start–end lines</th><th>Current Code</th><th>Suggested checkmate / retained code</th><th>Decision and compatibility reason</th></tr></thead><tbody>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>479–479</td><td><pre><code class="language-r">testthat::test_that(&quot;process_tri validates and errors on unsupported input paths&quot;, {</code></pre></td><td><pre><code class="language-r">testthat::test_that(
  &quot;process_tri(variables=character(0)): rejects an empty selector vector&quot;,
  {
    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = character(0)),
      regexp = &quot;`variables` must be a non-empty character vector of regex patterns&quot;
    )
  }
)
testthat::test_that(
  &quot;process_tri(variables=NA_character_): rejects a missing selector&quot;,
  {
    testthat::expect_error(
      process_tri(path = &quot;.&quot;, variables = NA_character_),
      regexp = &quot;`variables` must be a non-empty character vector of regex patterns&quot;
    )
  }
)
testthat::test_that(&quot;process_tri validates and errors on unsupported input paths&quot;, {</code></pre></td><td>Proposed coverage only — Non-character variables is already covered at 460–467. Add empty/missing selectors separately; validation occurs before reading path. Compare old/new checks on NULL, typed empty vectors, every NA storage type, NaN, Inf, matrices, classed scalars and custom methods before adopting any candidate.</td></tr>
</tbody></table></div>

# Complete file coverage and regression inventory

Every file is listed below. The excerpt is an audit anchor, not a claim that it is the only code reviewed. Suggested code is unchanged here; use the specific replacement rows above for candidate ranges. The call links are a conservative static regression aid, not proof of execution coverage. Wrapper string dispatch, passed preprocessing functions and mocks require the full mocked suite, not just literal-call filtering.

<div class="table-scroll"><table class="validation-table">
<thead><tr><th>File path</th><th>Start–end lines</th><th>Current Code</th><th>Suggested checkmate / retained code</th><th>Decision and compatibility reason</th></tr></thead><tbody>
<tr><td><code>R/amadeus-package.R</code></td><td>2–2</td><td><pre><code class="language-r">utils::globalVariables(c(&quot;LATITUDE&quot;, &quot;LONGITUDE&quot;))</code></pre></td><td><pre><code class="language-r">utils::globalVariables(c(&quot;LATITUDE&quot;, &quot;LONGITUDE&quot;))</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 2 lines; 0 expect_error calls; SHA-256 9bec3445735cbc05. Registers NSE global variable names through utils::globalVariables; no input validator to replace. </td></tr>
<tr><td><code>R/calculate_covariates.R</code></td><td>267–269</td><td><pre><code class="language-r">    if (!is.logical(frac) || length(frac) != 1L || is.na(frac)) {
      stop(&quot;`frac` should be a single logical value (TRUE/FALSE).&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!is.logical(frac) || length(frac) != 1L || is.na(frac)) {
      stop(&quot;`frac` should be a single logical value (TRUE/FALSE).&quot;)
    }</code></pre></td><td>Coverage inventory — excerpt retained — Concrete proposals appear above. Full file scanned: 5244 lines; 0 expect_error calls; SHA-256 d99f41ec9faf23b6. Direct/static-transitive regression links: calc_apply_time_summary, calc_summarize_by, calculate_modis_fire_vector, check_by_time, sum_edc. </td></tr>
<tr><td><code>R/calculate_covariates_auxiliary.R</code></td><td>1025–1035</td><td><pre><code class="language-r">  if (
    !is.character(fun_temporal) ||
      length(fun_temporal) != 1L
  ) {
    stop(
      paste0(
        &quot;`fun_temporal` must be NULL or a single &quot;,
        &quot;character string.\n&quot;
      )
    )
  }</code></pre></td><td><pre><code class="language-r">  if (
    !is.character(fun_temporal) ||
      length(fun_temporal) != 1L
  ) {
    stop(
      paste0(
        &quot;`fun_temporal` must be NULL or a single &quot;,
        &quot;character string.\n&quot;
      )
    )
  }</code></pre></td><td>Coverage inventory — excerpt retained — Concrete proposals appear above. Full file scanned: 1814 lines; 0 expect_error calls; SHA-256 b4f8dc211f0e2130. Direct/static-transitive regression links: calc_summarize_by, check_by_time. </td></tr>
<tr><td><code>R/download.R</code></td><td>3125–3135</td><td><pre><code class="language-r">  if (
    !is.character(jurisdiction) ||
      length(jurisdiction) != 1 ||
      is.na(jurisdiction)
  ) {
    stop(
      &quot;`jurisdiction` must be a single character value such as &quot;,
      &quot;\&quot;US\&quot;, \&quot;AZ\&quot;, or \&quot;tbl\&quot;.\n&quot;,
      call. = FALSE
    )
  }</code></pre></td><td><pre><code class="language-r">  if (
    !is.character(jurisdiction) ||
      length(jurisdiction) != 1 ||
      is.na(jurisdiction)
  ) {
    stop(
      &quot;`jurisdiction` must be a single character value such as &quot;,
      &quot;\&quot;US\&quot;, \&quot;AZ\&quot;, or \&quot;tbl\&quot;.\n&quot;,
      call. = FALSE
    )
  }</code></pre></td><td>Coverage inventory — excerpt retained — Concrete proposals appear above. Full file scanned: 5612 lines; 0 expect_error calls; SHA-256 2b863098540046bb. </td></tr>
<tr><td><code>R/download_auxiliary.R</code></td><td>126–126</td><td><pre><code class="language-r">  stopifnot(length(key) == length(scale))</code></pre></td><td><pre><code class="language-r">  stopifnot(length(key) == length(scale))</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 1418 lines; 0 expect_error calls; SHA-256 71f5edb1867e5495. </td></tr>
<tr><td><code>R/ignore.R</code></td><td>276–278</td><td><pre><code class="language-r">    if (!is.character(path) || is.null(path)) {
      stop(&quot;path is not a character.&quot;)
    }</code></pre></td><td><pre><code class="language-r">    if (!is.character(path) || is.null(path)) {
      stop(&quot;path is not a character.&quot;)
    }</code></pre></td><td>Coverage inventory — excerpt retained — Concrete proposals appear above. Full file scanned: 484 lines; 0 expect_error calls; SHA-256 2552785048535ff4. </td></tr>
<tr><td><code>R/manipulate_spacetime_data.R</code></td><td>13–22</td><td><pre><code class="language-r">  stopifnot(
    &quot;x is not a sftime&quot; = class(x)[1] == &quot;sftime&quot;,
    &quot;x is not inherited from a data.table&quot; =
      class(x)[3] == &quot;data.table&quot;,
    &quot;time column should be called time&quot; =
      attributes(x)$time_column == &quot;time&quot;,
    &quot;geometry column should be called geometry&quot; =
      attributes(x)$sf_column == &quot;geometry&quot;,
    &quot;geometry is not a sfc_POINT&quot; = class(x$geometry)[1] == &quot;sfc_POINT&quot;
  )</code></pre></td><td><pre><code class="language-r">  stopifnot(
    &quot;x is not a sftime&quot; = class(x)[1] == &quot;sftime&quot;,
    &quot;x is not inherited from a data.table&quot; =
      class(x)[3] == &quot;data.table&quot;,
    &quot;time column should be called time&quot; =
      attributes(x)$time_column == &quot;time&quot;,
    &quot;geometry column should be called geometry&quot; =
      attributes(x)$sf_column == &quot;geometry&quot;,
    &quot;geometry is not a sfc_POINT&quot; = class(x$geometry)[1] == &quot;sfc_POINT&quot;
  )</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 433 lines; 0 expect_error calls; SHA-256 7099c32980e9bebe. </td></tr>
<tr><td><code>R/process.R</code></td><td>1493–1497</td><td><pre><code class="language-r">  if (
    !is.logical(ignore_case) || length(ignore_case) != 1 || is.na(ignore_case)
  ) {
    stop(&quot;`ignore_case` must be TRUE or FALSE.\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (
    !is.logical(ignore_case) || length(ignore_case) != 1 || is.na(ignore_case)
  ) {
    stop(&quot;`ignore_case` must be TRUE or FALSE.\n&quot;)
  }</code></pre></td><td>Coverage inventory — excerpt retained — Concrete proposals appear above. Full file scanned: 5235 lines; 0 expect_error calls; SHA-256 4f7bf0cd094cf92d. Direct/static-transitive regression links: process_flatten_sds, process_modis_merge, process_parse_ncdf_day_codes. </td></tr>
<tr><td><code>R/process_auxiliary.R</code></td><td>815–821</td><td><pre><code class="language-r">  if (
    !is.logical(include_file) ||
      length(include_file) != 1L ||
      is.na(include_file)
  ) {
    stop(&quot;`include_file` must be a single logical value (TRUE/FALSE).\n&quot;)
  }</code></pre></td><td><pre><code class="language-r">  if (
    !is.logical(include_file) ||
      length(include_file) != 1L ||
      is.na(include_file)
  ) {
    stop(&quot;`include_file` must be a single logical value (TRUE/FALSE).\n&quot;)
  }</code></pre></td><td>Coverage inventory — excerpt retained — Concrete proposals appear above. Full file scanned: 1113 lines; 0 expect_error calls; SHA-256 ca8cdfd168de8bfc. Direct/static-transitive regression links: info_resolve_paths. </td></tr>
<tr><td><code>R/process_improve_sites_builtin.R</code></td><td>1–4</td><td><pre><code class="language-r">process_improve_sites_builtin &lt;- function() {
  # nolint start: line_length_linter.
  txt &lt;- paste(c(
    &quot;SiteID|SiteCode|SiteName|Country|State|County|AQSCode|Latitude|Longitude|Elevation|StartDate|EndDate|DataStartDate|DataEndDate|DemographicCode|LandUseCode|StreetAddress|LocDesc|Sponsor|ProgramKey&quot;,</code></pre></td><td><pre><code class="language-r">process_improve_sites_builtin &lt;- function() {
  # nolint start: line_length_linter.
  txt &lt;- paste(c(
    &quot;SiteID|SiteCode|SiteName|Country|State|County|AQSCode|Latitude|Longitude|Elevation|StartDate|EndDate|DataStartDate|DataEndDate|DemographicCode|LandUseCode|StreetAddress|LocDesc|Sponsor|ProgramKey&quot;,</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 251 lines; 0 expect_error calls; SHA-256 79036d043a4208c3. Built-in site data construction has no input-validation guard to replace. </td></tr>
<tr><td><code>tests/testthat/helper-download-tests.R</code></td><td>7–7</td><td><pre><code class="language-r">  testthat::expect_true(dir.exists(directory_to_save))</code></pre></td><td><pre><code class="language-r">  testthat::expect_true(dir.exists(directory_to_save))</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 12 lines; 0 expect_error calls; SHA-256 b176f7bac32672a8. </td></tr>
<tr><td><code>tests/testthat/helper-fixtures.R</code></td><td>13–16</td><td><pre><code class="language-r">fixture_spatraster &lt;- function(nrow = 10L, ncol = 10L, value = 1) {
  testthat::skip_if_not_installed(&quot;terra&quot;)
  r &lt;- terra::rast(
    nrows = nrow, ncols = ncol,</code></pre></td><td><pre><code class="language-r">fixture_spatraster &lt;- function(nrow = 10L, ncol = 10L, value = 1) {
  testthat::skip_if_not_installed(&quot;terra&quot;)
  r &lt;- terra::rast(
    nrows = nrow, ncols = ncol,</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 51 lines; 0 expect_error calls; SHA-256 c8620535556302b2. </td></tr>
<tr><td><code>tests/testthat/helper-mocks-download.R</code></td><td>33–36</td><td><pre><code class="language-r">mocks_download_stack &lt;- function(success = 1L,
                                 failed = 0L,
                                 skipped = 0L,
                                 hash_value = &quot;hash-ok&quot;,</code></pre></td><td><pre><code class="language-r">mocks_download_stack &lt;- function(success = 1L,
                                 failed = 0L,
                                 skipped = 0L,
                                 hash_value = &quot;hash-ok&quot;,</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 99 lines; 0 expect_error calls; SHA-256 9da98fd82b6ec610. Mocks intentionally bypass I/O; imposing production validators in them would change their purpose. </td></tr>
<tr><td><code>tests/testthat/helper-mocks-process.R</code></td><td>11–14</td><td><pre><code class="language-r">mocks_list_files &lt;- function(files) {
  force(files)
  list(
    list.files = function(...) files</code></pre></td><td><pre><code class="language-r">mocks_list_files &lt;- function(files) {
  force(files)
  list(
    list.files = function(...) files</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 43 lines; 0 expect_error calls; SHA-256 aa277e5889b58934. Mocks intentionally bypass I/O; imposing production validators in them would change their purpose. </td></tr>
<tr><td><code>tests/testthat/helper-skips.R</code></td><td>32–32</td><td><pre><code class="language-r">  stopifnot(length(var) == 1L, is.character(var))</code></pre></td><td><pre><code class="language-r">  stopifnot(length(var) == 1L, is.character(var))</code></pre></td><td>Coverage inventory — excerpt retained — Concrete proposals appear above. Full file scanned: 80 lines; 0 expect_error calls; SHA-256 74f143e0f26276b2. </td></tr>
<tr><td><code>tests/testthat/test-aqs-live.R</code></td><td>17–17</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 209 lines; 0 expect_error calls; SHA-256 2ed9efa7549172d8. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-aqs.R</code></td><td>230–237</td><td><pre><code class="language-r">    testthat::expect_error(
      download_aqs(
        year = c(1900, 1901),
        directory_to_save = &quot;.&quot;,
        acknowledgement = TRUE
      ),
      &quot;year&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      download_aqs(
        year = c(1900, 1901),
        directory_to_save = &quot;.&quot;,
        acknowledgement = TRUE
      ),
      &quot;year&quot;
    )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 883 lines; 9 expect_error calls; SHA-256 b2397a99a1cd78c7. Direct/static-transitive regression links: download_data, process_covariates. </td></tr>
<tr><td><code>tests/testthat/test-by-summarize.R</code></td><td>47–50</td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_covariates(covariate = &quot;goes&quot;, from = from, locs = locs, locs_id = &quot;site_id&quot;, radius = 0, .by = &quot;day&quot;),
    regexp = &quot;no longer supported&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    calculate_covariates(covariate = &quot;goes&quot;, from = from, locs = locs, locs_id = &quot;site_id&quot;, radius = 0, .by = &quot;day&quot;),
    regexp = &quot;no longer supported&quot;
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 68 lines; 2 expect_error calls; SHA-256 e4a72b99b194811f. Direct/static-transitive regression links: calc_summarize_by, calculate_covariates, calculate_geos, calculate_goes, calculate_merra2. </td></tr>
<tr><td><code>tests/testthat/test-calc.R</code></td><td>52–54</td><td><pre><code class="language-r">    testthat::expect_error(
      suppressWarnings(calculate_covariates(covariate = cand))
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      suppressWarnings(calculate_covariates(covariate = cand))
    )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 1160 lines; 43 expect_error calls; SHA-256 72049ce6895482d7. Direct/static-transitive regression links: calc_apply_time_summary, calc_summarize_by, calculate_covariates, calculate_drought, calculate_modis, calculate_modis_fire_vector, calculate_narr, check_by_time, check_fun_temporal, process_mcd14ml, process_tri. </td></tr>
<tr><td><code>tests/testthat/test-coverage-followup.R</code></td><td>45–53</td><td><pre><code class="language-r">  testthat::expect_error(
    amadeus::download_geos(
      collection = &quot;aqc_tavg_1hr_g1440x721_v1&quot;,
      date = &quot;2024-01-01&quot;,
      directory_to_save = temp_dir,
      acknowledgement = TRUE
    ),
    &quot;Invalid date returns HTTP code 404&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    amadeus::download_geos(
      collection = &quot;aqc_tavg_1hr_g1440x721_v1&quot;,
      date = &quot;2024-01-01&quot;,
      directory_to_save = temp_dir,
      acknowledgement = TRUE
    ),
    &quot;Invalid date returns HTTP code 404&quot;
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 931 lines; 22 expect_error calls; SHA-256 b479867d7cd2a05c. Direct/static-transitive regression links: calculate_covariates, calculate_drought, calculate_ecoregion, calculate_edgar, calculate_groads, calculate_koppen_geiger, calculate_modis, calculate_nlcd, calculate_prism, calculate_tri, download_tri. </td></tr>
<tr><td><code>tests/testthat/test-cropscape-live.R</code></td><td>11–11</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 87 lines; 0 expect_error calls; SHA-256 dcbd5ec79fd2b3ee. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-cropscape.R</code></td><td>82–82</td><td><pre><code class="language-r">  testthat::expect_error(download_cropscape(year = 2020, source = &quot;CMU&quot;))</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(download_cropscape(year = 2020, source = &quot;CMU&quot;))</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 421 lines; 6 expect_error calls; SHA-256 4953959833d794cb. Direct/static-transitive regression links: calculate_covariates, download_data, process_covariates, process_cropscape. </td></tr>
<tr><td><code>tests/testthat/test-download-dispatch.R</code></td><td>19–19</td><td><pre><code class="language-r">      testthat::expect_identical(result, source)</code></pre></td><td><pre><code class="language-r">      testthat::expect_identical(result, source)</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 70 lines; 0 expect_error calls; SHA-256 dbbef042040ef7f4. Direct/static-transitive regression links: download_data. </td></tr>
<tr><td><code>tests/testthat/test-download.R</code></td><td>38–43</td><td><pre><code class="language-r">    testthat::expect_error(
      download_data(
        dataset_name = download_datasets[d],
        acknowledgement = FALSE
      )
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      download_data(
        dataset_name = download_datasets[d],
        acknowledgement = FALSE
      )
    )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 2699 lines; 27 expect_error calls; SHA-256 8b2da7c859c50205. Direct/static-transitive regression links: download_data. </td></tr>
<tr><td><code>tests/testthat/test-drought-live.R</code></td><td>43–43</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 114 lines; 0 expect_error calls; SHA-256 a55dbda1761ac074. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-drought.R</code></td><td>150–153</td><td><pre><code class="language-r">  testthat::expect_error(
    process_drought(source = &quot;foo&quot;, path = testdata_spei, date = &quot;2020-01-01&quot;),
    regexp = &quot;arg&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_drought(source = &quot;foo&quot;, path = testdata_spei, date = &quot;2020-01-01&quot;),
    regexp = &quot;arg&quot;
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 2078 lines; 20 expect_error calls; SHA-256 b75c1c2de0aa10c2. Direct/static-transitive regression links: calculate_covariates, calculate_drought, download_data, process_covariates. </td></tr>
<tr><td><code>tests/testthat/test-dummies.R</code></td><td>52–56</td><td><pre><code class="language-r">  testthat::expect_error(
    dum_res &lt;- calculate_temporal_dummies(
      locs = site_faux_err
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    dum_res &lt;- calculate_temporal_dummies(
      locs = site_faux_err
    )
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 86 lines; 5 expect_error calls; SHA-256 3d588d1d4e570b59. </td></tr>
<tr><td><code>tests/testthat/test-ecoregion-live.R</code></td><td>8–8</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 76 lines; 0 expect_error calls; SHA-256 112908e6a4d33ce0. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-ecoregion.R</code></td><td>117–120</td><td><pre><code class="language-r">  testthat::expect_error(
    process_ecoregion(&quot;/invalid/path/to/file.gpkg&quot;),
    &quot;path&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_ecoregion(&quot;/invalid/path/to/file.gpkg&quot;),
    &quot;path&quot;
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 1133 lines; 9 expect_error calls; SHA-256 6616d1a6563c26f0. Direct/static-transitive regression links: calculate_covariates, calculate_ecoregion, download_data, process_covariates. </td></tr>
<tr><td><code>tests/testthat/test-edgar-live.R</code></td><td>11–11</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 75 lines; 0 expect_error calls; SHA-256 cbc337a12ddf294d. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-edgar.R</code></td><td>339–353</td><td><pre><code class="language-r">  testthat::expect_error(
    suppressWarnings(
      amadeus::download_edgar(
        species = &quot;CO&quot;,
        temp_res = &quot;yearly&quot;,
        sector_yearly = &quot;ENE&quot;,
        year_range = c(2015, 2016, 2017),
        acknowledgement = TRUE,
        download = FALSE,
        directory_to_save = paste0(tempdir(), &quot;/e/&quot;),
        unzip = FALSE
      )
    ),
    &quot;year_range must be of length 1 or 2&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    suppressWarnings(
      amadeus::download_edgar(
        species = &quot;CO&quot;,
        temp_res = &quot;yearly&quot;,
        sector_yearly = &quot;ENE&quot;,
        year_range = c(2015, 2016, 2017),
        acknowledgement = TRUE,
        download = FALSE,
        directory_to_save = paste0(tempdir(), &quot;/e/&quot;),
        unzip = FALSE
      )
    ),
    &quot;year_range must be of length 1 or 2&quot;
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 1137 lines; 15 expect_error calls; SHA-256 63331bca82442dd3. Direct/static-transitive regression links: calculate_edgar. </td></tr>
<tr><td><code>tests/testthat/test-geos-live.R</code></td><td>13–13</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 71 lines; 0 expect_error calls; SHA-256 10195372a4ce52e8. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-geos.R</code></td><td>299–301</td><td><pre><code class="language-r">  testthat::expect_error(
    process_geos()
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_geos()
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 720 lines; 6 expect_error calls; SHA-256 c7e2b2593d64d4af. Direct/static-transitive regression links: calculate_geos, process_covariates. </td></tr>
<tr><td><code>tests/testthat/test-gmted-live.R</code></td><td>11–11</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 66 lines; 0 expect_error calls; SHA-256 98c20da5018f6399. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-gmted.R</code></td><td>280–290</td><td><pre><code class="language-r">  testthat::expect_error(
    gmted &lt;-
      process_gmted(
        variable &lt;- &quot;Breakline Emphasis; 7.5 arc-seconds&quot;,
        path = testthat::test_path(
          &quot;..&quot;,
          &quot;testdata&quot;,
          &quot;gmted&quot;
        )
      )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    gmted &lt;-
      process_gmted(
        variable &lt;- &quot;Breakline Emphasis; 7.5 arc-seconds&quot;,
        path = testthat::test_path(
          &quot;..&quot;,
          &quot;testdata&quot;,
          &quot;gmted&quot;
        )
      )
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 495 lines; 2 expect_error calls; SHA-256 626943371ba2c7e1. Direct/static-transitive regression links: download_data. </td></tr>
<tr><td><code>tests/testthat/test-goes-live.R</code></td><td>11–11</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 66 lines; 0 expect_error calls; SHA-256 935918fccc48deec. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-goes.R</code></td><td>24–27</td><td><pre><code class="language-r">  testthat::expect_error(
    goes_parse_start_datetime(&quot;bad_filename.nc&quot;),
    regexp = &quot;Cannot parse&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    goes_parse_start_datetime(&quot;bad_filename.nc&quot;),
    regexp = &quot;Cannot parse&quot;
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 1030 lines; 14 expect_error calls; SHA-256 6876993af6df9634. Direct/static-transitive regression links: calculate_covariates, calculate_goes, download_data, process_covariates. </td></tr>
<tr><td><code>tests/testthat/test-gridmet-live.R</code></td><td>31–31</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 125 lines; 0 expect_error calls; SHA-256 f9ea094e7ccbabe0. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-gridmet.R</code></td><td>99–107</td><td><pre><code class="language-r">  testthat::expect_error(
    download_data(
      dataset_name = &quot;gridmet&quot;,
      variables = &quot;Precipitation&quot;,
      year = c(10, 11),
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), &quot;/g/&quot;)
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    download_data(
      dataset_name = &quot;gridmet&quot;,
      variables = &quot;Precipitation&quot;,
      year = c(10, 11),
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), &quot;/g/&quot;)
    )
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 1026 lines; 8 expect_error calls; SHA-256 10a852347d32afa4. Direct/static-transitive regression links: calculate_gridmet, download_data, process_gridmet, process_parse_ncdf_day_codes. </td></tr>
<tr><td><code>tests/testthat/test-groads-live.R</code></td><td>11–11</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 72 lines; 0 expect_error calls; SHA-256 44e8da6046ba45bb. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-groads.R</code></td><td>144–147</td><td><pre><code class="language-r">  testthat::expect_error(
    process_groads(path = 1L),
    regexp = &quot;must be a single file path to a \\.shp or \\.gdb roads file&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_groads(path = 1L),
    regexp = &quot;must be a single file path to a \\.shp or \\.gdb roads file&quot;
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 454 lines; 7 expect_error calls; SHA-256 6a5db5b3ef1622c7. Direct/static-transitive regression links: calculate_groads, download_data, process_groads. </td></tr>
<tr><td><code>tests/testthat/test-hms-live.R</code></td><td>14–14</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 69 lines; 0 expect_error calls; SHA-256 1c6845ae027fba26. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-hms.R</code></td><td>79–87</td><td><pre><code class="language-r">  testthat::expect_error(
    download_data(
      dataset_name = &quot;hms&quot;,
      acknowledgement = TRUE,
      directory_to_save = error_directory,
      unzip = FALSE,
      remove_zip = TRUE
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    download_data(
      dataset_name = &quot;hms&quot;,
      acknowledgement = TRUE,
      directory_to_save = error_directory,
      unzip = FALSE,
      remove_zip = TRUE
    )
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 1469 lines; 7 expect_error calls; SHA-256 4e5b2caf993def9c. Direct/static-transitive regression links: calculate_hms, download_data. </td></tr>
<tr><td><code>tests/testthat/test-huc-live.R</code></td><td>11–11</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 66 lines; 0 expect_error calls; SHA-256 dc8b6f9556688fe5. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-huc.R</code></td><td>129–129</td><td><pre><code class="language-r">  testthat::expect_error(process_huc(path))</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(process_huc(path))</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 231 lines; 4 expect_error calls; SHA-256 fc5af4c524f587cd. </td></tr>
<tr><td><code>tests/testthat/test-improve-live.R</code></td><td>11–11</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 84 lines; 0 expect_error calls; SHA-256 c05dda99a013817c. Direct/static-transitive regression links: calculate_covariates. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-improve.R</code></td><td>105–108</td><td><pre><code class="language-r">  testthat::expect_error(
    process_improve(path = &quot;/nonexistent_dir_xyz&quot;),
    regexp = &quot;valid directory&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_improve(path = &quot;/nonexistent_dir_xyz&quot;),
    regexp = &quot;valid directory&quot;
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 571 lines; 14 expect_error calls; SHA-256 0c7b667a081a0f2b. Direct/static-transitive regression links: calculate_covariates, calculate_improve, download_data, process_covariates. </td></tr>
<tr><td><code>tests/testthat/test-info-helpers.R</code></td><td>67–70</td><td><pre><code class="language-r">      testthat::expect_error(
        get_geos_info(path = &quot;.&quot;),
        regexp = &quot;No GEOS-CF \\.nc4 files&quot;
      )</code></pre></td><td><pre><code class="language-r">      testthat::expect_error(
        get_geos_info(path = &quot;.&quot;),
        regexp = &quot;No GEOS-CF \\.nc4 files&quot;
      )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 134 lines; 10 expect_error calls; SHA-256 bbcf1494787e0de1. Direct/static-transitive regression links: get_geos_info, get_merra2_info, get_modis_info. </td></tr>
<tr><td><code>tests/testthat/test-koppen-geiger-live.R</code></td><td>11–11</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 69 lines; 0 expect_error calls; SHA-256 39e5e1fa78104aad. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-koppen-geiger.R</code></td><td>29–37</td><td><pre><code class="language-r">      testthat::expect_true(
        length(
          list.files(
            directory_to_save,
            include.dirs = TRUE
          )
        ) &gt;=
          1
      )</code></pre></td><td><pre><code class="language-r">      testthat::expect_true(
        length(
          list.files(
            directory_to_save,
            include.dirs = TRUE
          )
        ) &gt;=
          1
      )</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 248 lines; 0 expect_error calls; SHA-256 cf94fd94f435a76d. Direct/static-transitive regression links: calculate_koppen_geiger, download_data. </td></tr>
<tr><td><code>tests/testthat/test-merra2-live.R</code></td><td>14–14</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 92 lines; 0 expect_error calls; SHA-256 83d2b496ce0e1ae4. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-merra2.R</code></td><td>247–256</td><td><pre><code class="language-r">  testthat::expect_error(
    download_data(
      dataset_name = &quot;merra2&quot;,
      date = &quot;2024-01-02&quot;,
      collection = &quot;inst1_2d_int_Nx&quot;,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE
    ),
    &quot;NASA_EARTHDATA_TOKEN&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    download_data(
      dataset_name = &quot;merra2&quot;,
      date = &quot;2024-01-02&quot;,
      collection = &quot;inst1_2d_int_Nx&quot;,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE
    ),
    &quot;NASA_EARTHDATA_TOKEN&quot;
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 1366 lines; 6 expect_error calls; SHA-256 955d76af1d593255. Direct/static-transitive regression links: calculate_merra2, download_data, process_covariates. </td></tr>
<tr><td><code>tests/testthat/test-modis-live.R</code></td><td>11–11</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 110 lines; 0 expect_error calls; SHA-256 4c2566549a744415. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-modis.R</code></td><td>156–169</td><td><pre><code class="language-r">  testthat::expect_error(
    download_data(
      dataset_name = &quot;modis&quot;,
      date = c(date_start, date_end),
      product = product,
      version = version,
      extent = vec_extent,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    ),
    &quot;NASA_EARTHDATA_TOKEN&quot;
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    download_data(
      dataset_name = &quot;modis&quot;,
      date = c(date_start, date_end),
      product = product,
      version = version,
      extent = vec_extent,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE,
      remove_command = FALSE
    ),
    &quot;NASA_EARTHDATA_TOKEN&quot;
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 3690 lines; 51 expect_error calls; SHA-256 ef1a829f62deed73. Direct/static-transitive regression links: calculate_modis, download_data, process_flatten_sds, process_mcd14ml, process_modis_daily, process_modis_merge. </td></tr>
<tr><td><code>tests/testthat/test-narr-live.R</code></td><td>11–11</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 83 lines; 0 expect_error calls; SHA-256 6e1d8c250482452f. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-narr.R</code></td><td>101–109</td><td><pre><code class="language-r">  testthat::expect_error(
    download_data(
      dataset_name = &quot;narr&quot;,
      variables = &quot;weasd&quot;,
      year = c(10, 11),
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path(&quot;..&quot;, &quot;testdata/&quot;, &quot;&quot;)
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    download_data(
      dataset_name = &quot;narr&quot;,
      variables = &quot;weasd&quot;,
      year = c(10, 11),
      acknowledgement = TRUE,
      directory_to_save = testthat::test_path(&quot;..&quot;, &quot;testdata/&quot;, &quot;&quot;)
    )
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 718 lines; 4 expect_error calls; SHA-256 e0084f7ff1160876. Direct/static-transitive regression links: calculate_narr, download_data. </td></tr>
<tr><td><code>tests/testthat/test-nei-live.R</code></td><td>11–11</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 63 lines; 0 expect_error calls; SHA-256 30773784b81089bb. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-nei.R</code></td><td>266–268</td><td><pre><code class="language-r">  testthat::expect_error(
    process_nei(testthat::test_path(&quot;../testdata&quot;, &quot;modis&quot;), year = 2017)
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    process_nei(testthat::test_path(&quot;../testdata&quot;, &quot;modis&quot;), year = 2017)
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 628 lines; 14 expect_error calls; SHA-256 01e78c88ab549eaf. Direct/static-transitive regression links: download_data. </td></tr>
<tr><td><code>tests/testthat/test-nlcd-live.R</code></td><td>11–11</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 66 lines; 0 expect_error calls; SHA-256 5566c439581c5d9c. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-nlcd.R</code></td><td>94–110</td><td><pre><code class="language-r">    testthat::expect_error(
      testthat::expect_warning(
        testthat::expect_warning(
          download_data(
            dataset_name = &quot;nlcd&quot;,
            year = 1900,
            product = &quot;land cover&quot;,
            directory_to_save = directory_to_save,
            acknowledgement = TRUE,
            download = FALSE,
            remove_command = TRUE
          ),
          regexp = &quot;Setting download=FALSE is deprecated&quot;
        ),
        regexp = &quot;Parameter &#x27;remove_command&#x27; is deprecated&quot;
      )
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      testthat::expect_warning(
        testthat::expect_warning(
          download_data(
            dataset_name = &quot;nlcd&quot;,
            year = 1900,
            product = &quot;land cover&quot;,
            directory_to_save = directory_to_save,
            acknowledgement = TRUE,
            download = FALSE,
            remove_command = TRUE
          ),
          regexp = &quot;Setting download=FALSE is deprecated&quot;
        ),
        regexp = &quot;Parameter &#x27;remove_command&#x27; is deprecated&quot;
      )
    )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 1049 lines; 20 expect_error calls; SHA-256 144d570b4ba81a2f. Direct/static-transitive regression links: calculate_nlcd, download_data, process_nlcd. </td></tr>
<tr><td><code>tests/testthat/test-population-live.R</code></td><td>11–11</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 75 lines; 0 expect_error calls; SHA-256 6cdbaf04550b324f. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-population.R</code></td><td>166–175</td><td><pre><code class="language-r">    testthat::expect_error(
      download_data(
        dataset_name = &quot;sedac_population&quot;,
        year = &quot;2020&quot;,
        data_format = &quot;InvalidFormat&quot;,
        data_resolution = &quot;30 second&quot;,
        directory_to_save = directory_to_save,
        acknowledgement = TRUE
      )
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      download_data(
        dataset_name = &quot;sedac_population&quot;,
        year = &quot;2020&quot;,
        data_format = &quot;InvalidFormat&quot;,
        data_resolution = &quot;30 second&quot;,
        directory_to_save = directory_to_save,
        acknowledgement = TRUE
      )
    )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 950 lines; 9 expect_error calls; SHA-256 c02cd5d801528306. Direct/static-transitive regression links: download_data. </td></tr>
<tr><td><code>tests/testthat/test-prism-live.R</code></td><td>11–11</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 47 lines; 0 expect_error calls; SHA-256 0cef5b9bcf6cd9b6. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-prism.R</code></td><td>84–94</td><td><pre><code class="language-r">  testthat::expect_error(
    download_prism(
      time = &quot;202105&quot;,
      element = &quot;soltotal&quot;,
      data_type = &quot;ts&quot;,
      format = &quot;nc&quot;,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    download_prism(
      time = &quot;202105&quot;,
      element = &quot;soltotal&quot;,
      data_type = &quot;ts&quot;,
      format = &quot;nc&quot;,
      directory_to_save = directory_to_save,
      acknowledgement = TRUE,
      download = FALSE
    )
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 521 lines; 10 expect_error calls; SHA-256 39f76911bdda8b82. Direct/static-transitive regression links: calculate_prism, process_prism. </td></tr>
<tr><td><code>tests/testthat/test-process.R</code></td><td>178–182</td><td><pre><code class="language-r">    testthat::expect_error(
      process_covariates(
        covariate = cty
      )
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      process_covariates(
        covariate = cty
      )
    )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 523 lines; 11 expect_error calls; SHA-256 a24dbc77bff23d93. Direct/static-transitive regression links: calculate_narr, process_conformity, process_covariates, process_modis_daily, process_parse_ncdf_day_codes. </td></tr>
<tr><td><code>tests/testthat/test-sedc.R</code></td><td>76–85</td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols,
      geom = TRUE
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    sum_edc(
      locs = ncpt,
      from = tri_r,
      locs_id = &quot;site_id&quot;,
      decay_range = 30000,
      target_fields = targcols,
      geom = TRUE
    )
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 254 lines; 7 expect_error calls; SHA-256 11c25d7236c580fb. Direct/static-transitive regression links: process_tri, sum_edc. </td></tr>
<tr><td><code>tests/testthat/test-spacetime.R</code></td><td>28–28</td><td><pre><code class="language-r">  testthat::expect_error(check_mysftime(stdata), &quot;x is not a sftime&quot;)</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(check_mysftime(stdata), &quot;x is not a sftime&quot;)</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 639 lines; 23 expect_error calls; SHA-256 935142c74bd5f6ac. </td></tr>
<tr><td><code>tests/testthat/test-terraclimate-live.R</code></td><td>24–24</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 76 lines; 0 expect_error calls; SHA-256 9a49f8281e6cbee8. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-terraclimate.R</code></td><td>61–69</td><td><pre><code class="language-r">  testthat::expect_error(
    download_data(
      dataset_name = &quot;terraclimate&quot;,
      variables = &quot;Precipitation&quot;,
      year = c(10, 11),
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), &quot;/epa/&quot;)
    )
  )</code></pre></td><td><pre><code class="language-r">  testthat::expect_error(
    download_data(
      dataset_name = &quot;terraclimate&quot;,
      variables = &quot;Precipitation&quot;,
      year = c(10, 11),
      acknowledgement = TRUE,
      directory_to_save = paste0(tempdir(), &quot;/epa/&quot;)
    )
  )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 478 lines; 5 expect_error calls; SHA-256 b91b40609ddc32ce. Direct/static-transitive regression links: calculate_terraclimate, download_data. </td></tr>
<tr><td><code>tests/testthat/test-tri-live.R</code></td><td>11–11</td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td><pre><code class="language-r">    skip_if_no_live_tests()</code></pre></td><td>Coverage inventory — excerpt retained — No new guard is justified: retain existing output checks, fixture construction, data definitions or skip/mock control flow. Full file scanned: 63 lines; 0 expect_error calls; SHA-256 2ddede084e9746a3. Direct/static-transitive regression links: download_tri. Preserve skip_if_no_live_tests and any credential/transient skips. No live requests were executed. </td></tr>
<tr><td><code>tests/testthat/test-tri.R</code></td><td>87–96</td><td><pre><code class="language-r">    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = &quot;.&quot;,
        acknowledgement = TRUE,
        jurisdiction = &quot;northcarolina&quot;,
        download = FALSE
      ),
      regexp = &quot;jurisdiction&quot;
    )</code></pre></td><td><pre><code class="language-r">    testthat::expect_error(
      download_tri(
        year = 2024L,
        directory_to_save = &quot;.&quot;,
        acknowledgement = TRUE,
        jurisdiction = &quot;northcarolina&quot;,
        download = FALSE
      ),
      regexp = &quot;jurisdiction&quot;
    )</code></pre></td><td>Coverage inventory — excerpt retained — No additional validation replacement justified by the error expectations; preserve the package call and diagnostic. Full file scanned: 948 lines; 34 expect_error calls; SHA-256 40ea269409928eca. Direct/static-transitive regression links: calculate_covariates, calculate_tri, download_data, download_tri, get_tri_info, process_tri. </td></tr>
</tbody></table></div>

# Compatibility and implementation verification

No production, test, dependency or existing review file was modified. This report does not authorize an implementation. Runtime checkmate use would require a declared dependency; DESCRIPTION currently does not declare checkmate. That dependency change is outside this documentation-only task.

Static checks performed: read every source/test file; enumerate validation calls and control-flow guards; trace named function calls and wrapper entry points; inspect diagnostic-sensitive expectations and live-test helpers; compare every quoted Current Code range with the actual file; check all table rows have five populated cells. No generated snippets were executed. Rscript fails at startup because libRblas.so cannot be loaded, including an attempt with the R library directory supplied through LD_LIBRARY_PATH. Therefore no unit-test pass or universal old/new equivalence is claimed.

Before any later implementation, use an R environment with the package and a pinned checkmate version. Compare the old and proposed predicates on ordinary and attributed/classed values, typed zero-length values, NULL, NA/NaN/Inf, wrong-type missing values, scalar/multiple values, whitespace and optional arguments. Keep function return values/visibility and branch order. Run the affected mocked test files and the full mocked suite once the proposed changes exist. Do not run live APIs merely to test local input validation; preserve existing opt-in gates.

For exact error-condition/call-stack compatibility across every possible R object, retain the original guards. Predicate hybrids preserve the package's explicit stop messages and successful control flow more closely than replacing entire blocks with assert*(), but cannot be certified identical here without runtime differential checks.
