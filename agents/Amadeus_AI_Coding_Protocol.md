# Amadeus AI Coding Protocol

Governance and operating standards for AI-assisted development

# 1. Purpose

AI agents support human developers in maintaining and extending the Amadeus R
package. They may investigate, propose, implement, test, and document changes
within an explicitly assigned scope.

Agents are development assistants, not autonomous architects or scientific
decision-makers. Human maintainers retain authority over:

- Package architecture
- Stable-core behavior
- Public APIs
- Scientific assumptions
- Metadata and schema contracts
- Dependency policy
- Releases and merges

The objective is correctness, reproducibility, maintainability, scientific
validity, and backward compatibility---not the amount of code produced.

# 2. Core principles

Every AI-generated change must:

1. Understand the relevant Amadeus architecture before editing.
2. Preserve the stable core unless explicitly authorized to change it.
3. Make the smallest coherent change that satisfies the request.
4. Reuse existing helpers and patterns where appropriate.
5. Add or update tests for changed behavior.
6. Validate affected spatial, temporal, and metadata contracts.
7. Preserve public APIs unless a breaking change is explicitly approved.
8. Never silently change a scientific assumption.
9. Avoid unrelated cleanup and speculative abstraction.
10. Be reviewable, reproducible, and explainable to a human developer.
11. Report what was verified and what was not.
12. Preserve unrelated work already present in the working tree.

An agent must not treat passing tests as sufficient evidence when the tests do not cover the scientific or metadata contract being changed.

# 3. The Amadeus stable core

For this protocol, the stable core includes:

- The three-tier workflow: `download_data()`, `process_covariates()`, and
  `calculate_covariates()`
- Dataset names and aliases accepted by public dispatchers
- Exported function names and argument semantics
- Return classes and documented output schemas
- Location identifier behavior through `locs_id`
- CRS, spatial units, extent, resolution, and geometry semantics
- Dates, time zones, temporal resolution, and layer ordering
- Variable names, units, scaling factors, classifications, and missing-value
  rules
- Download acknowledgement, authentication, retry, and throttling behavior
- Hashing and file-layout behavior
- The mocked-versus-live testing architecture
- Metadata consumed by downstream users or workflows

The following are critical changes:

- Removing or renaming exported functions
- Changing defaults or argument meanings
- Changing dispatcher names or aliases
- Changing return classes or column names
- Changing units, CRS, spatial resolution, or time interpretation
- Changing aggregation, interpolation, scaling, weighting, or classification
- Changing missing-value behavior
- Changing downloaded file layouts relied upon by later tiers
- Adding or replacing a scientific algorithm
- Adding a required dependency
- Changing package metadata or supported R versions

Agents may investigate and propose critical changes. They must not implement
them without explicit human authorization covering the critical change.

# 4. Agent operating modes

## 4.1 Explore

Agents may perform read-only investigation without additional approval.

Permitted activities include:

- Reading code, tests, documentation, configuration, and version history
- Tracing dispatch and data flow
- Reproducing test failures
- Running non-destructive checks
- Identifying inconsistencies
- Comparing behavior with documented contracts
- Preparing implementation plans
- Proposing architectural or scientific changes

Exploration does not authorize external writes, package releases, merges, or
changes to remote services.

## 4.2 Develop

Agents may modify files when the user has requested implementation.

Permitted activities include:

- Fixing bugs
- Adding approved functionality
- Writing or updating tests
- Updating documentation
- Regenerating Roxygen artifacts
- Refactoring within the requested scope
- Creating a branch when requested or required by the workflow
- Preparing a commit or pull request when explicitly requested

All changes require human review before merge. Agents must not merge their own
changes.

## 4.3 Critical

Critical mode applies to stable-core, API, metadata, schema, dependency, or
scientific changes.

Before implementation, the agent must present:

- The problem and evidence
- The proposed change
- Scientific and architectural justification
- Compatibility impact
- Migration requirements
- Alternatives considered
- Required regression tests
- Documentation impact
- Rollback strategy

Explicit human authorization is required before implementing the critical
portion.

# 5. Mandatory workflow

Every development task follows:

    -> Understand
    -> Inspect
    -> Define contract and risk
    -> Plan
    -> Implement
    -> Test
    -> Validate scientific and metadata behavior
    -> Review the diff
    -> Prepare commit or PR when requested
    -> Human approval and merge

# 6. Repository inspection requirements

Inspection must be proportional to the task. Agents should inspect relevant portions of:

- `AGENTS.md` and `agents/`
- `README.md`
- `DESCRIPTION`
- `NAMESPACE`
- `.lintr`
- The appropriate public dispatcher
- The source-specific implementation
- Shared helpers called by that implementation
- Existing tests for the affected dataset
- Testing helpers and fixtures
- Relevant vignettes
- Relevant CI workflows
- `NEWS.md` for recent related changes
- Git status and the existing diff
- Related issues or pull requests when supplied or accessible

Agents should not reread the entire repository mechanically for a one-line
localized task. They must, however, inspect enough surrounding code to
understand the applicable contract.

Before adding a new dataset, inspect all three dispatchers even if only one or
two tiers will be implemented.

# 7. Prompting and coding workflow

Prompting in Amadeus should support effectiveness, accuracy, reproducibility,
and consistency. Effective AI-assisted development begins with a prompt that
defines an observable outcome and continues through an evidence-driven coding
loop. The goal is not an elaborate prompt, but enough concrete, local context
for the agent to act on the correct layer with minimal guessing. A prompt is a
working specification: it guides investigation and implementation, but it does
not override repository evidence, the stable-core rules, or human review.

## 7.1 Prompt construction

A development prompt should provide, when known:

1. Objective: the user-visible behavior or problem to resolve.
2. Scope: affected dataset, API tier, functions, and files, plus anything
   explicitly out of scope.
3. Evidence: error output, failing test, issue, provider documentation,
   sample input, or current-versus-expected behavior.
4. Contract: expected inputs, output class and schema, identifiers, units,
   CRS, time behavior, side effects, errors, and warnings.
5. Constraints: backward compatibility, dependency, network, performance,
   security, and scientific requirements.
6. Acceptance criteria: specific conditions that demonstrate completion.
7. Verification: focused tests and broader checks expected before handoff.
8. Deliverables: code, tests, documentation, migration notes, or a
   diagnosis-only report.

Prompts should distinguish observed facts, hypotheses, requirements, and
suggestions. They should state what must remain unchanged and name uncertainties
instead of filling them with unsupported assumptions. File names and proposed
implementations may be included as leads, but the agent must confirm them
against the repository before editing.

Prefer concrete anchors such as `download_geos()` authentication failure,
`process_narr()` time dimensions, `calculate_modis()` identifier behavior, or a
failing `tests/testthat/test-narr.R` check. A short prompt anchored to the owning
tier and a controlling symptom is more useful than a broad request to repair
the repository.

A concise prompt is sufficient when the contract is already established in the
repository. More detail is required for new datasets, scientific changes,
cross-tier changes, or bugs with multiple plausible interpretations.

## 7.2 Intake and prompt refinement

Before coding, translate the request into a task brief containing:

- The requested outcome
- The operating mode: Explore, Develop, or Critical
- The affected tier or tiers
- The behavior that must remain unchanged
- The evidence needed to confirm the problem
- The acceptance criteria
- The intended validation depth
- Known unknowns and assumptions

Resolve uncertainty in proportion to its impact:

- Make and disclose a low-risk, reversible assumption when repository
  conventions clearly support it.
- Ask a focused question when different answers would materially alter the
  public API, scientific result, dependency set, data schema, or amount of
  work.
- Stop and request explicit authorization before implementing a critical
  change.
- Do not ask the user for information that can be obtained safely from the
  repository, supplied logs, or existing tests.

If the request is diagnosis-only, investigate and report the cause without
editing. If the request asks for implementation, carry the work through code,
tests, documentation, and diff review unless a stated constraint prevents it.

## 7.3 Context assembly

Build context progressively rather than placing the entire repository in a
prompt. Start with the nearest authoritative material:

1. Applicable instructions and the current user request
2. Git status and existing diffs
3. Public dispatcher and source-specific implementation
4. Directly called helpers and adjacent-tier contracts
5. Existing tests, fixtures, and testing conventions
6. Roxygen documentation and relevant vignettes
7. Package metadata, CI, and history when relevant

Trace one representative path from public entry point to output before
changing it. For a bug, reproduce or characterize the failure first. For a new
dataset, map download, process, and calculate responsibilities even when the
requested implementation covers fewer than three tiers.

Context supplied to an AI tool must exclude credentials, protected data, and
irrelevant large artifacts. Prefer minimal representative fixtures and redact
sensitive values without obscuring the behavior under investigation.

## 7.4 Plan-to-code loop

Use the following loop for implementation work:

    -> Frame the task
    -> Establish the current behavior
    -> State the contract and risk
    -> Select the smallest coherent change
    -> Write or identify a failing check
    -> Implement
    -> Run focused validation
    -> Inspect outputs and the diff
    -> Broaden validation as risk requires
    -> Document and hand off

## 7.5 Coding practices expected of agents

Agents must demonstrate the following practices while implementing changes:

- Repository navigation: locate definitions, call sites, tests, exports,
  and documentation before editing.
- Contract-first reasoning: state observable behavior independently of a
  proposed implementation.
- Boundary validation: validate user and provider inputs at the earliest
  appropriate boundary without duplicating checks throughout the call chain.
- Data-shape awareness: reason explicitly about class, dimensions, names,
  types, ordering, missingness, and identifier cardinality.
- Spatial and temporal awareness: verify CRS, units, geometry, extent,
  resolution, dates, time zones, and layer ordering when applicable.
- Failure-path design: provide errors and warnings that identify the failed
  condition and the corrective action without exposing secrets.
- Appropriate R idioms: prefer clear vectorized or table operations over
  avoidable loops, use explicit package namespaces where appropriate, and
  follow existing `data.table`, `dplyr`, `tidyr`, `terra`, `sf`, and `httr2`
  patterns.
- Parallel safety: pass file paths rather than in-memory `terra` objects
  across workers and preserve deterministic ordering and random-number behavior.
- Test design: isolate network and filesystem boundaries, reuse fixtures,
  test representative values, and include meaningful failure cases.
- Documentation discipline: keep Roxygen, examples, vignettes, generated
  artifacts, and actual behavior consistent.
- Change discipline: avoid speculative abstractions, unrelated formatting,
  hidden side effects, and duplicated helpers; never alter raw source data to
  make a workflow or test succeed.
- Evidence-based completion: report exact checks, provenance, assumptions,
  and limitations without claiming verification that was not performed.

When generating a substantial block of code, break the work into reviewable
units with an explicit contract for each unit. Generated code is a draft until
it has been read in context, executed where practical, and validated against
representative outputs.

## 7.6 Multi-agent prompting and accountability

Use multiple agents only when the work divides into bounded, independently
verifiable subtasks that reduce elapsed time or provide useful independent
review. Suitable work includes inspecting separate tiers, investigating code
while another worker designs tests, independently reviewing documentation,
running distinct validation suites, comparing provider documentation with
package behavior, or reviewing a completed implementation.

Do not delegate when a task is small and localized, workers would edit the same
file, scientific or API decisions remain unresolved, acceptance criteria cannot
be stated, coordination costs outweigh the benefit, or delegation would
separate a scientific decision from its implementation and validation. Apply
the operational safeguards in Section 22.4 to every delegated task.

One lead agent remains accountable for task definition, coordination,
integration, validation, and final reporting. Delegation neither transfers
that accountability nor expands the authority granted by the user.

Before delegation, the lead agent must establish the overall objective, scope,
contract, risk classification, dependencies, and acceptance criteria. Each
worker prompt must identify:

- The worker's bounded responsibility and the boundary or handoff being
  examined
- Whether the assignment is read-only or permits edits
- Non-overlapping file ownership when edits are permitted
- Inputs, dependencies, acceptance criteria, and evidence to return
- Whether another worker is expected to validate rather than edit
- Scientific, stable-core, API, and integration decisions retained by the lead
  unless a human maintainer assigns them elsewhere

During execution, the lead agent must maintain a current view of assignments,
file ownership, dependencies, blockers, changed assumptions, and validation
already run or still required. The lead must inspect worker evidence and output
directly; a worker's completion statement alone is not sufficient for
integration.

## 7.7 Prompt patterns by task type

The following patterns may be adapted rather than copied mechanically.

### Diagnosis

Investigate <observed behavior> in <function/dataset>.
Reproduce it using <input or test>, trace the relevant call path, and identify
the root cause with file-level evidence. Do not modify files. Report contract
impact, likely fix scope, and the tests that would prevent recurrence.

### Bug fix

Fix <observed behavior> in <function/dataset> while preserving <contracts>.
Add an offline regression test that demonstrates <expected behavior> and the
relevant failure path. Run <focused checks>, review the diff, and report any
broader checks not run. Do not change <explicit exclusions>.

### New dataset or feature

Add <dataset/feature> to <tiers> using <authoritative provider contract>.
Support <names/aliases/inputs> and return <class/schema/units/CRS/time rules>.
Reuse <relevant patterns>, add routine mocked tests and gated live tests where
appropriate, update documentation, and validate dispatcher-to-output behavior.
Treat any listed scientific or API decision as requiring maintainer approval.

### Refactor

Refactor <scope> to achieve <maintainability or performance objective> without
changing <public/scientific/metadata behavior>. Establish characterization
tests first, keep the diff scoped, compare representative outputs before and
after, and report any behavior that could not be proven equivalent.

### Code review

Review <diff or branch> for defects, regressions, security issues, and missing
tests. Prioritize findings by impact, cite file and line, explain the affected
contract and a reproduction scenario, and distinguish confirmed problems from
questions. Do not edit unless explicitly requested.

## 7.8 Iterative prompting and communication

Prompts may be refined as evidence emerges. Each follow-up should preserve the
task's accepted constraints and state what changed, for example:

- New evidence invalidated an assumption.
- A test exposed a cross-tier effect.
- Provider documentation conflicts with current behavior.
- A required critical change needs approval.
- An environmental limitation prevents a planned check.

Progress updates should be brief and evidence-based: what was inspected, what
was learned, what is changing next, and whether risk or scope has changed. Do
not report routine command-by-command narration, fabricate tool output, or hide
failed and skipped checks.

If repeated prompting produces patches without resolving the failure, stop
patching and return to diagnosis. Re-establish the reproduction, reduce it to
the smallest failing example, and verify the contract before making another
code change.

## 7.9 Prompting and coding anti-patterns

Avoid prompts or implementations that:

- Ask broadly to "improve," "modernize," or "clean up" code without an
  observable outcome.
- Prescribe a solution without stating the problem or acceptance criteria.
- Paste secrets, protected data, entire data products, or irrelevant files.
- Ask the agent to assume current URLs, schemas, or provider behavior without
  verification.
- Combine unrelated bugs, refactors, dependency changes, and documentation
  rewrites in one task.
- Optimize for fewer lines, maximal abstraction, or a passing test count at the
  expense of clarity and contract coverage.
- Replace specific assertions with existence checks.
- Mock the code under test so completely that orchestration is no longer
  exercised.
- Modify generated files manually when an authoritative source can regenerate
  them.
- Claim scientific correctness from syntax, lint, or unit-test success alone.

## 7.10 Reusable prompt template

Objective
- <observable outcome>

Scope
- Dataset/tier/functions: <...>
- In scope: <...>
- Out of scope: <...>

Current evidence
- Reproduction, error, test, or documentation: <...>

Required contract
- Inputs and aliases: <...>
- Output class/schema/order: <...>
- CRS/units/time/scientific rules: <...>
- Errors, warnings, and side effects: <...>
- Behavior that must remain unchanged: <...>

Constraints and approvals
- Compatibility/dependencies/network/security/performance: <...>
- Critical decisions already approved or still requiring approval: <...>

Acceptance criteria
- <testable criterion 1>
- <testable criterion 2>

Verification and deliverables
- Tests/checks to run: <...>
- Code, tests, documentation, or report expected: <...>
- Handoff details required: <...>

Before acting on this template, the agent must verify its repository-specific
claims. Before handoff, the agent must map each acceptance criterion to code,
test evidence, documentation, or an explicitly reported limitation.

# 8. AI-generated code requirements

AI-generated code must:

- Follow existing R and Roxygen conventions.
- Follow the line-length policy in `.lintr`.
- Use existing helpers instead of duplicating functionality.
- Avoid new dependencies unless approved.
- Avoid unnecessary abstraction.
- Validate inputs at the appropriate boundary.
- Produce actionable warnings and errors.
- Preserve existing API behavior unless authorized otherwise.
- Include tests for success and meaningful failure paths.
- Update documentation when behavior changes.
- Avoid modifying functioning code solely for style.
- Avoid mixing unrelated cleanup with behavioral changes.
- Use `httr2` for HTTP behavior.
- Use temporary directories in tests.
- Avoid network access in routine tests.
- Never include credentials or machine-specific absolute paths.
- Regenerate documentation rather than manually editing generated artifacts.

A bug fix should normally include a regression test that fails before the fix
and passes afterward.

# 9. Required handoff

At the end of a task, the agent reports:

1. What changed and why
2. Important files changed
3. Public, metadata, or scientific contract impact
4. Tests and checks run
5. Checks skipped or not run
6. Remaining risks or assumptions
7. Whether explicit human approval is still required

The agent must distinguish among:

- Implemented
- Tested locally
- Verified by live provider
- Proposed but not implemented
- Awaiting human approval
