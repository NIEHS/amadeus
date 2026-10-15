# amadeus Agent Definitions

This directory contains LLM/AI specialist agent definitions for the
**amadeus** R package. The specialists cover the three API tiers and
cross-cutting test work; each has a prompt and descriptive YAML metadata.

> **Note:** This directory is listed in `.Rbuildignore` — it has no impact
> on `R CMD CHECK`, test coverage, or any CI/CD workflow.

## Agents

| System Prompt | Metadata | Domain |
|---|---|---|
| [`download-agent.md`](download-agent.md) | [`download-agent.yaml`](download-agent.yaml) | `download_data()` + all `download_*()` functions |
| [`process-agent.md`](process-agent.md) | [`process-agent.yaml`](process-agent.yaml) | `process_covariates()` + all `process_*()` functions |
| [`calculate-agent.md`](calculate-agent.md) | [`calculate-agent.yaml`](calculate-agent.yaml) | `calculate_covariates()` + all `calculate_*()` functions |
| [`test-agent.md`](test-agent.md) | [`test-agent.yaml`](test-agent.yaml) | testthat unit/integration tests |

## How agent resources work together for repository tasks

These files are complementary, not competing sources of instructions. Each has
a distinct role:

| Component | Role | Use it for |
|---|---|---|
| [Repository instructions](../AGENTS.md) | Package-wide context and conventions | General repository guidance |
| [Amadeus AI Coding Protocol](Amadeus_AI_Coding_Protocol.md) | Shared governance and development workflow | Operating mode, risk, approval, validation, and handoff requirements |
| Tier skill checklists | Task-specific checks | Detailed tier practices; consult the matching file under `../.agents/skills/` |
| Specialist prompts | Focused role and domain context | Selecting the lead agent and scoping its work |
| Specialist YAML | Descriptive agent metadata | Finding relevant domains, files, and declared tools; not an executable prompt by itself |
| Source code, tests, and documentation | Evidence of current package behavior | Confirming the actual implementation and contracts |

The protocol is authoritative for the shared workflow. Specialist prompts and
skills should add role-specific context without overriding it. Repository code,
tests, and documentation determine current behavior; verify prompt inventories
and examples against them before relying on those details.

### Select a lead specialist

- **Download Agent** — provider selection, authentication, transfers, archives,
  and downloaded-file layout. See [prompt](download-agent.md), [metadata](download-agent.yaml),
  and [download skill](../.agents/skills/download.md).
- **Process Agent** — reading source files and producing spatial or
  spatiotemporal objects. See [prompt](process-agent.md), [metadata](process-agent.yaml),
  and [process skill](../.agents/skills/process.md).
- **Calculate Agent** — extracting covariates and shaping location-level
  results. See [prompt](calculate-agent.md), [metadata](calculate-agent.yaml),
  and [calculate skill](../.agents/skills/calculate.md).
- **Test Agent** — test design and verification across all tiers. See
  [prompt](test-agent.md), [metadata](test-agent.yaml), and
  [test skill](../.agents/skills/test.md). Testing is cross-cutting; it is not
  a fourth data-processing tier.

For a task crossing tiers, choose one lead to own the overall change and
involve the other affected specialists for bounded reviews or implementation
tasks. Make the handoff contract between tiers explicit.

### Apply the materials to a task

1. Start with the repository instructions and the shared protocol.
2. Select the lead specialist by the code path or behavior in scope.
3. Add the relevant specialist prompt and any substantive tier skill to the
   task context. Use YAML as descriptive routing metadata unless your agent
   environment explicitly supports it.
4. Have the agent inspect current implementation, tests, and downstream
   contracts before proposing or making changes.
5. Follow the protocol's mandatory workflow and handoff requirements; use the
   skill for additional checks specific to the tier.

Skills, prompts, and YAML files are repository resources; their presence does
not guarantee that a particular editor or agent runtime automatically loads
them. Configure your environment's supported instructions, prompt, or skill
mechanism, or explicitly provide the relevant files when starting a task.
Keep detailed workflow rules in the protocol and detailed tier checks in the
skills rather than duplicating them here.

## Package overview (shared context)

**amadeus** (**a** **m**echanism for **d**ata, **e**nvironments, and **u**ser **s**etup)
downloads, processes, and extracts spatiotemporal environmental data from 20+ public sources.

Three-tier API:
1. `download_data(dataset_name, ...)` → raw files on disk
2. `process_covariates(covariate, path, ...)` → `SpatRaster` / `SpatVector` / `sf`
3. `calculate_covariates(covariate, from, locs, locs_id, ...)` → `data.frame` / `SpatVector`
