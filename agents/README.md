# amadeus Agent Definitions

This directory contains LLM/AI specialist agent definitions for the
**amadeus** R package. Each agent is a system prompt + YAML metadata file
for one tier of the amadeus three-tier API.

> **Note:** This directory is listed in `.Rbuildignore` — it has no impact
> on `R CMD CHECK`, test coverage, or any CI/CD workflow.

## Agents

| System Prompt | Metadata | Domain |
|---|---|---|
| [`download-agent.md`](download-agent.md) | [`download-agent.yaml`](download-agent.yaml) | `download_data()` + all `download_*()` functions |
| [`process-agent.md`](process-agent.md) | [`process-agent.yaml`](process-agent.yaml) | `process_covariates()` + all `process_*()` functions |
| [`calculate-agent.md`](calculate-agent.md) | [`calculate-agent.yaml`](calculate-agent.yaml) | `calculate_covariates()` + all `calculate_*()` functions |
| [`test-agent.md`](test-agent.md) | [`test-agent.yaml`](test-agent.yaml) | testthat unit/integration tests |

## How to use

### As a system prompt in any LLM

1. Open your preferred LLM interface (Claude, ChatGPT, GitHub Copilot Chat, etc.)
2. Create a new conversation and paste the contents of the relevant `*-agent.md`
   file as the system prompt (or "custom instructions").
3. Ask questions, request issue triage, or ask for code generation in that domain.

### With GitHub Copilot workspace

Add a reference in `.github/copilot-instructions.md`:

```markdown
For download function issues, refer to agents/download-agent.md.
For process function issues, refer to agents/process-agent.md.
For calculate function issues, refer to agents/calculate-agent.md.
For test writing, refer to agents/test-agent.md.
```

### Choosing the right agent

- **Broken URL / new data source / authentication error** → Download Agent
- **Wrong output type / CRS mismatch / missing time dimension** → Process Agent
- **Wrong extracted values / missing locs_id column / geom handling** → Calculate Agent
- **Missing tests / failing tests / adding a new source** → Test Agent

## Package overview (shared context)

**amadeus** (**a** **m**echanism for **d**ata, **e**nvironments, and **u**ser **s**etup)
downloads, processes, and extracts spatiotemporal environmental data from 20+ public sources.

Three-tier API:
1. `download_data(dataset_name, ...)` → raw files on disk
2. `process_covariates(covariate, path, ...)` → `SpatRaster` / `SpatVector` / `sf`
3. `calculate_covariates(covariate, from, locs, locs_id, ...)` → `data.frame` / `SpatVector`
