---
title: "Live test failure on {{ date | date('YYYY-MM-DD') }}"
labels: ["live-test-failure", "automated"]
---
The scheduled live-test workflow failed.

- Workflow run: https://github.com/{{ env.GITHUB_REPOSITORY }}/actions/runs/{{ env.GITHUB_RUN_ID }}
- Triggered by: `{{ env.GITHUB_EVENT_NAME }}`
- Commit: `{{ env.GITHUB_SHA }}`

Please investigate the run logs above and update or close this issue once resolved.
