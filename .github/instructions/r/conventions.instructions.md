---
name: 'R Conventions'
description: 'Code and naming conventions for R files'
applyTo: '**/*.R'
---

# R Code and Naming Conventions

- Use native pipe `|>`, line length ≤80 and indent of 4 spaces.
- Document all functions with roxygen2 headers. The header should end
  with an `@noRd` tag unless they are `@export`ed.
- Maintain consistent `mod_`/`utils_`/`app_` file naming.
