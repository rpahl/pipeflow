# Row-filter operators re-exported from data.table

pipeflow re-exports the row-filter operators of data.table so that they
are available after attaching pipeflow and can be used in boolean
filters passed to `[.pipeflow`, e.g. `p[tags %like% "daily"]`. They
behave exactly as in data.table; see its documentation for details.
