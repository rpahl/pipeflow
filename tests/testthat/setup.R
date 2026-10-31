# Many tests give params with the same name different values on purpose, to tell the
# steps apart. The tests of the warning about this turn it on again locally.
local({
    old <- options(pipeflow.warn_param_conflicts = FALSE)
    withr::defer(options(old), teardown_env())
})
