# Pipelines as functions (`R/as_function.R`)

`as.function(p)` turns a pipeline into an ordinary R function. You can call
that function directly, or add it as a step to an "outer" pipeline with
`pip_add()`. The function keeps the inner pipelines it has run, one per
outer step and partition key, so a later call only runs the inner steps
that changed params or inputs have outdated. `pip_inner()` lets you look at
these kept pipelines.

This file builds on [pipeflow.md](pipeflow.md). Read that first for the
data model, `.step_call` and the run loop.

## 1. Function map

| Function | Role |
|---|---|
| `as.function.pipeflow(x, out, cache, on_error, lgr)` | Entry point. Validates the arguments, takes a reset snapshot (`template`), builds the formals from `pip_get_params()`, and assembles the function. |
| `.assert_fun_out()`, `.assert_fun_cache()` | Validate `out` (NULL, step names, or a function with ≥ 1 argument) and `cache`. |
| `.fun_quote(args)` | Wraps symbol and call defaults in `quote()`, so they are not evaluated a second time. |
| `.fun_body(runner, formalNames)` | Builds the body `runner(list(a = a, ...))`. The runner and `base::list` are inlined as objects, so a formal named `list` cannot shadow them. |
| `.fun_runner(ctx)` | Makes the closure that holds `ctx` and the `cache` env (class `pipeflow_fun_cache`, with `slots` and `active`). |
| `.fun_call(args, ctx, cache)` | One call: find the pipeline, set the params, run it, check the result, extract the output. |
| `.fun_pip(cache, stepCall, ctx)` | Picks (or makes) the slot pipeline for the current `step` and `key`. |
| `.fun_slots()` | New slot set of a step: `pips` (hashed env key → pipeline), `keys`, `mapped`. |
| `.fun_keep_keys(slots, keys, mapped)` | Drops the pipelines whose keys are gone. Runs once per outer step run, because the `keys` object is `identical()` for all partitions of that run. |
| `.fun_slot_pips(slots)` | The kept pipelines in key order. |
| `.fun_cache(fun)` | Recovers the cache env from a function made by `as.function()`, or returns `NULL`. |
| `.fun_release(fun, step)` | Drops a step's slots when it is mapped over zero keys. |
| `.fun_rename(fun, from, to)` | Moves a step's slots when the outer step is renamed. |
| `.fun_check_run(pip, out)` | With `on_error = "continue"`: raises the error if a requested step failed or was blocked. |
| `.fun_failure_causes(dat)` | Follows failures along the run (honouring `~try()` and `locked`) and returns step → failing step. |
| `.fun_out(pip, out)` | Builds the result: `pip_collect()`, one or more step outputs, or `out(snapshot)`. |
| `.fun_snapshot(pip)` | A light copy (`.pip_clone(deep = FALSE)`) that keeps the run state and last-run time. |
| `.fun_pips(fun)` | All kept pipelines of a function. Only the tests use it. |
| `pip_inner(x, step)` | Exported. Snapshots of the kept pipelines of an outer step. |

## 2. Construction

```mermaid
flowchart TD
    AF["as.function.pipeflow(x, out, cache, on_error, lgr)"]
    AF --> AP[".assert_pip(x)<br/>(no views)"]
    AF --> AO[".assert_fun_out / .assert_fun_cache / .assert_logger"]
    AF --> T["template = pip_clone(x)<br/>pip_reset(template)"]
    T --> GP["pip_get_params(template)"]
    GP --> Q[".fun_quote → formals"]
    AF --> CTX["ctx = list(template, out, onError, lgr, keep = cache)"]
    CTX --> RUN[".fun_runner(ctx)<br/>cache env: slots = list(), active = list()"]
    RUN --> B[".fun_body(runner, names(args))"]
    Q --> FN
    B --> FN["as.function(c(args, body),<br/>envir = new.env(parent = baseenv()))"]
```

The function this produces looks like this:

```mermaid
classDiagram
    class f_inner {
        <<function>>
        formals : unbound params with defaults
        body : calls runner with all formals
    }
    class runner {
        <<closure>>
        ctx : template, out, onError, lgr, keep
        cache : pipeflow_fun_cache
    }
    class cache {
        <<env pipeflow_fun_cache>>
        slots : list, step to slots env
        active : list of running pipenvs
    }
    class slots {
        <<env, one per outer step>>
        pips : hashed env, key to pipeline
        keys : chr, keys of the last call
        mapped : lgl
    }
    f_inner --> runner : inlined in body
    runner --> cache
    cache --> slots : slots[[step]]
```

The cache belongs to the function and not to a global registry. Dropping
the function drops its pipelines. Serialising the function copies them,
which is cheap when the function was made with `cache = FALSE` or has not
been called yet.

## 3. One call

```mermaid
sequenceDiagram
    participant O as outer pip_run / .pip_execute_step_call
    participant CS as .pip_call_step
    participant F as f_inner (body)
    participant C as .fun_call
    participant P as .fun_pip
    participant I as inner pipeline
    participant K as .fun_check_run / .fun_out

    O->>CS: step, key, keys (sets .step_call)
    CS->>F: do.call(f_inner, args)
    F->>C: runner(list(a = a, ...))
    C->>C: stepCall = .pip_step_call()  (read BEFORE the inner run)
    C->>P: .fun_pip(cache, stepCall, ctx)
    alt cache = FALSE
        P-->>C: pip_clone(template)
    else
        P->>P: step = stepCall$step or "<direct>"
        P->>P: key/keys = stepCall or "<single>" (mapped = !is.null(key))
        P->>P: slots[[step]] (create via .fun_slots)
        P->>P: .fun_keep_keys(slots, keys, mapped)
        P->>P: pips[[key]] or assign pip_clone(template)
        P->>P: in cache$active? then fresh pip_clone (recursion)
        P-->>C: pip
    end
    C->>C: push pipenv to cache$active (popped on.exit)
    C->>I: .pip_set_params(pip, args, unknown = "ignore", bind = FALSE)
    Note over I: unchanged values are skipped,<br/>so only changed params outdate steps
    C->>I: pip_run(pip, lgr = ctx$lgr, on_error = ctx$onError)
    I->>CS: nested .pip_call_step (restores outer .step_call on exit)
    C->>K: .fun_check_run(pip, out)
    C->>K: .fun_out(pip, out)
    K-->>O: result
```

Details that are easy to break:

- **`bind = FALSE`.** The function's arguments are values. A formula
  argument is stored as a value and does not become a reference
  (`.pip_set_params(bind = FALSE)`). This is the only caller with
  `bind = FALSE`. Because of it, an inner pipeline can hold an unbound param
  whose value is a formula, and `.pip_copy_from()` has special handling for
  that case.
- **Read `.step_call` first.** `.fun_call()` reads the step call before
  `pip_run()`, because the inner run's own `.pip_call_step()` calls
  overwrite it until they return.
- **Re-entrancy.** `cache$active` holds the `pipenv`s that are currently
  running. A recursive call that would get a slot that is already running
  gets a throwaway clone instead, so it does not change the params of the
  running pipeline.
- **Failure keeps progress.** The slot is stored before the run, so after an
  inner error the steps that already ran are reused on the next call.
- **Placeholder keys.** `.fun_key_single` (`"<single>"`) is used for calls
  that are not mapped, and `.fun_step_direct` (`"<direct>"`) for calls made
  outside a step. If one of them clashes with a real name, calls share a
  pipeline. That is slower but still correct.

## 4. Slots across outer runs

```mermaid
flowchart LR
    subgraph outer["outer pipeline"]
        by_cyl["by_cyl<br/>exec = split"] --> coefs["coefs<br/>fun = f_inner (auto)"]
        df["df"] --> coefs2["coefs2<br/>fun = f_inner"]
    end
    subgraph cache["f_inner cache$slots"]
        s1["slots$coefs<br/>mapped = TRUE<br/>pips: '4', '6', '8'"]
        s2["slots$coefs2<br/>mapped = FALSE<br/>pips: single"]
        s3["slots for direct calls<br/>pips: single"]
    end
    coefs -. "key per partition" .-> s1
    coefs2 -. "single call" .-> s2
    user(["f_inner(...) at the console"]) -.-> s3
```

How slots change with the outer pipeline:

| Outer event | Effect on slots | Done by |
|---|---|---|
| Partition keys change | Pipelines of keys that are gone are removed | `.fun_keep_keys()` (first call of the run) |
| Step switches between mapped and single | All pipelines of the other mode are dropped | `.fun_keep_keys()` (`mapped` changed) |
| Mapped over zero keys | No call happens, so the slots are dropped explicitly | `.pip_execute_step_call()` → `.fun_release()` |
| Outer step renamed | `slots[[from]]` moves to `slots[[to]]` | `pip_rename()` → `.fun_rename()` |
| Outer step removed or replaced | The slots stay until the function is garbage-collected | — |
| `pip_run(force = TRUE)` on outer | Inner pipelines are **not** forced | — (use `cache = FALSE`) |

If the same function is used by two outer pipelines with the same step name,
both use the same slot. Results stay correct, but caching does not help.

## 5. Output and errors

```mermaid
flowchart TD
    R["after inner pip_run"] --> CR{".fun_check_run<br/>any failed unlocked step?"}
    CR -- no --> OUT
    CR -- yes --> FC[".fun_failure_causes(dat)<br/>step → failing step,<br/>skipping ~try() args and locked steps"]
    FC --> AFF{"requested steps affected?<br/>(out = NULL or a function: all steps)"}
    AFF -- no --> OUT
    AFF -- yes --> ERR["stop(.pip_step_error(cause,<br/>parent = pip[[cause, 'out']]))"]
    OUT{".fun_out(out)"} -- NULL --> COL["pip_collect(pip)"]
    OUT -- "chr (1)" --> ONE["pip[[s, 'out']]"]
    OUT -- "chr (n)" --> MANY["named list of outputs"]
    OUT -- function --> SNAP["out(.fun_snapshot(pip))"]
```

- With `on_error = "stop"`, the inner `pip_run()` raises a
  `pipeflow_step_error`. In the outer run it becomes the step's error, and
  for a mapped step it is wrapped once more in a `pipeflow_key_error`.
- `.fun_failure_causes()` must follow failures the same way as the
  `failures` bookkeeping in `pip_run()`. If you change which steps
  `pip_run()` skips or runs with a failure, change both.
- `.fun_snapshot()` and `pip_inner()` return light copies. They have their
  own step table and DAG, but share the output objects, so modifying an
  output in place (e.g. data.table `:=`) also changes the kept pipeline.

## 6. Coupling with the core

```mermaid
flowchart LR
    subgraph core["R/pipeflow.R"]
        pcs[".pip_call_step / .pip_step_call<br/>(.step_call env)"]
        pesc[".pip_execute_step_call"]
        pren["pip_rename"]
        psp[".pip_set_params(bind = FALSE)"]
        prun["pip_run"]
        pclone["pip_clone / .pip_clone(deep = FALSE)"]
        pcol["pip_collect"]
        pse[".pip_step_error"]
        fta["formula_try_args (utils.R)"]
    end
    subgraph gen["R/generic.pipeflow.R"]
        pcf[".pip_copy_from<br/>(keeps formula values)"]
    end
    subgraph asf["R/as_function.R"]
        fcall[".fun_call"]; fpip[".fun_pip"]; frel[".fun_release"]
        fren[".fun_rename"]; fchk[".fun_check_run"]; fout[".fun_out"]
        fsnap[".fun_snapshot"]; inner["pip_inner"]
    end
    fcall --> pcs & psp & prun
    fpip --> pclone
    fsnap --> pclone
    fout --> pcol
    fchk --> pse
    fchk --> fta
    pesc --> frel
    pren --> fren
    inner --> fsnap
    pcf -. "handles state created by" .-> psp
```

If you change any of the core functions above, check `as_function.R` too,
and run `tests/testthat/test_as_function.R`. That file defines its
fixtures inline and not in a `helper_*.R` file. Closures from a helper
would have the attached package environment as an ancestor, and under
`load_all()` they cannot be serialised without a warning.
