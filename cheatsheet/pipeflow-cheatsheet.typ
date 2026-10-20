// pipeflow cheat sheet: two pages, US Letter landscape.
// Build with cheatsheet/build.sh (needs Typst, tested with 0.15).
// R code is always written as raw text (`...`), so that ~, $, # and <-
// are not read as Typst markup.

#let version = "0.5.0" // pipeflow version the sheet is checked against
#let updated = "2026-10"

// colours: frame and interior of the logo, roles in code, and the step
// states of R/pipeflow-package.R (also used by pip_graph())
#let orange = rgb("#c2540d")
#let light = rgb("#f3f3f3")
#let c-input = rgb("#7b3fa0")
#let c-param = rgb("#2e7d32")
#let dim-grey = luma(115)
#let rule-grey = luma(185)
#let cell-grey = luma(222)
#let cell-hl = rgb("#f2b48c")
#let cell-alt = rgb("#c9b3dd")
#let state-fill = (
  new: rgb("#47b8ff"),
  done: rgb("#87ff6f"),
  outdated: rgb("#ffa040"),
  failed: rgb("#ff4c4c"),
)
#let run-fill = (
  ready: rgb("#87ff6f"),
  restart: rgb("#b48cff"),
  running: rgb("#47b8ff"),
  halted: rgb("#ffa040"),
  failed: rgb("#ff4c4c"),
  continued: rgb("#ffd84c"),
)
#let site = "https://rpahl.github.io/pipeflow/"
#let mono = "DejaVu Sans Mono"

// the README previews (--input preview=1) get a frame around the page
#let preview = sys.inputs.at("preview", default: "0") == "1"

#set document(
  title: "pipeflow cheat sheet",
  author: "Roman Pahl",
  date: none,
)
#set page(
  paper: "us-letter",
  flipped: true,
  margin: (x: 0.4in, top: 0.32in, bottom: 0.45in),
  background: if preview {
    rect(width: 100%, height: 100%, stroke: 2pt + luma(175))
  },
  footer: {
    set text(size: 6pt, fill: dim-grey)
    line(length: 100%, stroke: 0.4pt + rule-grey)
    v(-2pt)
    align(right)[
      Created by Roman Pahl • Learn more at
      #link(site)[rpahl.github.io/pipeflow] • pipeflow #version •
      Updated: #updated
    ]
  },
)
#set text(font: "DejaVu Sans", size: 7.2pt, fill: luma(25))
#set par(leading: 0.42em, spacing: 0.55em)
#set smartquote(enabled: false)
#set list(indent: 0pt, body-indent: 4pt, spacing: 0.55em)

// code: function calls bold, step references purple, comments grey
#let code-pat = "~\s*(?:try\(\s*-?\s*[\w.]+\s*\)|-?\s*[\w.]+)|[\w.$]+\(|#.*"
#show raw: set text(font: mono, size: 6.9pt)
#show raw: it => {
  show regex(code-pat): m => {
    if m.text.starts-with("~") { text(fill: c-input, m) } else if (
      m.text.starts-with("#")
    ) { text(fill: dim-grey, m) } else { text(weight: "bold", m) }
  }
  it
}
#show raw.where(block: true): set par(leading: 0.4em)

// ---------------------------------------------------------------------
// helpers

#let vignette-link(name, body) = link(site + "articles/" + name + ".html", body)

// section with a title that links to the matching vignette
#let section(title, vignette: none, body) = block(
  width: 100%,
  breakable: false,
  above: 20pt,
  below: 0pt,
  {
    block(
      width: 100%,
      above: 0pt,
      below: 9pt,
      stroke: (top: 0.6pt + rule-grey),
      inset: (top: 4pt),
      text(size: 13.5pt, fill: orange, if vignette == none { title } else {
        vignette-link(vignette, title)
      }),
    )
    body
  },
)

// small heading inside a section
#let sub(title) = block(
  width: 100%,
  above: 10pt,
  below: 6pt,
  stroke: (top: (paint: rule-grey, thickness: 0.5pt, dash: "dotted")),
  inset: (top: 3.5pt),
  text(size: 6.2pt, weight: "bold", fill: luma(70), upper(title)),
)

// code with its description below
#let fn(code, desc) = block(
  above: 10pt,
  below: 10pt,
  breakable: false,
  stack(spacing: 3pt, code, desc),
)

// pictogram on the left, code and description on the right
#let pfn(pic, code, desc) = block(
  above: 10pt,
  below: 10pt,
  breakable: false,
  grid(
    columns: (auto, 1fr),
    column-gutter: 9pt,
    align: (center + horizon, left + top),
    pic, stack(spacing: 3pt, code, desc),
  ),
)

#let greybox(body) = block(
  fill: light,
  inset: 7pt,
  width: 100%,
  breakable: false,
  body,
)

// a step (or a state): a box with the colour of the state
#let chip(state, label: none, w: auto, h: auto, fill: none) = box(
  width: w,
  height: h,
  fill: if fill == none { state-fill.at(state) } else { fill },
  stroke: 0.5pt + luma(90),
  radius: 2pt,
  inset: (x: 2.5pt, y: 1.3pt),
  baseline: 20%,
  align(center + horizon, text(size: 6.2pt, fill: black, if label == none {
    state
  } else { label })),
)

// a small table with one fill per cell, given row by row
#let cells(rows, w: 6.6pt, h: 4.3pt) = grid(
  columns: (w,) * rows.first().len(),
  rows: h,
  gutter: 0.7pt,
  ..rows.flatten().map(f => rect(width: w, height: h, fill: f, stroke: none)),
)

// a small table with one fill per row
#let minitab(fills, cols: 3) = cells(fills.map(f => (f,) * cols))

// arrow between two points (in pt), for use inside a box
#let arrow(x1, y1, x2, y2, dashed: false, col: luma(95)) = {
  let (dx, dy) = (x2 - x1, y2 - y1)
  let len = calc.sqrt(dx * dx + dy * dy)
  let (ux, uy) = (dx / len, dy / len)
  let (hl, hw) = (3.2, 1.6)
  let (bx, by) = (x2 - ux * hl, y2 - uy * hl)
  place(line(
    start: (x1 * 1pt, y1 * 1pt),
    end: (bx * 1pt, by * 1pt),
    stroke: (
      paint: col,
      thickness: 0.6pt,
      dash: if dashed { "densely-dashed" } else { none },
    ),
  ))
  place(polygon(
    fill: col,
    stroke: none,
    (x2 * 1pt, y2 * 1pt),
    ((bx - uy * hw) * 1pt, (by + ux * hw) * 1pt),
    ((bx + uy * hw) * 1pt, (by - ux * hw) * 1pt),
  ))
}

// a small dependency graph; nodes are dictionaries with id, x, y, state,
// label and w (all positions in pt), edges are (from, to, dashed)
#let node-h = 9
#let dag(w, h, nodes, edges, notes: ()) = box(width: w * 1pt, height: h * 1pt, {
  let pos = (:)
  for n in nodes { pos.insert(n.id, n) }
  for e in edges {
    let (a, b) = (pos.at(e.at(0)), pos.at(e.at(1)))
    arrow(
      a.x + a.w + 0.6,
      a.y + node-h / 2,
      b.x - 0.6,
      b.y + node-h / 2,
      dashed: e.len() > 2 and e.at(2),
    )
  }
  for n in nodes {
    place(dx: n.x * 1pt, dy: n.y * 1pt, chip(
      n.state,
      label: n.label,
      w: n.w * 1pt,
      h: node-h * 1pt,
    ))
  }
  for (x, y, body) in notes {
    place(dx: x * 1pt, dy: y * 1pt, text(size: 6pt, fill: dim-grey, body))
  }
})
#let nd(id, x, y, state, w: 26, label: none) = (
  id: id,
  x: x,
  y: y,
  state: state,
  w: w,
  label: if label == none { id } else { label },
)

#let arrow-txt = text(fill: dim-grey)[→]

// ---------------------------------------------------------------------
// pictograms

// the step table as print(p) shows it after pip_run(p)
#let step-table = {
  set text(font: mono, size: 6.4pt)
  table(
    columns: 5,
    stroke: none,
    inset: (x: 2.6pt, y: 1.5pt),
    align: (x, y) => (right, right, right, center, right).at(x) + horizon,
    table.header(..([step], [params], [depends], [state], [out]).map(h => text(
      weight: "bold",
      h,
    ))),
    table.hline(stroke: 0.4pt + luma(160)),
    [numbers], [n], [], chip("done"), [1,2,3,4,5],
    [squared], [x,k], [numbers], chip("done"), [1, 4, 9,16,25],
    [total], [x], [squared], chip("done"), [55],
  )
}

// a view: rows of the step table
#let views-pic = grid(
  columns: 3,
  column-gutter: 3pt,
  align: horizon,
  minitab((cell-grey, cell-hl, cell-hl, cell-grey)),
  arrow-txt,
  minitab((cell-hl, cell-hl)),
)

// p[state == "outdated"]: rows picked by their value in the state column
#let expr-pic = {
  let (d, o) = (state-fill.done, state-fill.outdated)
  grid(
    columns: 3,
    column-gutter: 3pt,
    align: horizon,
    cells((
      (d, cell-grey, cell-grey),
      (o, cell-hl, cell-hl),
      (d, cell-grey, cell-grey),
      (o, cell-hl, cell-hl),
    )),
    arrow-txt,
    cells(((o, cell-hl, cell-hl),) * 2),
  )
}

// rbind(): the steps of two pipelines in one
#let bind-pic = grid(
  columns: 5,
  column-gutter: 3pt,
  align: horizon,
  minitab((cell-hl, cell-hl)),
  text(fill: dim-grey)[+],
  minitab((cell-alt, cell-alt)),
  text(fill: dim-grey)[=],
  minitab((cell-hl, cell-hl, cell-alt, cell-alt)),
)

// q <- pip_clone(p): two independent pipelines
#let clone-pic = grid(
  columns: 3,
  column-gutter: 3pt,
  row-gutter: 3pt,
  align: horizon,
  raw("p"), arrow-txt, minitab((cell-hl, cell-hl)),
  raw("q"), arrow-txt, minitab((cell-alt, cell-alt)),
)

// after rbind(p1, p2): connect the parts with pip_replace(p, "data2", ...)
#let connect-pic = dag(
  178,
  30,
  (
    nd("data", 0, 6, "done"),
    nd("prep", 44, 6, "done"),
    nd("data2", 100, 6, "new", w: 28),
    nd("model", 148, 6, "outdated", w: 30),
  ),
  (("data", "prep"), ("prep", "data2", true), ("data2", "model")),
  notes: (
    (29, 16, [p1]),
    (130, 16, [p2]),
  ),
)

// states after pip_replace(p, "squared", ...)
#let modify-pic = dag(
  150,
  19,
  (
    nd("numbers", 0, 8, "done", w: 34),
    nd("squared", 56, 8, "new", w: 34),
    nd("total", 112, 8, "outdated", w: 34),
  ),
  (("numbers", "squared"), ("squared", "total")),
  notes: ((55, -2, [replaced / new]), (111, -2, [outdated])),
)

// states after pip_set_params(p, list(k = 3))
#let params-pic = dag(
  150,
  19,
  (
    nd("numbers", 0, 10, "done", w: 34),
    nd("squared", 56, 10, "outdated", w: 34),
    nd("total", 112, 10, "outdated", w: 34),
  ),
  (("numbers", "squared"), ("squared", "total")),
  notes: ((62, -1, text(fill: c-param, raw("k = 3"))),),
)

// pip_graph() as drawn by visNetwork: a hexagon per step, coloured by state
#let hexagon(x, y, r, fill) = place(polygon(
  fill: fill,
  stroke: 0.5pt + luma(90),
  ..range(6).map(i => {
    let a = 30deg + 60deg * i
    ((x + r * calc.cos(a)) * 1pt, (y + r * calc.sin(a)) * 1pt)
  }),
))
#let graph-pic = box(width: 86pt, height: 21pt, {
  let steps = (
    (12, "numbers", "done"),
    (43, "squared", "outdated"),
    (74, "total", "outdated"),
  )
  for i in range(steps.len() - 1) {
    arrow(steps.at(i).at(0) + 5.5, 6, steps.at(i + 1).at(0) - 5.5, 6)
  }
  for (x, label, state) in steps {
    hexagon(x, 6, 5.5, state-fill.at(state))
    place(dx: (x - 15) * 1pt, dy: 13.5pt, box(width: 30pt, align(
      center,
      text(size: 5.4pt, fill: dim-grey, label),
    )))
  }
})

// .self$halt() in step check: the run stops after it
#let halt-pic = box(width: 176pt, height: 22pt, {
  place(dag(
    176,
    22,
    (
      nd("data", 0, 13, "done"),
      nd("check", 46, 13, "done"),
      nd("model", 104, 13, "new"),
      nd("report", 146, 13, "new", w: 30),
    ),
    (("data", "check"), ("check", "model"), ("model", "report")),
    notes: ((34, 0, raw(".self$halt()")),),
  ))
  place(dx: 88pt, dy: 10pt, line(
    end: (0pt, 15pt),
    stroke: 1.8pt + run-fill.halted,
  ))
})

// .self$add() and .self$restart() in step check: the run starts again
#let restart-pic = box(width: 140pt, height: 30pt, {
  let col = luma(95)
  arrow(26.6, 24.5, 45.4, 24.5)
  arrow(72.6, 24.5, 101.4, 24.5, dashed: true)
  place(curve(
    stroke: 0.6pt + col,
    curve.move((59pt, 19.5pt)),
    curve.cubic((59pt, 5pt), (13pt, 5pt), (13pt, 16.5pt)),
  ))
  place(polygon(fill: col, (13pt, 19.6pt), (11.4pt, 16.3pt), (14.6pt, 16.3pt)))
  set text(fill: dim-grey)
  place(dx: 15pt, dy: 0pt, raw("$restart()"))
  place(dx: 104pt, dy: 10.5pt, raw("$add()"))
  set text(fill: luma(25))
  place(dx: 0pt, dy: 20pt, chip("done", label: "data", w: 26pt, h: 9pt))
  place(dx: 46pt, dy: 20pt, chip("done", label: "check", w: 26pt, h: 9pt))
  place(dx: 102pt, dy: 20pt, box(
    width: 28pt,
    height: 9pt,
    fill: state-fill.new,
    stroke: (paint: luma(90), thickness: 0.5pt, dash: "densely-dashed"),
    radius: 2pt,
    align(center + horizon, text(size: 6.2pt, fill: black)[extra]),
  ))
})

// a step that runs a copy of another pipeline
#let nested-pic = box(width: 176pt, height: 28pt, {
  place(dx: 46pt, dy: 0pt, rect(
    width: 82pt,
    height: 28pt,
    radius: 2pt,
    fill: light,
    stroke: 0.5pt + luma(90),
  ))
  place(dx: 49pt, dy: 2.5pt, text(size: 6.2pt)[fit])
  place(dx: 85pt, dy: 2.5pt, text(size: 6pt, fill: dim-grey)[copy of inner])
  arrow(26.6, 15.5, 45.4, 15.5)
  arrow(128.6, 15.5, 145.4, 15.5)
  arrow(77.6, 19, 96.4, 19)
  for (x, y, label, w) in (
    (0, 11, "prep", 26),
    (52, 14.5, "data", 25),
    (97, 14.5, "coefs", 26),
    (146, 11, "report", 30),
  ) {
    place(dx: x * 1pt, dy: y * 1pt, chip(
      "done",
      label: label,
      w: w * 1pt,
      h: node-h * 1pt,
    ))
  }
})

// split · map · reduce with two partitions
#let smr-pic = dag(
  190,
  42,
  (
    nd("data", 0, 15, "done"),
    nd("a1", 52, 9, "done", label: "a"),
    nd("b1", 52, 21, "done", label: "b"),
    nd("a2", 106, 9, "done", label: "a"),
    nd("b2", 106, 21, "done", label: "b"),
    nd("all", 160, 15, "done"),
  ),
  (
    ("data", "a1"),
    ("data", "b1"),
    ("a1", "a2"),
    ("b1", "b2"),
    ("a2", "all"),
    ("b2", "all"),
  ),
  notes: (
    (52, 0, [by_group]),
    (106, 0, [means]),
    (55, 33, [split]),
    (109, 33, [auto]),
    (162, 33, [reduce]),
  ),
)

// on_error = "continue": a failed step, a skipped one, a ~try() step
#let fail-pic = dag(
  200,
  28,
  (
    nd("data", 0, 9, "done"),
    nd("model", 50, 9, "failed"),
    nd("report", 104, 0, "new"),
    nd("safe", 104, 18, "done"),
  ),
  (("data", "model"), ("model", "report"), ("model", "safe", true)),
  notes: (
    (134, 1.5, [skipped]),
    (134, 19.5, text(fill: c-input, raw("~try(model)"))),
  ),
)

// the anatomy of a step in the Basics box
#let anatomy = box(align(left, text(font: mono, size: 10pt)[
  *pip\_add(*p, #text(fill: orange)["squared"],\
  #h(4 * 0.602em)\\(#text(fill: c-input)[x = \~numbers],
  #text(fill: c-param)[k = 2]) x^k*)*
]))

#let logo = image("/logo/logo.svg", height: 0.74in, alt: "pipeflow hex logo")

// ---------------------------------------------------------------------
// page 1: basics, build, run, collect, views, select, modify, combine

#place(top + right, dy: -0.2in, logo)
#block(height: 0.5in, width: 100%, align(left, text(
  size: 21pt,
  fill: luma(45),
)[Data analysis pipelines with pipeflow : : *CHEAT SHEET*]))

#grid(
  columns: (1fr, 1fr, 1fr),
  column-gutter: 16pt,
  [
    #greybox[
      #text(size: 13.5pt, fill: orange, vignette-link(
        "v01-get-started",
        [Basics],
      ))
      #v(1pt)
      pipeflow builds data analysis pipelines from plain R functions.
      Each function becomes a *step*, and function parameters can be
      connected to other steps:

      #v(8pt)
      #align(center, anatomy)
      #v(8pt)

      #align(center)[
        Add step #text(fill: orange)[*squared*] to pipeline `p`. Input parameter
        #text(fill: c-input)[*x*] is the output of step
        #text(fill: c-input)[*numbers*], and
        #text(fill: c-param)[*k*] is a normal parameter.
      ]

      #v(6pt)
      ```
      p <- pip_new("demo") |>
         pip_add("numbers", \(n = 5) seq_len(n)) |>
         pip_add("squared", \(x = ~numbers, k = 2) x^k) |>
         pip_add("total", \(x = ~squared) sum(x))

      pip_run(p)        # run all steps
      pip_collect(p)    # collect step outputs
      ```

      #v(8pt)
      `print(p)` shows the pipeline as a table, one row per step:
      #v(1pt)
      #align(center, step-table)
    ]

    #section("Build a pipeline", vignette: "v01-get-started")[
      #fn[`pip_new("demo")`][create an empty pipeline.]
      #fn[`pip_add(p, "numbers", \(n = 5) seq_len(n))`][
        add a step at the end.
      ]
      #fn[`pip_add(p, "fit", fun, tags = c("model", "core"))`][
        tag steps to select or collect them later.
      ]
    ]

    #section("Parameters", vignette: "v01-get-started")[
      Arguments that are not inputs are parameters.
      #fn[`pip_get_params(p)`][all parameters as a named list.]
      #v(2pt)
      #fn[`pip_set_params(p, list(k = 3))`][
        set a parameter in all steps that have it. These steps and the
        steps that depend on them become outdated.
      ]
      #align(center, params-pic)
    ]
  ],
  [
    #section("Run", vignette: "v01-get-started")[
      Each step has a state:
      #grid(
        columns: (auto, 1fr),
        column-gutter: 6pt,
        row-gutter: 4.5pt,
        align: (left + horizon, left + horizon),
        chip("new"), [not run yet],
        chip("done"), [up to date, skipped by the next run],
        chip("outdated"), [an input or a parameter changed],
        chip("failed"), [the last run raised an error],
      )
      #fn[`pip_run(p)`][
        run all steps where state is not yet done.
      ]
      #fn[`pip_run(p, force = TRUE)`][
        force re-run of all steps regardless of state.
      ]
      #fn[`pip_run(p["total"])`][
        run selected step(s), plus the upstream steps they need.
      ]
      #fn[`pip_run(p, lgr = NULL)`][run without log messages.]
    ]

    #section("Collect and inspect", vignette: "v04-collect-output")[
      #fn[`pip_collect(p)`][the outputs of all steps as a named list.]
      #fn[`pip_collect(p, by = "tags")`][grouped by tag.]
      #fn[`p[, c("step", "out")]`][output in table format.]
      #fn[`p[["total", "out"]]`][output of one step.]
    ]

    #section("Views", vignette: "v03b-pipeline-views")[
      Views are a window onto the pipeline by reference (no copy);
      most `pip_*()` functions work with views.
      #fn[`v <- pip_view(p, tags = "core", state = "done")`][
        filter by certain columns of the step table.
      ]

      #fn[`pip_set_params(v, list(k = 3))`][
        set parameters for all steps of the view.
      ]
      #fn[`pip_run(v)`][
        run the steps of the view, plus the upstream steps they need.
      ]
      #fn[`pip_collect(v)`][
        collect from the filtered view.
      ]
    ]
  ],
  [
    #section("Select steps with [ ]", vignette: "v03b-pipeline-views")[
      Creates views via subsetting.

      #pfn(views-pic)[`v <- p[2:3]`, `p[c("squared", "total")]`][
        by position or name: the result is a view.
      ]
      #pfn(expr-pic)[`v <- p[state == "outdated"]`][
        by an expression on the step table, as in data.table.
      ]
      #sub[Operators from data.table]
      #grid(
        columns: 4,
        column-gutter: 1fr,
        `%like%`, `%ilike%`, `%notin%`, `%between%`,
      )
      #v(4pt)
      For example, `p[step %like% "^sq"]` selects step squared.
    ]

    #section("Modify steps", vignette: "v02-modify-pipeline")[
      Changes are checked; affected steps run again:
      #v(2pt)
      #fn[`pip_replace(p, "squared", \(x = ~numbers) x^3)`][
        a new function for a step: the step becomes new, its downstream
        steps outdated.
      ]
      #align(center, modify-pic)
      #fn[`pip_rename(p, "total", "sum")`][
        rename a step and all references to it.
      ]
      #fn[`pip_remove(p, "sum")`][
        remove a step (`force = TRUE`: also the steps that use it).
      ]
      #fn[`pip_lock(p["numbers"])`, `pip_unlock(p)`][
        locked steps freeze their parameters and output: runs skip them.
      ]
    ]

    #section("Combine pipelines", vignette: "v03a-combine-pipelines")[
      #pfn(bind-pic)[`p <- rbind(p1, p2)`][
        a new pipeline with the steps of both. Clashing step names get a
        suffix: `data` becomes `data2`.
      ]
      #align(center, connect-pic)
      #fn[`pip_replace(p, "data2", \(x = ~prep) x)`][
        connects data2 with prep manually after the bind.
      ]
      #v(2pt)
      To connect the parts in one go, use:
      #v(-4pt)
      #fn[`p <- rbind(p1, p2, bind = list(data = ~prep))`][
      ]
    ]

  ],
)

#pagebreak()

// ---------------------------------------------------------------------
// page 2: connect, split/map/reduce, nest, copy, failures, self-modify

#place(top + right, dy: -0.2in, logo)
#block(height: 0.5in, width: 100%, align(left, text(
  size: 21pt,
  fill: luma(45),
)[Advanced features and workflows]))

#grid(
  columns: (1fr, 1fr, 1fr),
  column-gutter: 16pt,
  [
    #section("Ways to connect steps", vignette: "v01-get-started")[
      #fn[`\(x = ~numbers)`][`x` is the output of step `numbers`.]
      #fn[`\(x = ~-1)`][
        the output of the previous step (`~-2`: two steps back).
      ]
      #fn[`\(x = ~numbers, y = ~squared)`][several inputs.]
      #fn[`\(x = ~try(model))`][
        mitigate potential failure.
      ]
      #fn[`pip_set_params(p, list(x = ~numbers))`][
        bulk-connect parameter after pipeline creation.
      ]
    ]

    #section("Split · map · reduce", vignette: "v05a-split-map-reduce")[
      #align(center, smr-pic)
      #fn[
        ```
        pip_add(p, "by_group", \(d = ~data) split(d$x, d$g),
                exec = "split")
        ```
      ][`"split"`: the output is a named list of partitions.]
      #fn[`pip_add(p, "means", \(x = ~by_group) mean(x))`][
        `"auto"` (the default): runs once per partition.
      ]
      #fn[
        ```
        pip_add(p, "all", \(m = ~means) unlist(m),
                exec = "reduce")
        ```
      ][`"reduce"`: one call that gets all partitions as a named list.]
    ]

    #section("Nested pipelines", vignette: "v05b-nested-pipeline")[
      A step can run a copy of another pipeline:
      #v(2pt)
      #align(center, nested-pic)
      #fn[`f <- as.function(inner, out = "coefs")`][
        a function that runs a copy of `inner`. Its arguments are the unbound
        params of `inner`; `out` is the step output it returns.
      ]
      #fn[
        ```
        pip_add(outer, "fit",
                as.function(inner, out = "coefs",
                            bind = list(data = ~prep)))
        ```
      ][
        as a step: `bind` connects the param `data` to step `prep`. The other
        unbound params of `inner` become params of `fit`.
      ]
    ]

  ],
  [
    #section("Copy and save", vignette: "v03a-combine-pipelines")[
      #pfn(clone-pic)[`q <- pip_clone(p)`][
        an independent copy. After `q <- p`, both names refer to the same
        pipeline.
      ]
      #fn[`saveRDS(p, "p.rds")`, `p <- readRDS("p.rds")`][
        save and restore a pipeline with its outputs and states.
      ]
    ]

    #section("Handle failures", vignette: "v05c-error-handling")[
      #align(center, fail-pic)
      #fn[`pip_run(p, on_error = "continue")`][
        by default, a run stops at the first error. With `"continue"`,
        a failing step gets state failed, the steps that use it are
        skipped, and all others run.
      ]
      #fn[
        ```
        pip_add(p, "safe", \(m = ~try(model)) {
            if (inherits(m, "pipeflow_failure")) NA else m
        })
        ```
      ][
        `~try()`: run even if `model` failed; `m` is then a
        `pipeflow_failure`.
      ]
      #fn[`pip_run(p)`][
        once fixed, the next run picks up the failed and skipped steps.
      ]
      #sub[Run state]
      `pip_run_state(p)` tells how the last run ended:
      #v(2pt)
      #grid(
        columns: (auto, 1fr),
        column-gutter: 6pt,
        row-gutter: 4.5pt,
        align: (left + horizon, left + horizon),
        chip("ready", fill: run-fill.ready), [without failures],
        chip("failed", fill: run-fill.failed), [stopped at an error],
        chip("continued", fill: run-fill.continued),
        [went on after failures],
        chip("halted", fill: run-fill.halted), [a step called `.self$halt()`],
      )
    ]

    #v(20pt)
    #greybox[
      #text(size: 9pt, fill: orange)[Good to know]
      #v(2pt)
      - `pip_*()` functions change the pipeline in place and return it
        invisibly, so they can be chained with `|>`.
      - `p$add(...)` is short for `pip_add(p, ...)`; likewise `$run()`,
        `$set_params()`, `$collect()`, ...
      - Every argument of a step function needs a default. Primitives
        like `sum` have none: wrap them, `\(x = ~-1) sum(x)`.
    ]
  ],
  [
    #section("Self-modifying pipelines", vignette: "v06-self-modify-pipeline")[
      In a step function, `.self` is the running pipeline.
      #v(4pt)
      #align(center, restart-pic)
      #fn[`.self$add()`, `$replace()`, `$remove()`, ...][
        change the pipeline during a run.
      ]
      #fn[`.self$restart()`][
        when the step is done, run again with the new structure.
      ]
      #v(4pt)
      #align(center, halt-pic)
      #fn[
        ```
        pip_add(p, "check", \(x = ~data) {
            if (length(x) == 0) .self$halt()
            x
        })
        ```
      ][
        `.self$halt()`: stop the run after this step; the next
        `pip_run()` continues.
      ]
    ]

    #section("Non-trivial parameter values", vignette: "v05d-param-value")[
      pipeflow supports non-trivial parameter values.
      #fn[
        ```
        my_param <- structure(
            list(value = 1.23, label = "My label"),
            class = "foo"
        )
        p <- pip_new("demo") |>
            pip_add("step", \(x = my_param) x)
        ```
      ][]
      #fn[
        ```
        # getter for class foo
        pip_param_value.foo <- function(x, ...) x$value

        # setter for class foo
        `pip_param_value<-.foo` <- function(x, ..., value) {
            if (inherits(value, "foo")) return(value)
            x$value <- value
            x
        }
        ```
      ][
        register getter and setter methods.
      ]

      #v(4pt)
      #fn[`pip_run(p)`][the step function receives `1.23`, not the `foo` object.]
      #fn[`pip_get_params(p)`][returns the stored `foo` object.]
      #fn[`pip_set_params(p, list(x = 99))`][
        sets the value to `99`; the label is kept.
      ]
    ]
  ],
)
