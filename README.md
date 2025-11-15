# elm-ml

A small machine-learning playground written in Elm 0.19. The project contains implementations of simple ML algorithms (KNN, KMeans, Naive Bayes, Regression) together with small demo pages and basic metrics.


## Project layout

- `src/` — Elm source files
  - `Main.elm` top-level demo app
  - `MainSvgDemo.elm` interactive SVG demos (index.html loads this)
  - `KNN.elm`, `KMeans.elm`, `NaiveBayes.elm`, `Regression.elm`, `Vector.elm`, `Dataset.elm`, `Metrics.elm` core ML modules
  - `DemoKNN.elm`, `DemoKMeans.elm`, `DemoNaiveBayes.elm`, `DemoRegression.elm` small demo wrappers
- `tests/` elm-test unit tests for the algorithems. in process
- `index.html` demo page that loads `MainSvgDemo`
- `elm.json` Elm project configuration

## Requirements

- Elm 0.19.1

## Build and run the demos

You can build the demo JavaScript with `elm make`. From the project root:

```powershell
# Build the main demo used by index.html
elm make src/MainSvgDemo.elm --output=elm.js
# or build the simpler main
elm make src/Main.elm --output=elm.js
```

Then open `index.html` in your browser. A quick way to serve the folder (so relative imports work) is to use a simple HTTP server:

```powershell
# Windows PowerShell: start a simple web server using Python (if installed)
python -m http.server 8000
# then open http://localhost:8000 in your browser
```

Or use `elm reactor` for quick exploration:

```powershell
elm reactor
# open http://localhost:8000 and click the modules
```

## Contributing

Contributions are welcome. Please open a branch or a PR with changes and include tests for new behavior.
