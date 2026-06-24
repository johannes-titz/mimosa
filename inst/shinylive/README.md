# mimosa on webR

This directory contains the app entry point used to build a Shinylive version of
mimosa. Shinylive runs Shiny for R in the browser with webR, so the exported app
can be hosted as static files without Shiny Server.

Build the static site from the repository root:

```r
install.packages("shinylive")
source("tools/build-shinylive.R")
```

Preview it locally:

```r
httpuv::runStaticServer("docs")
```

The generated `docs/` directory can be deployed to any static host, including
GitHub Pages.

The browser version depends on WebAssembly package binaries. If a dependency
fails to install in the browser, check <https://repo.r-wasm.org/> for the
current webR package availability.
