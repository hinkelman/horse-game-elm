# Horse Game (Elm)

An Elm 0.19.2 port of the [Shiny app](https://github.com/hinkelman/horse-game) for tracking the
[Across the Board Kentucky Derby horse racing game](https://www.travishinkelman.com/horse-game/):
enter the base value and the four scratches, tap each dice roll, and see the kitty and each horse's
chance of winning.

Differences from the Shiny version:

- **Exact win probabilities.** The Shiny version runs 1,000 simulated games per roll. This version
  computes the odds exactly: it treats each horse's progress as an independent Poisson process and
  integrates the Erlang finish-time densities. The merged processes produce the same order of rolls
  as the real game, so the result is exact apart from numerical integration error.
- **Static site.** It needs no server and deploys anywhere that serves static files.
- **Saved progress.** The game state is kept in `localStorage`, so a refresh doesn't lose rolls.
- Mobile-first single-column layout, color-coded roll pad with undo, a race-track view of every
  horse, and dark mode.

## Develop

```sh
elm reactor            # then open http://localhost:8000/public/index.html after building once
# or
elm make src/Main.elm --output=public/elm.js && python3 -m http.server -d public
```

## Build & deploy

```sh
./build.sh             # optimized + minified (also minifies if `terser` is installed)
```

Upload the `public/` folder to any static host (GitHub Pages, Netlify, Cloudflare Pages, S3, …).
The included workflow `.github/workflows/deploy.yml` builds and publishes to GitHub Pages on every
push to `main`. To turn it on, go to *Settings → Pages → Source* and choose "GitHub Actions".
