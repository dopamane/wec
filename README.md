# wec
wav echo effect

Usage

```
tone -t 0.33 300 | wec -t 1 -c 3 -p 50 | aplay
```

Development

```
cabal build
cabal haddock
cabal run wec -- --help
```
