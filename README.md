# wec
wav echo effect

Usage

```
espeak-ng -w hello.wav "hello"
wec -d 1.5 hello.wav | aplay
```

Development

```
cabal build
cabal run wec -- --help
```
