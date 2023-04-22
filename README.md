# tetris-hs

Simple tetris in Haskell.

## How to build

### Windows

1. Install [ghcup](https://www.haskell.org/ghcup/) with stack.
2. Install dependencies with the following commands.

```
stack exec -- pacman -Syu msys2-keyring
stack exec -- pacman -Syuu
stack exec -- pacman -S mingw-w64-x86_64-crt-git mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2 mingw-w64-x86_64-SDL2_ttf
```

3. Run `stack build`.
