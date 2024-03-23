# Advent of Code 2023 in Nix

Advent of Code 2023 solutions in pure Nix language, because I've been [nerd snipped](https://twitter.com/DeterminateSys/status/1731051201741926435).

```bash
nix-instantiate --eval solve.nix --strict --arg input ./input.txt
```

Note that some of these do not work on the provided example because they rely on properties found only in the real input.