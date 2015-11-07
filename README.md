# thibaud-dauce

Personal website with Hakyll.

## Requirements

### Stack

You need to install `stack` first. See https://github.com/commercialhaskell/stack for more information.

On Archlinux, just run:
```bash
yaourt -S stack-bin
```

## Installation

First build the executable:
```bash
stack setup
stack build
```

Then, generate the website:
```bash
stack exec thibaud build
```

## Usage

When starting to write a new page, first run:
```bash
stack exec thibaud rebuild
```

Then run the web server with:
```bash
stack exec thibaud watch
```
