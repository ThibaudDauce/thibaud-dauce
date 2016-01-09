# Thibaud Dauce

This is [my personal website](http://www.thibaud-dauce.fr) with [Hakyll](http://jaspervdj.be/hakyll/), all the explanations are [in my blog post](http://www.thibaud-dauce.fr/posts/2015-10-18-new-static-website.html).

## Requirements

### Stack

You need to install the [`stack` build tool](https://github.com/commercialhaskell/stack) first. See [the official documentation](https://github.com/commercialhaskell/stack/blob/release/doc/install_and_upgrade.md) for installation instructions.

On Arch Linux, just run:
```bash
yaourt -S stack-bin
```

### Sass

I'm using [libsass](http://sass-lang.com/libsass) to compile my CSS. For installation instruction, go to https://github.com/sass/libsass.

On Arch Linux, just run:
```bash
pacman -S sassc
```

## Installation

First, download GHC and all the Haskell stuff:
```bash
stack setup
```

Then, build the executable:
```bash
stack build
```

And finally, generate the website:
```bash
stack exec thibaud build
```

## Usage

### Haskell modification

Each time you modify the Haskell files, you need to rebuild the executable with:
```bash
stack build
```

### Posts' creations and modifications

To start with a clean installation, run:
```bash
stack exec thibaud rebuild
```

This command will clean the `_site` folder and rebuild all the website.

Then run the web server with:
```bash
stack exec thibaud watch
```

This command will trigger a rebuild each time a file is changed. To rebuild the CSS after a change in a `.scss` file, you need to trigger a rebuild manually with `stack exec thibaud rebuild`.
