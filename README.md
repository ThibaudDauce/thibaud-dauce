# Thibaud Dauce

This is [my personal website](https://thibaud.dauce.fr) with [Hakyll](http://jaspervdj.be/hakyll/), all the explanations are [in my blog post](https://thibaud.dauce.fr/posts/2015-10-18-new-static-website.html).

## Requirements

### Stack

You need to install the [`stack` build tool](https://github.com/commercialhaskell/stack) first. See [the official documentation](https://github.com/commercialhaskell/stack/blob/release/doc/install_and_upgrade.md) for installation instructions.

On Arch Linux, just run:

```bash
pacman -S stack
```

### Front-end

I'm using [Laravel Mix](https://laravel-mix.com/) to compile my CSS. You'll need NPM to run the scripts.

On Arch Linux, just run:

```bash
pacman -S nodejs
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

This command will trigger a rebuild each time a file is changed. To rebuild the CSS after a change, you'll need to run `npm run watch`.
