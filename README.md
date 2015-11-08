# thibaud-dauce

Personal website with Hakyll, [explanations here](http://www.thibaud-dauce.fr/posts/2015-10-18-new-static-website.html).

## Requirements

### Stack

You need to install the `stack` build tool first. See https://github.com/commercialhaskell/stack for more information.

On Arch Linux, just run:
```bash
yaourt -S stack-bin
```

### Sass

I'm using [SASS](http://sass-lang.com/) to compile my CSS. For installation instruction, go to http://sass-lang.com/install.

On Arch Linux, just run:
```bash
yaourt -S ruby-sass
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
