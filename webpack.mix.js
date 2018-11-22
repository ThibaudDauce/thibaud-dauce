let mix = require('laravel-mix')
let glob = require('glob-all')

require('laravel-mix-tailwind');
require('laravel-mix-purgecss');

mix.setPublicPath('./_site')
   .postCss('css/app.css', 'css')
   .tailwind('css/tailwind.js')
   .purgeCss({
      folders: ['_site'],
      extensions: ['html'],
      whitelistPatternsChildren: [/content/],
   })
