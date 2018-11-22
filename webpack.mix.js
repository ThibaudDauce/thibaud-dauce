let mix = require('laravel-mix')

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
