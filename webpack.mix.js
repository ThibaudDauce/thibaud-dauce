let mix = require('laravel-mix')

require('laravel-mix-purgecss');
let tailwindcss = require('tailwindcss');

mix.setPublicPath('./_site')
   .postCss('css/app.css', 'css', [
      tailwindcss('css/tailwind.js'),
   ])
   .purgeCss({
      folders: ['_site'],
      extensions: ['html'],
      whitelistPatternsChildren: [/content/],
   })
