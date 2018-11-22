---
title: Welcome to my new static website!
image: /images/guard.jpg
thumbnail: /images/thumbnail-guard.jpg
---

I just rebuilt my website with Hakyll, a library for generating static sites. No major visual changes: the design remains almost the same except I'm now using the newest [KNACSS](http://knacss.com/) version and I dropped [Geomicons](http://geomicons.com/) for [Font awesome](http://fontawesome.io/). I also took advantage of this change to add a new blog to my website, I'll try to keep it updated weekly (even if we all know it's impossible ^^).

Why switch from PHP and [Laravel](https://laravel.com) to just HTML files? Why a static website generator?

<!--more-->

### Why a static website generator?

Because, to quote [the Hakyll website](http://jaspervdj.be/hakyll/):

> Static sites are fast, secure, easy to deploy, and manageable using version control.

![A static, fast, secure and easy to deploy Buckingham's guard](/images/guard.jpg)

I like the idea of using only an Apache web server (~~yeah, no Nginx for me, I'm old school~~ not anymore, only fools never change their minds <i class="fa fa-smile-o"></i>) and deploy my application easily with  ~~`scp`~~ `rsync`. And, of course, no security breach, it's not a Wordpress!

But, if HTML files are so cool, why don't just use HTML files? Generators brings a lot of possibilities for a blog.

- Use of layouts or templates for your pages. Don't repeat yourself: don't copy / paste the `doctype` and the `header` for each post.
- Create a dynamic index for all your posts (with a summary), an archive page and a RSS / Atom feed.
- Write in [Markdown](https://daringfireball.net/projects/markdown/) instead of HTML, it's a bit easier and it's always possible to add HTML tags in your posts for specific information.

### How is it working?

As I said, I'm using [Hakyll](http://jaspervdj.be/hakyll/). It's develop with [Haskell](https://www.haskell.org/) but every language has a static site generator available: [Pelican](https://github.com/getpelican/pelican/) in Python, [Jekyll](http://jekyllrb.com/) in Ruby, [Sculpin](https://sculpin.io/) in PHP…

I don't think Hakyll is the best one, but I'm currently in functional programming and I really wanted to try to do some Haskell. Hopefully, there is not a lot of Haskell to write and thanks to [Yann Esposito's article](http://yannesposito.com/Scratch/en/blog/Hakyll-setup/), the Haskell part was really easy to do.

After developing the main Haskell file, I just run `stack build` once to compile my program. Then, to write this blog post, I run `stack exec thibaud watch`, open `http://localhost:8000` and start writing my post with [my editor](https://atom.io). Each time I save the file, a rebuild is triggered, and I can see the changes directly in my browser. Really efficient workflow!

I write Markdown compiled in HTML thanks to the great [Pandoc](http://pandoc.org/) library. I use an HTML comment `<!--more-->` to create a « teaser » field. And I also can use HTML to add font awesome character for example like `<i class="fa fa-smile-o"></i>`. All of the code is on [Github](https://github.com/ThibaudDauce/thibaud-dauce), and I'm open to contributions <i class="fa fa-smile-o"></i>.

### What's remaining to do?

This is a first quick version of my Hakyll website, a lot is remaining:

- Remove the `.html` extensions in the URL;
- Add a RSS feed;
- Clean some SASS files;
- Remove external calls to Google fonts and Font Awesome CDN;
- Concatenate CSS files;
- Add texts about my current work in Spark / Scala and my summer internship with NodeJS and Amazon Kinesis.

Thanks for reading me! My English is far from perfect, so I'm open to all remarks, comments and improvements either with Github pull requests or on [Twitter](https://twitter.com/ThibaudDauce).
