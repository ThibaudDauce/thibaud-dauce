---
title: "What to learn in 2019?"
description: "In 2019, I want to challenge the way I build applications by exploring other interesting technologies."
---

Computer science is evolving at a rapid pace. There are new developers bringing new ideas. And there are many ancient discoveries forgotten or underused today. The fact is: there is a lot to learn in computer science in 2019. 

This year, I want to go out of my comfort zone and try new stuff. Don't get me wrong, I still love the PHP ecosystem, and Laravel is still an amazing framework to work with (I even [teach Laravel](https://www.formation-laravel.fr/) if you're interested).

So, why switch? To begin with, I don't want to switch but only becoming productive in other areas of computed sciences. Secondly, PHP, as a language, is not that great and the recent features added are not really interesting. I have the feeling PHP is trying to copy Java or other mainstream languages without the correctness of the compilation. I don't consider my program crashing at runtime because of a type annotation is a real improvement over my program crashing at runtime because the method is called on the wrong object.

### What are the goals? Productivity, correctness and performances.

**Productivity** is the most meaningful one. I'm paid to deliver value to a customer and not to do research around programming languages. But productivity is also the easiest metric to improve, I'm sure you can be productive in any language if you practice enough.

**Correctness** is about writing a program and have some guaranties that the result will work as expected. PHP is especially bad at this, but other languages can have different levels of verification with more or less strict type systems.

And for **performances**, even though PHP 7 brought a significant speed boost, the language is still slow. The web is also a really slow platform, and I would like experimenting unfamiliar things like high performance CLI apps and native GUIs. 

### Who are the challengers?

[Haskell](https://haskell-lang.org/) is a strictly typed, purely functional, "if it compiles, it works" kind of language. I'm afraid that the accent on absolute correctness and the bad documentation will slow me down on some manageable problems.
<br class="inline-block mb-2">
<span class="text-orange-light">★★</span><span class="text-orange-darker">☆☆</span> Productivity |
<span class="text-orange-light">★★★★</span><span class="text-orange-darker"></span> Correctness |
<span class="text-orange-light">★★</span><span class="text-orange-darker">☆☆</span> Performances

[Elixir](https://elixir-lang.org/) is a functional language based on the Erlang VM with the Ruby syntax. I think I can become highly productive with this kind of language. The correctness is a little bit behind because the language is dynamically typed but the compilation can still identify some issues before going into production. And last but not least, the performance are amazing: less than a millisecond to render a simple HTML webpage with [Phoenix](https://phoenixframework.org/).
<br class="inline-block mb-2">
<span class="text-orange-light">★★★</span><span class="text-orange-darker">☆</span> Productivity |
<span class="text-orange-light">★★</span><span class="text-orange-darker">☆☆</span> Correctness |
<span class="text-orange-light">★★★</span><span class="text-orange-darker">☆</span> Performances

[Rust](https://www.rust-lang.org/) is a low-level programming language with C-like performances. Memory management lower the productivity, but the documentation is really good. And the type system can detect the majority of silly errors at compile-time.
<br class="inline-block mb-2">
<span class="text-orange-light">★</span><span class="text-orange-darker">☆☆☆</span> Productivity |
<span class="text-orange-light">★★★</span><span class="text-orange-darker">☆</span> Correctness |
<span class="text-orange-light">★★★★</span><span class="text-orange-darker"></span> Performances

[PHP](https://secure.php.net/), because this is the tool I provide the most value with. The ecosystem is amazing, but I will not discover anything new soon. And I don't like that.
<br class="inline-block mb-2">
<span class="text-orange-light">★★★★</span><span class="text-orange-darker"></span> Productivity |
<span class="text-orange-light">★</span><span class="text-orange-darker">☆☆☆</span> Correctness |
<span class="text-orange-light">★</span><span class="text-orange-darker">☆☆☆</span> Performances

### So? What will I learn in 2019?

My first choice is Rust. It's the language where I have the most to learn.

For a few weeks I tried building a text editor with Rust, and it was very hard. How to render a character on the screen? How to create a blinking cursor? How to add words in the middle of a large string? I think this project is overly complex for me now but it was a good first try.

I also built a small utility (a few lines of code) to print statistics about my websites. I don't use standard analytics, so I just grabbed the last few days of Nginx's logs and printed the visits for each day and each URI.

Thanks for reading, it's been a while (more than one year) between my previous post and this one, and my written english is a little bit Rusty, please forgive me :-) You can subscribe to my RSS feed to receive my future posts if you want.