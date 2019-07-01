---
title: "I don't like camelCase"
description: "Snake case, camel case… Why different languages made different choices?"
---

Which one is the most readable?

```php
function getFirstUserFrom(SiretNumber $siretNumber): User {}
```

```php
function get_first_user_from(Siret_Number $siret_number): User {}
```

I think the snake case version is much more readable than the camel case.

How we name our functions is not a technical choice, it's a convention. Nobody can say that the snake case version is worse than the camel case one. The people from the PSR committee decided [a long time ago](https://www.php-fig.org/psr/psr-1/) that, in PHP, methods will be named in camel case and classes will be named in studly case. But in Rust, or in Python, a different choice was made: it's snake case for functions and methods.

Don't let other people decide what works best for you. Recently, I started using snake case for all my functions, methods and variables. And I think I'll switch to the `Siret_Number` approach for all my classes.