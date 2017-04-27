---
title: "Pattern Matching in PHP"
image: /images/migrations-subdirectories.png
thumbnail: /images/thumbnail-migrations-subdirectories.png
code: https://gitlab.com/thibauddauce/pattern-matching
description: How to build a pattern matching library in PHP?
---

I'm a big fan of Haskell and one of my favorite feature of this awesome language is pattern matching. If you don't know what's pattern matching, it's this:

```haskell
data Customer = Student | Individual

getPrice :: Customer -> Int
getPrice Student    = 10
getPrice Individual = 30
```

The main advantage is, if I add a possibility in my type, for example `data Customer = Student | Individual | Company`, the Haskell compiler will tell me that my function is missing a pattern. I will need to provide a `getPrice Company = ?`.

Of course, in PHP we don't have a compiler but we can still have some checks…

<!--more-->

### The problem

Imagine I have a class for my customer:

```php
<?php

class Customer {
    const STUDENT = 'student';
    const INDIVIDUAL = 'individual';

    public $type;

    public function getPrice()
    {
        if ($this->type === self::STUDENT) {
            return 10;
        } elseif ($this->type === self::INDIVIDUAL) {
            return 30;
        } else {
            throw new InvalidArgumentException('Neither student nor individual.');
        }
    }
}
```

By the way, pay attention of how 4 lines of code became 18 lines. But, of course, Haskell is a incomprehensible language ;-)

If I have my unit tests like these:

```php
$customer = new Customer; // in real life, I would use a name constructor…
$customer->type = Customer::STUDENT;

$this->assertEquals(10, $customer->getPrice());
```

```php
$customer = new Customer;
$customer->type = Customer::INDIVIDUAL;

$this->assertEquals(30, $customer->getPrice());
```

If latter, I add a new type `const COMPANY = 'company'` in my class, my tests will remain green. If I use this `if() {} elseif () {} else {}` in a lot of place, it will be difficult to catch all occurrences.

### The solution

Use [my small pattern matching library](https://gitlab.com/thibauddauce/pattern-matching) (or build your own, it's only a mater of hours…):

```php
<?php

class Customer {
    const STUDENT = 'student';
    const INDIVIDUAL = 'individual';

    public $type;

    public function patternMatchOnType(array $actions)
    {
        return (new Pattern([self::STUDENT, self::INDIVIDUAL]))
            ->match($this->type, $actions);
    }

    public function getPrice()
    {
        $this->patternMatchOnType([
            self::STUDENT => 10,
            self::INDIVIDUAL => 30,
        ]);
    }
}
```

If I add a new type and I change my enumerate definition in one place `new Pattern([self::STUDENT, self::INDIVIDUAL, self::COMPANY])`, my previous tests are now failing with a new exception:

```
MissingPatternsDuringMatch: 'company' was missing during the match. Expected patterns were 'student', 'individual', 'company' and received patterns were 'student', 'individual'.
```

Even if, I never wrote a test to check if the price for a company is correct.

### Extra features of my library

You can check [the test file](https://gitlab.com/thibauddauce/pattern-matching/blob/master/tests/PatternTest.php) for all features.

#### Checks

My `match()` function is three simple checks happening every time. It means, it raises an exception even if the pattern could be resolved.

- there is no missing pattern in the array (all cases must have a response)
- there is no extra pattern in the array (if a type is removed, I want to remove all the occurrences of the dead code)
- the value provided is in the pattern list

#### Callbacks

If your application is doing expensive work for each pattern (more expensive than returning "10" or "30"), you can wrap the result in a callback:

```php
$this->patternMatchOnType([
    self::STUDENT => function() {
        return StudentPrice::fetchFromFile();
    },
    self::INDIVIDUAL => function() {
        return Price::where('type', self::INDIVIDUAL)->first()->value;
    },
]);
```

#### Callbacks arguments

And you can also give arguments to your callbacks with `with`:

```php
public function patternMatchOnType(array $actions)
{
    return (new Pattern([self::STUDENT, self::INDIVIDUAL]))
        ->with($this) // The callbacks will receive the instance of the customer
        ->match($this->type, $actions);
}

public function getPrice()
{
    $this->patternMatchOnType([
        self::STUDENT => 10,
        self::INDIVIDUAL => function(Customer $customer) {
            return IndividualPrice::where('age', '>', $customer->age)->first()->value;
        },
    ]);
}
```

Even if, in this case, you could simply use `$this->age` inside the callback.
