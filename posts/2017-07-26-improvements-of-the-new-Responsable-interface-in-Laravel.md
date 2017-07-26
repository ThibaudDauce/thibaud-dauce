---
title: "Improvements of the new Responsable interface in Laravel"
image: /images/responsable.png
thumbnail: /images/thumbnail-responsable.png
description: "Laravel 5.5 will ship with a new feature: the Responsable interface. Why not add some sugar to it?"
---

The new `Responsable` interface in Laravel is really awesome, it allows us to simplify our controllers with custom responses objects.

But one of the complexity with responses is the content negotiation and [the new feature](https://github.com/laravel/framework/commit/c0c89fd73cebf9ed56e6c5e69ad35106df03d9db) doesn't help us on this. How could we solve this problem?

<!--more-->

---

### The new feature

First, let's look at how Laravel uses the `Responsable` interface in the Router.

```php
if ($response instanceof Responsable) {
    $response = $response->toResponse();
}

if ($response instanceof PsrResponseInterface) {
    $response = (new HttpFoundationFactory)->createResponse($response);
} elseif (! $response instanceof SymfonyResponse &&
           ($response instanceof Arrayable ||
            $response instanceof Jsonable ||
            $response instanceof ArrayObject ||
            $response instanceof JsonSerializable ||
            is_array($response))) {
    $response = new JsonResponse($response);
} elseif (! $response instanceof SymfonyResponse) {
    $response = new Response($response);
}
```

The only method on the `Respondable` interface is the `toResponse` method. Then, if the response is `Jsonable` or something similar, Laravel will use the `JsonResponse` which `json_encode` all the data.

### Adding Content Negotiation

There is no sign of Content-Negotiation in this method. So if I need to build a response in JSON or in HTML depending of the type the request, I will have some conditions in my controller:

```php
public function index()
{
    $users = User::all();

    if (request()->wantsJson()) {
        return new UsersJsonResponse($users);
    }

    return new UsersHtmlResponse($users);
}
```

Not too bad, thanks to the awesome `wantsJson()` method, but it could be better (especially if my controller is not a simple database query):

```php
public function index()
{
    $users = User::all();

    // … Lots of really complicated stuff

    return UsersResponse($users);
}
```

Better! Now let's dive in the `UsersResponse`:

```php
class UsersResponse implements Responsable
{
    public $users;

    public function __construct($users)
    {
        $this->users = $users;
    }

    public function toResponse()
    {
        if (request()->wantsJson()) {
            return $this->users;
        }

        return view('users.index', ['users' => $this->users]);
    }
}
```

### Avoiding conditionals with polymorphism

It's often a good thing to replace conditionals by polymorphism. I would rather prefer this response with a new `NegociateContent` trait:

```php
class UsersResponse implements Responsable
{
    use NegociateContent;

    public $users;

    public function __construct($users)
    {
        $this->users = $users;
    }

    public function toJsonResponse()
    {
        return $this->users;
    }

    public function toHtmlResponse()
    {
        return view('users.index', ['users' => $this->users]);
    }
}
```

Or this one which extends a new `BaseResponse`:

```php
class UsersResponse extends BaseResponse
{
    public $users;

    public function __construct($users)
    {
        $this->users = $users;
    }

    public function toJsonResponse()
    {
        return $this->users;
    }

    public function toHtmlResponse()
    {
        return view('users.index', ['users' => $this->users]);
    }
}
```

The `NegociateContent` or the `BaseResponse` implementations are really straightforward. We can add more checks and content type if needed of course.

```php
class BaseResponse implements Respondable
{
    public function toResponse()
    {
        if (request()->wantsJson()) {
            return $this->toJsonResponse();
        }

        return $this->toHtmlResponse();
    }
}
```

### Let's try to build the same `view` helper as in mailables

We can even simplify the `toHtmlResponse` method with the same concept as in `Mailable`. As you may know, in mailables, all public properties are directly accessible in the mail view.

```php
public $users;

public function toHtmlResponse()
{
    return $this->view('users.index');
}
```

I stole this code from the [`Illuminate\Mail\Mailable`](https://github.com/laravel/framework/blob/master/src/Illuminate/Mail/Mailable.php#L223) class in the framework (I just refactored it to collections).

```php
public function view($view, $overrides = [])
{
    return collect((new ReflectionClass($this))->getProperties(ReflectionProperty::IS_PUBLIC))
        ->reject(function($property) {
            return $property->getDeclaringClass()->getName() == self::class;
        })
        ->mapWithKeys(function($property) {
            return [$property->getName(), $property->getValue($this)];
        })
        ->merge($overrides)
        ->pipe(function($data) use($view) {
            return view($view, $data);
        });
}
```

### Conclusion

I think these few lines of code really improve the `Responsable` interface. I really like the `Mailable` reflection as it feels really natural to me.

I may try to pull request the framework to add these features if people like them so don't hesitate to send me a message [on Twitter \@ThibaudDauce](https://twitter.com/ThibaudDauce) or [by mail (thibaud@dauce.fr)](mailto:thibaud@dauce.fr) to tell me what you think!
