---
title: "Laravel Foreign Relations"
image: /images/stripe.png
thumbnail: /images/thumbnail-stripe.png
code: https://framagit.org/ThibaudDauce/laravel-foreign-relations
description: In this article, I'll create a proof of concept using Laravel's Eloquent relations to fetch a foreign data source as Stripe.
---

The Laravel community is a huge fan of [Stripe](https://stripe.com). As Laravel, Stripe provides a great API to interact with the product. In this article, I'll create a proof of concept using Laravel's Eloquent relations to fetch a foreign data source as Stripe. The same idea could be applied to other foreign data providers.

<!--more-->

### Laravel's relation mecanism

As you may know, you can [define relations](https://laravel.com/docs/5.3/eloquent-relationships) with Laravel. In your model, `hasMany`, `belongsTo` or `belongsToMany` are already-provided helpers to create links between your models.

```php
<?php

class User {  
    public function comments()
    {
        return $this->hasMany(Comment::class);
    }
}
```

But behind the scene, these methods returns children of the `Relation` object. These children could be `HasMany`, `BelongsTo`, `BelongsToMany`, etc. The `Relation` object is an abstract class you need to extend to build a new type of relations. This class is tightly tied with the Eloquent Builder class, which is a big problem. Laravel assumes Eloquent models in both sides of our relation. But in our case, it's one Eloquent model and one `Stripe\Customer` object.

### Building the `StripeRelation`

`StripeRelation` is my child for the `Relation` object. It will fetch and hydrate my models with the customers from Stripe.

#### Constructor

In order to build a `Relation` object, Laravel needs a builder from the foreign class and the instance of the main model. In our example, the foreign class is `Stripe\Customer` and the main model is `App\User`.

First problem: I don't have an Eloquent builder for Stripe customers. I'll need [to create my own](#building-the-stripequerybuilder).

```php
public function __construct(Model $model)
{
    parent::__construct(app(StripeQueryBuilder::class), $model);
}
```

#### `addConstraints` and `addEagerConstraints`

The main goal of these methods is to set a `where` clause on the query builder with "id = stripe_id" or "id in [stripe_id_1, stripe_id_2, stripe_id_3]". Since my query builder is a special one, and I know it will be a custom `StripeQueryBuilder`, I can set two public properties `id` for when I need to fetch one Stripe customer, `ids` when I need to fetch all Stripe customers for a collection of users. We will see in the next part how we use these information to fetch the customers from Stripe.

```php
public function addConstraints()
{
    $this->query->id = $this->parent->{$this->localKey};
}

public function addEagerConstraints(array $models)
{
    $this->query->ids = $this->getKeys($models, $this->localKey);
}
```

In these methods `$this->localKey` is the name of the attribute of the user model which contains the Stripe ID (default to `stripe_id`).

#### `initRelation`

This method sets the default foreign model for all the users. I don't have a default Stripe model so I just return the unchanged array of models.

#### `getResults`

The name of this method is misleading, its goal is just to fetch one foreign model (one Stripe customer) for one parent model (one user) when you simply type `$user->stripe`. I can delegate the API call to my `StripeQueryBuilder` class. The `stripe_id` of the user has already been set in the `StripeQueryBuilder` by the parent `Relation` class with `addConstraints`.

```php
public function getResults()
{
    return $this->query->first();
}
```

#### `match`

This method is called when you try to load the relation on a collection to avoid the [N + 1 problem](https://laravel.com/docs/5.3/eloquent-relationships#eager-loading). It takes three arguments:

- the models — an array of users which needs to be populated with the customers from Stripe
- the results — an Eloquent Collection from the query builder (basicaly a call to `$query->addEagerConstraints()->get()`) which contains for each Stripe ID the Stripe customer associated
- the relation — the name of the current relation being processed

The logic is simple, loop over the models and set the relation to whatever is in the `$results` collection:

```php
public function match(array $models, Collection $results, $relation)
{
    foreach ($models as $model) {
        $model->setRelation($relation, $results[$model->{$this->localKey}]);
    }

    return $models;
}
```

### Building the `StripeQueryBuilder`

Now we need to build our `StripeQueryBuilder` which will call the Stripe API to fetch the requested customers. My class needs to extend the Eloquent Builder and override some methods. Most of the methods will not be changed so it will be impossible to call more *intelligent* methods like `$user->stripe()->update(['delinquent' => true])`.

#### Constructor

The parent model needs a Query. I will simply mock it.

```php
public function __construct()
{
    parent::__construct(Mockery::mock(QueryBuilder::class));
}
```

#### `first`

The `first` method fetch one customer based on the ID previously set in the `$id` attribute of the Query Builder.

```php
public function first($columns = ['*'])
{
    if ($this->id) {
        return Customer::retrieve($this->id);
    }

    throw new \Exception("'first' method called with no ID provided.");
}
```

#### `get`

It's impossible to ask Stripe for all customers with IDs in an array (a simple `WHERE IN` with SQL). I need to fetch all customers and then filter them with PHP. The maximum number of customers for each request is 100. So I need to ask for 100 customers until Stripe tells me that there is no more or all my IDs have been found.

```php
public function get($columns = ['*'])
{
    $models = new Collection;

    if ($this->ids) {
        $totalCount = count($this->ids);

        foreach ($this->fetchAllStripeCustomers() as $customer) {
            if (in_array($customer->id, $this->ids)) {
                $models[$customer->id] = $customer;
            }

            if ($totalCount === $models->count()) {
                // All models were found, no need to continue to fetch the customers.
                break;
            }
        }
    }

    return $models;
}
```

### Conclusion

This project is a very basic implementation. It's probably possible to do more. But it shows how you can keep a simple API for your models even with foreign relations. The working final project is available on [https://framagit.org/ThibaudDauce/laravel-foreign-relations](https://framagit.org/ThibaudDauce/laravel-foreign-relations).

I may put this work in a package if some people are interested.

Eventually, for french artisan, I offer Laravel trainings, just visit [https://www.formations-laravel.fr](https://www.formations-laravel.fr) and send me a message!
