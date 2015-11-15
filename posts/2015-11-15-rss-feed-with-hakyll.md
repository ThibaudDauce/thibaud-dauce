---
title: An RSS Feed with Hakyll
---

Twitter is a really cool social network. I daily use it and I discover a lot of new stuff with it. Unfortunately, it's a company, and they need to earn some money. More and more often, Twitter shows me unwanted content like *tweets you could like*, *accounts to follow* or *what happens when you're away*.

I don't want all these features, I just want the tweets of people I follow in chronological order. What I want is an RSS feed.

<!--more-->

![RSS, the perfect alternative to Twitter](/images/rss.jpg)

# Add an RSS feed in Hakyll

It's a really simple operation, everything is explained [here](http://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html). In the tutorial, they explain how to render either an Atom or an RSS feed. I decided to provide both. To do that, I've created a small function named `createFeed`.

```haskell
createFeed :: Identifier -> RenderingFunction -> Rules ()
```

Where `RenderingFunction` is the signature provided by Hakyll for `renderAtom` and `renderRss`.

```haskell
type RenderingFunction = FeedConfiguration
           -> Context String
           -> [Item String]
           -> Compiler (Item String)
```

Then, my function is just a copy paste from the function in the tutorial.

```haskell
createFeed name renderingFunction = create [name] $ do
      route idRoute
      compile $ do
          posts <- fmap (take 10) . recentFirst =<<
              loadAllSnapshots "posts/*" "content"
          renderingFunction myFeedConfiguration feedCtx posts
```

Eventually, I just need to call my new function for `renderAtom` and `renderRss`.

```haskell
createFeed "feed.xml" renderRss
createFeed "atom.xml" renderAtom
```

# The HTML part

To make my feeds work with most of the aggregators, I need to add two `link` to my default template.

```html
<link rel="alternate" type="application/rss+xml" title="Thibaud Dauce's blog" href="./feed.xml">
<link rel="alternate" type="application/atom+xml" title="Thibaud Dauce's blog" href="./atom.xml">
```

# Which reader

I personally use [FreshRSS](http://freshrss.org/). It's a simple, self-hostable aggregator under the AGPL license. The installation is really simple and there is a lot of useful features (like keyboard shortcuts and integration with [Wallabag](https://www.wallabag.org/))

![The clear interface of FreshRSS](/images/freshrss.png)
