---
title: "Your app doesn't have \"users\""
image: /images/your-app-doesnt-have-users.png
thumbnail: /images/thumbnail-your-app-doesnt-have-users.png
description: The most important part of your code is how you're naming things. Don't forget your first class!
---

I'm a big fan of Laravel. The thing I like the most about the community is the focus on understandable code. Not patterns, not architectures, but how to choose the best API for a task so the result is readable as plain English.

But today, I want to talk about one name that Laravel's developers often get wrong.

<!--more-->

---

The default installation of Laravel comes with a `User` class and a migration to create a `users` table. Why? Because, obviously, every application has users.

*That's not true.*

Adam Wathan is a Laravel developer who provides an awesome course to learn how to build great applications: ["Test Driven Laravel"](https://adamwathan.me/test-driven-laravel/).

![Test Driven Laravel course](/images/ticket-beast-promoters.png)

TicketBeast, the Test Driven Laravel sample application, is about promoters and concerts, but in the application there is a `User` class and a `Concert` class but no `Promoter` class.

He is also producing free lives, twice a week, on his [YouTube channel](https://www.youtube.com/channel/UCy1H38XrN7hi7wHSClfXPqQ/videos) where he builds [KiteTail](https://kitetail.co/). Adam put a lot of thought into the name of his users and eventually chose **"makers"** after a few videos. But once again, there is no `Maker` class in KiteTail.

If you watch (and you should) the new series at Laracasts ["Let's Build A Forum with Laravel and TDD"](https://laracasts.com/series/lets-build-a-forum-with-laravel/), you will notice that Jeffrey also uses the `User` class. But this time, he really uses the term "user" when speaking and writing descriptions for his tests.

On the other hand, in the second video, he used the term **"creator"** for "a thread has a creator" and he renamed his relation from `user()` to `creator()`. That's much more readable than `user()`, because what is the user of a thread anyway? If the default doesn't seem right for you, don't use it!

When you don't use the default, Laravel will look for a "creator_id" column in the table instead of a "user_id" one. Jeffrey chose to override the default with `return $this->belongsTo(User::class, 'user_id');`, but I think it could have been better to embrace the new term and rename the table's column from "user_id" to "creator_id". If you need to speak about this table, you could then say "I store the ID of the creator with the body of the thread" instead of "I store the ID of the user with the body of the thread". The first one seems much more natural to me.

---

Thread **creator**, concert **promoter**, product **maker**, it's only three examples among many.

The most important thing to keep in mind is that you should name your classes as it feels natural to **you**. For example, I would use **"members"** to designate the users of a forum but Jeffrey seems to use **"users"** naturally. There is no right or wrong here (as often in development), you only need to choose the correct one so you can speak about your code without translating anything.

So next time you run `laravel new my-project`, don't be lazy, take 5 minutes to think about your product and if you need to rename your `User` class into something else. It's always easier when it's done on a fresh project :-)

I choose to speak about `User` because I think it's often forgotten but it's also true in most applications for "status", "data", "items", "result", "payload"… But don't forget that in a `Collection` class, "items" is perfectly natural, and in the Guzzle HTTP library, "payload" is fine too.

If you want to learn more about it:

- [https://murze.be/2017/06/the-status-antipattern/](https://murze.be/2017/06/the-status-antipattern/)
- [http://twentypercent.fm/naming-things](http://twentypercent.fm/naming-things)

And if you want to learn how to improve your code:

- [https://adamwathan.me/test-driven-laravel/](https://adamwathan.me/test-driven-laravel/)
- [Adam's YouTube channel](https://www.youtube.com/channel/UCy1H38XrN7hi7wHSClfXPqQ/videos)
- [Laracasts](https://laracasts.com/)
- For french speakers: [Formations Laravel](https://www.formations-laravel.fr/)
