---
title: "Laravel Recursive Migrations"
image: /images/migrations-subdirectories.png
thumbnail: /images/thumbnail-migrations-subdirectories.png
code: https://framagit.org/ThibaudDauce/laravel-recursive-migrations
description: This package allows to put Laravel migrations into sub-directories. Let's build it together!
---

I'm currently developing the [Quantic Telecom](https://www.quantic-telecom.net) website with Laravel. My `app` folder is well-organized as my `views` and my `resources` but not my migrations because the Laravel `php artisan migrate` command doesn't run nested migrations. My package [`thibaud-dauce/laravel-recursive-migrations`](https://framagit.org/ThibaudDauce/laravel-recursive-migrations) will allow us to put migrations into sub-directories.

In this blog post, I will go through the development of this package. For information, I found two links speaking about this particular problem: on StackOverflow ["Laravel running migrations on “app/database/migrations” folder recursively"](http://stackoverflow.com/questions/21641606/laravel-running-migrations-on-app-database-migrations-folder-recursively) and this issue on Github ["[Proposal/Enhancement] Artisan CLI Migrate command does not go through all sub folders of the migrations folder."](https://github.com/laravel/framework/issues/2561)

The goal of my package is not to create new commands like `submigrate`, `submigrate:rollback`… I want to keep the original names `migrate`, `migrate:rollback` and add an option `--recursive` which will look into all sub-directories.

<!--more-->

When thinking about this problem, several solutions came into my mind:

- Extending [`Illuminate\Database\Migrations\Migrator`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Migrations/Migrator.php)
- Extending all commands (migrate, refresh, reset…)

### Extending [`Illuminate\Database\Migrations\Migrator`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Migrations/Migrator.php)

The easiest approach is to extend the [`Illuminate\Database\Migrations\Migrator`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Migrations/Migrator.php) and override the [`getMigrationFiles()`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Migrations/Migrator.php#L298-L307) method. This method currently use the [`glob()`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Filesystem/Filesystem.php#L364-L367) method of the filesystem to get all files in the migration directory following the "*_*.php" pattern. Using the [`allFiles()`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Filesystem/Filesystem.php#L398-L401) recursive method instead should do the trick.

The main benefit of this approach is to act on all commands (migrate, rollback, reset…) with only one change because the [`Migrator`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Migrations/Migrator.php) is used by all of them. But the drawback is that the [`Migrator`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Migrations/Migrator.php) is a low level class, so it can't access the command's flags. So I will be forced to apply migrations recursively all the time.

### Extending all commands

Extending the commands' classes will allow me to override some of their methods and modify their behavior. It seems to be a good option for me. The only disadvantage is the fact that I need to create a new class for each of the migrations' commands: [`MigrateCommand`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Console/Migrations/MigrateCommand.php), [`RefreshCommand`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Console/Migrations/RefreshCommand.php), [`ResetCommand`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Console/Migrations/ResetCommand.php), [`RollbackCommand`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Console/Migrations/RollbackCommand.php) and [`StatusCommand`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Console/Migrations/StatusCommand.php). But I can manage the duplicate code with some traits.

So, I will split the work into two traits. The first one will be in charged of fetching subdirectories and will be general (pure and not linked to Laravel or the migration system). The second one, the ugly one, will need to be add to a child of the [`Illuminate\Database\Console\Migrations\BaseCommand`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Console/Migrations/BaseCommand.php) and will override the [`getMigrationPaths()`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Console/Migrations/BaseCommand.php#L24-L36), [`getOptions()`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Console/Migrations/MigrateCommand.php#L104-L119) and [`call()`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Console/Command.php#L179-L186) methods. I think it's often a bad choice to force a trait into an inheritance tree, but I couldn't find another solution…

#### My first trait: [`Subdirectories`](https://framagit.org/ThibaudDauce/laravel-recursive-migrations/blob/master/src/Subdirectories.php)

This trait will be pure. It will fetch the sub-directories for a path. I could have done a service class injected by the container but I thought it will be over complicated for some business logic that will never change (sub-directories will always be sub-directories). A service class would have given more flexibility to the end user by allowing him to change or extend my implementation.

The first method of this trait will fetch all sub-directories in a path with the help of the [`Finder`](https://github.com/symfony/symfony/blob/3.2/src/Symfony/Component/Finder/Finder.php) component:

```php
/**
 * Fetch all subdirectories from a path.
 * The original path is included in the array.
 *
 * @param string $path
 * @return string[]
 */
protected function subdirectories($path)
{
    return Collection::make(Finder::create()->in($path)->directories())
        ->map(function (SplFileInfo $directory) {
            return $directory->getPathname();
        })
        ->prepend($path)
        ->values()
        ->toArray();
}
```

The second method will flatMap the result of the first method for multiple base paths. flatMap is a function which map all element in an array to an array (you get an array of array) and then merge all arrays into one.

```php
/**
 * Fetch all subdirectories from an array of base paths.
 * The original paths are included in the array.
 *
 * @param string[] $paths
 * @return string[]
 */
protected function allSubdirectories($paths)
{
    return Collection::make($paths)
        ->flatMap(function ($path) {
            return $this->subdirectories($path);
        })
        ->toArray();
}
```

#### My second trait: [`RecursiveMigrationCommand`](https://framagit.org/ThibaudDauce/laravel-recursive-migrations/blob/master/src/RecursiveMigrationCommand.php)

The goal of this trait is to provide new methods for the children of [`Illuminate\Database\Console\Migrations\BaseCommand`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Console/Migrations/BaseCommand.php). It will use the previously seen [`Subdirectories`](https://framagit.org/ThibaudDauce/laravel-recursive-migrations/blob/master/src/Subdirectories.php) trait.

First, it will add the `--recursive` option. Nothing special here to the [`getOptions()`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Console/Migrations/MigrateCommand.php#L104-L119) method.

```php
/**
 * Get the console command options.
 *
 * @return array
 */
protected function getOptions()
{
    return array_merge(parent::getOptions(), [
        ['recursive', 'r', InputOption::VALUE_NONE, 'Indicates if the migrations should be run recursively (nested directories).']
    ]);
}
```

Then, it will add the sub-directories to the migration paths if the flag is set. The [`allSubdirectories()`](https://framagit.org/ThibaudDauce/laravel-recursive-migrations/blob/master/src/Subdirectories.php#L36-43) method comes from the [`Subdirectories`](https://framagit.org/ThibaudDauce/laravel-recursive-migrations/blob/master/src/Subdirectories.php) trait.

```php
/**
 * Get all of the migration paths.
 *
 * @return array
 */
protected function getMigrationPaths()
{
    $paths = parent::getMigrationPaths();
    return $this->option('recursive') ? $this->allSubdirectories($paths) : $paths;
}
```

And finally, the last one, `call()`, is only required for the [`RefreshCommand`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Console/Migrations/RefreshCommand.php). [`RefreshCommand`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Console/Migrations/RefreshCommand.php) does not contain any logic. It's only a wrapper around the [`ResetCommand`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Console/Migrations/ResetCommand.php) and the [`MigrateCommand`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Console/Migrations/MigrateCommand.php). Internally, it calls the others commands with the [`call()`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/Console/Migrations/RefreshCommand.php#L50-L66) method, passing its arguments like "step", "database" or "path"… But in this implementation, it will not pass the "recursive" flag to the other commands. I need to set it if the command called is a *migrate* command:

```php
/**
 * Call another console command.
 *
 * @param  string  $command
 * @param  array   $arguments
 * @return int
 */
public function call($command, array $arguments = [])
{
    if (starts_with($command, 'migrate')) {
        $arguments['--recursive'] = $this->option('recursive');
    }

    parent::call($command, $arguments);
}
```

#### New Commands

I also need to create my own commands for the [`MigrateCommand`](https://framagit.org/ThibaudDauce/laravel-recursive-migrations/blob/master/src/Commands/MigrateCommand.php), [`RefreshCommand`](https://framagit.org/ThibaudDauce/laravel-recursive-migrations/blob/master/src/Commands/RefreshCommand.php), [`ResetCommand`](https://framagit.org/ThibaudDauce/laravel-recursive-migrations/blob/master/src/Commands/ResetCommand.php), [`RollbackCommand`](https://framagit.org/ThibaudDauce/laravel-recursive-migrations/blob/master/src/Commands/RollbackCommand.php) and [`StatusCommand`](https://framagit.org/ThibaudDauce/laravel-recursive-migrations/blob/master/src/Commands/StatusCommand.php). All the implementations are the same thanks to the [`RecursiveMigrationCommand`](https://framagit.org/ThibaudDauce/laravel-recursive-migrations/blob/master/src/RecursiveMigrationCommand.php) trait. For example, my [`MigrateCommand`](https://framagit.org/ThibaudDauce/laravel-recursive-migrations/blob/master/src/Commands/MigrateCommand.php):

```php
<?php declare(strict_types = 1);

namespace ThibaudDauce\LaravelRecursiveMigrations\Commands;

use ThibaudDauce\LaravelRecursiveMigrations\RecursiveMigrationCommand;
use Illuminate\Database\Console\Migrations\MigrateCommand as BaseMigrateCommand;

class MigrateCommand extends BaseMigrateCommand
{
    use RecursiveMigrationCommand;
}
```

### Laravel integration

Last but not least, I need to create [a service provider](https://framagit.org/ThibaudDauce/laravel-recursive-migrations/blob/master/src/LaravelRecursiveMigrationsServiceProvider.php) to bind the new commands in the container. The names of the commands in the container are simple, and I only need to extend the binding.

```php
$this->app->extend('command.migrate', function () {
    return new MigrateCommand($this->app['migrator']);
});

$this->app->extend('command.migrate.rollback', function () {
    return new RollbackCommand($this->app['migrator']);
});

$this->app->extend('command.migrate.refresh', function () {
    return new RefreshCommand($this->app['migrator']);
});

$this->app->extend('command.migrate.reset', function () {
    return new ResetCommand($this->app['migrator']);
});

$this->app->extend('command.migrate.status', function () {
    return new StatusCommand($this->app['migrator']);
});
```

I didn't found a way to override the first binding with `bind()` because the [`MigrationServiceProvider`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Database/MigrationServiceProvider.php) is deferred by the [`ConsoleSupportServiceProvider`](https://github.com/laravel/framework/blob/5.3/src/Illuminate/Foundation/Providers/ConsoleSupportServiceProvider.php) so it will register his binding after mines. If I also defer my service provider, I get a "Provider already exists!" exception.

So, I need to use the `extend()` method which is not really the correct one I think. The extend method will first resolve the previous binding and give it to my closure. In my service provider, I don't care of the previous instance of the command since I create a new one (a new child) from scratch.

### Conclusion

The project is missing PHPUnit tests even if I already try the commands by hand in a fresh Laravel project. But it's available now on [Packagist](https://packagist.org/packages/thibaud-dauce/laravel-recursive-migrations) and on [Git](https://framagit.org/ThibaudDauce/laravel-recursive-migrations). Please open an issue if you find anything wrong in it.

I discover a lot about the migration system in Laravel during this little project, and I encourage you to dig in the source code of the framework: you'll learn a lot!
