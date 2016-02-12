---
title: Tips & Tricks with LaTeX
image: /images/latex.png
---

In [my previous blog post](/posts/2015-12-06-write-well-formed-documents-with-latex.html), I talked about how LaTeX is a fantastic tool and I concluded with some generators that could be interesting. I had to write some documents with LaTeX and I wanted to improve my work flow. I will share with you some tips & tricks I discovered.

<!--more-->

## Title page

Create a title page with LaTeX could scare you but I realized that with the awesome LaTeX documentation, it was really easy to customize the title page. Just check [this wiki page](https://en.wikibooks.org/wiki/LaTeX/Title_Creation) and it should explain everything to you.

## White spaces!

![An ugly compact documents with a lot of text](/images/latex-compact.jpg)

I don't like reading documents with a lot of text and no spaces. By default, paragraphs in LaTeX don't have margins, so I've got into the habit of adding `\\` after every paragraph to add a new line. I didn't know but this practice was the reason of all my LaTeX warning, complaining about *Underfull \\hbox (badness 10000) in paragraph at lines 11--51*. I knew it wasn't the best solution but I didn't take the time to look deeper.

After just a few searches, I found the solution. And it's really straight forward, just add in your main file:
```latex
\setlength{\parskip}{\baselineskip}
```

The main drawback of this solution is that the table of content is using paragraph to print the titles. I ended up with big skip after every line in my table of content. For my use case it was really cool and the result is in fact nicer (because I don't have a lot of chapters and I think it's clearer) but for the majority, it's gonna be awful. I'm sorry I don't have a solution right now but as soon as I need to write another document, I will check a solution for this problem.

![A document with a lot of text but more white spaces. Better!](/images/latex-blank.jpg)

## Automatic compilation work flow

* Save file
* Switch desktop to Okular
* Alt-Tab to switch to Konsole
* Up arrow to get the `make`
* Enter to execute the command
* Alt-Tab to go back to Okular
* Look the result

This was my previous work flow. Not optimized at all, so I looked for some file watcher tool and I found the most basic one: *inotify-tools*. With Arch Linux it's just:
```bash
pacman -S inotify-tools
```

And then I wrote a simple bash script I named `watch.sh` watching recursively for changes in my current folder and running the `make` command each time a file was saved.
```bash
while true; do
  change=$(inotifywait -re close_write,moved_to,create .)
  make && make clean
done
```

And here is my new work flow:

* Save file
* Switch desktop to Okular
* Look the result

Much better!

## GitLab CI

I'm a big fan of GitLab, did you know that you can have as many private repositories as you want in [GitLab.com](https://gitlab.com/users/sign_in)?

GitLab is an open-source alternative to Github, the interface and the features are awesome! GitLab comes with a Continuous Integration tool called GitLab CI. As with Travis, you can write a `.gitlab-ci.yml` and run the tests of your project after each `git push`. What's the point for LaTeX documents? I don't have tests. But I have an artifact. An artifact is a compiled file you want to access after your build (a binary to run your software, a documentation to deploy or a PDF document for example). Here is my `.gitlab-ci.yml` file:
```yml
---

pdflatex:
  script: make
  artifacts:
```

After your first build, you should see a new button in the GitLab UI.

![Access your artifacts right from GitLab](/images/artifacts_button.png)

And browse all the files.

![Your PDF file should be listed here](/images/artifacts_browser.png)

Of course, to use GitLab CI you need to deploy a GitLab CI runner with LaTeX installed. But it's really [with Ansible](/posts/2016-01-24-automate-deployment-with-ansible.html) :-)
```yml
---

- name: be sure Tex Live is present
  apt: name=texlive-full state=latest
```

## A few more tips

Use `\graphicspath{{images/}}` to set the root folder of all your graphics.

If you use a french keyboard like me and manually write all you non-breakable spaces, insert this `\DeclareUnicodeCharacter{00A0}{~}`

If you want a font size bigger than 12pt, `\usepackage{extsizes}` and define your document as a `\documentclass[14pt,a4paper]{extreport}`.

And if you want two words to always be one (no hyphenation breaks), define `\hyphenation{Quantic Telecom}`.
