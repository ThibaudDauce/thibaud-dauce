---
title: Write well-formed documents with LaTeX
image: /images/latex.png
thumbnail: /images/thumbnail-latex.png
---

[LaTeX](http://www.latex-project.org/) is an awesome tool. It allows writing well-formed documents with no efforts. You can manage your references, your bibliography, your acronyms and a lot more.

<!--more-->

![It's really hard to illustrate LaTeX, try to find out why by searching "latex" in Google image :-)](/images/no-image-available.png)

## LaTeX installation

There are two different ways to install LaTeX. First, the basic installation with nothing more than the few main packages. LaTeX is a modular system, each functionality is in a package and you can install them separately. But it's a lot of pain to fix compilation issues when some package is missing and it's really time-consuming. On the other hand you can install the full LaTeX pack. It's the LaTeX compiler with every well-known packages so it's really big (a few Go). The choice of one method over the other is just about the size of your disk.

## Functionalities

### Language specific typography

LaTeX will take care of adding typographical spaces where needed. It's really useful, especially in language like French where you need to add thin non-breaking spaces before a lot of punctuation signs.

### Images

LaTeX can be really useful with images, except the verbosity of the code.
```latex
\begin{figure}
  \centering
  \includegraphics[width=0.5\textwidth]{image.png}
  \caption{Title of the image.}
  \label{label-of-the-image}
\end{figure}
```

To include an image, you need to use the `\includegraphics` command with some options and the name of your file. To provide a title and a label (useful for references), you need to wrap the image in the `figure` environment. Finally, you need to specify the `\centering` command because it's not the default in LaTeX.

One thing to know with LaTeX is you don't have a lot of control about where your image will be rendered: it could be just after the text, just before or two-page away. It's an advantage because you don't have to bother about the placement of your pictures and it forces you to use the good practice of referencing all your pictures in your text (use "figure 2.1" instead of "below" or "right after"). The main problem here is when you have highly visual documents with figures not only here to help the comprehension of your text but also with a real meaning. It can be annoying to switch between pages to read the document. In these cases, LaTeX may not be the most suitable tool to write your report.

### References

One of the most useful feature of LaTeX is the references. It allows you to add labels in your text and use them to link to some parts of your document. LaTeX will replace the command `\ref{a label}` with the according number of the chapter, section or figure. If a label is missing, you can easily find it out looking for `??` in your PDF (or in the ugly LaTeX logs ^^). Of course, every reference is a real link in the PDF to jump to the referring section. If you add a chapter, every reference stay up-to-date without any modification.

### Bibliography and glossary

Who never run into this problem of unused abbreviation or bibliography? LaTeX can help you in these cases. For the bibliography, you just need to specify all your sources and then use them in the text. [BibTeX](https://en.wikibooks.org/wiki/LaTeX/Bibliography_Management) will take care of everything else.
```latex
@Misc{documents-with-latex,
  author = {Thibaud Dauce},
  title = {Write well-formed documents with LaTeX},
  howpublished = {\url{https://thibaud.dauce.fr/posts/2015-12-06-write-well-formed-documents-with-latex.html}},
  month = {December},
  year = {2015},
}
```

This is an example of entry in a BibTex file. You can use this entry in your document with the `\cite` command, for example:
```latex
Thibaud thinks \LaTeX is awesome \cite{documents-with-latex}.
```

If one of the entry of your bibliography is not used anymore in your document, it will not be printed at the end. The same concept works with [the glossary](https://en.wikibooks.org/wiki/LaTeX/Glossary). You just need to define all your acronyms and definitions:
```latex
\newacronym[longplural={Frames per Second}]{fpsLabel}{FPS}{Frame per Second}
```

and use it:
```latex
My camera is recording at 30 \glspl{fpsLabel}.
```

If it's the first use of the acronym, LaTeX will show the full name and after that he'll use the acronym only in the document. A lot of options and configuration are available in the [documentation](https://en.wikibooks.org/wiki/LaTeX/Glossary).

### Syntax highlighting

You can use the Python package `pygments` and the LaTeX package `minted` to add syntax highlighting in LaTeX. To use it, you need to install `pygments`:
```bash
yaourt -S python-pygments
```

Then you can compile your document with the `-shell-escape` flag to allow the LaTeX compiler to run system commands and that's it!

## LaTeX generator

I recently discover there was a [Yeoman](http://yeoman.io/) generator for LaTeX named `generator-latex` [available on Github](https://github.com/LeoColomb/generator-latex). The source code is a bit old and I didn't use it yet on a real project but it seems promising. The creation of the LaTeX boilerplate can be really long and I often copy paste old projects. This generator could be really awesome. It includes `grunt`, a NodeJS build tool to compile the project each time there is a modification on files and a live-reload of the PDF in the browser.

## Conclusion

It was a really just an introduction to all the features available with LaTeX. Write documents with LaTeX is harder than in Markdown or in LibreOffice but it brings a lot of very useful features.

The compilation logs of LaTex are awful but the documentation is really great with a lot of examples and explanations.
