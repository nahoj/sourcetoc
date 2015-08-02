sourcetoc computes a table of contents for a source code file based on
headings found in comments.

~~~
# <TOC>                              >>>>   # <TOC>
# </TOC>                                    # Section 1 .....................  9
                                            #   Section 1.1 ................. 10
...                                         # Section 2 ..................... 14
                                            # </TOC>
# == Section 1 ==
# === Section 1.1 ===                       ...

...                                         # == Section 1 ==
                                            # === Section 1.1 ===
# == Section 2
                                            ...
...
                                            # == Section 2

                                            ...
~~~

sourcetoc supports three styles of headings (wiki, atx (markdown)
and html) and a number of programming languages (no strong integration).


## Install

Requirements: OCaml ≥ 3.12

To install once do `sudo make install`.

To get sourcetoc with updates you can do:

~~~
git clone ...
cd sourcetoc
sudo make lninstall  # creates a symlink
~~~

And then to update:

~~~
cd sourcetoc
git pull
~~~


## Usage

Basic usage:

    sourcetoc example.py

This will modify `example.py` in place after copying it to
`example.py~`.

In a file, sourcetoc considers one-line comments that start at the
beginning of a line and contain either `<TOC>` or `</TOC>` or a
heading. sourcetoc works line by line and does not ignore comment
lines that might have a different meaning because of context
(e.g. inside a multi-line string).

~~~java
/* == Valid == */

/* == Invalid (multi-line) ==
 */

 // == Invalid (space) ==

String s = "
// == Valid (context ignored) ==
";
~~~

The table of contents is generated from the list of headings and is
inserted between the `<TOC>` and `</TOC>` lines. These two must be
unique and appear in this order. Anything that exists between them
(typically a previous version of the TOC) is erased. The rest of the
file is unchanged.

By default sourcetoc expects wiki-style headings (`== ... ==`). It
also supports atx/Markdown headings (`## ... ##`) and HTML headings
(`<h2>...</h2>`). See `--heading-style`.
