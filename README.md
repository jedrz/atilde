# atilde

A minor mode for automagical insertion of tildes after or between some words in
*LaTeX* buffers.

## Usage

To quickly start up put this file somewhere in your `load-path` and add the
following lines in your .emacs:

    (require 'atilde)
    (add-hook 'latex-mode-hook 'atilde-mode)

## Description

With `atilde-mode` enabled, pressing the *space* key insert a hard space (tilde
character) after Polish vowels or two letter words. Tildes are also inserted
between some words like: `2014~r.`.

This package is indented to use in *LaTeX* buffers since some environments
(such as `displaymath` or `verb`) surrounding the cursor are ignored and tildes
are never inserted automatically.

All missing tildes can be inserted in entire buffer with `atilde-query-replace`
command.

Positions of missing tildes are marked by default in red color.

## Customization

Do `M-x customize-group atilde RET` to open a customize buffer and change some 
*atilde* variables.

### Adding new words to insert tildes after or between

To add new words after which tildes should be inserted you have to add a new
regexp to `atilde-after-regexps` list. For example to have tildes be inserted
after every single, two and three letter words u can do:

    (setq atilde-after-regexps '("\\w\\{1,3\\}")')

On the other hand, to add a new pair of words between which tildes can be
inserted, `atilde-between-regexps` has to be modified, which is a list of cons
cells. Each cons cell consists of a regexp matching the preceding part of
expression and the following one, between which tildes are allowed to be
inserted.

### Adding an ignored environment

To add a new environment as ignored one, add a cons cell to the
`atilde-ignored-envs` list. For example to ban insertion of tildes between
`\begin{myenv}` and `\end{myenv}` do:

    (add-to-list 'atilde-ignored-envs '("\\begin{myenv}" . "\\end{myenv}"))

### Customize highlight positions of missing tildes

To disable this feature set `atilde-highlight-missing-tildes` variable to
`nil`. To change the color do:

    M-x customize-face atilde-missing-tilde RET

## License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.
