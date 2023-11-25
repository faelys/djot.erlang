[![Casual Maintenance Intended](https://casuallymaintained.tech/badge.svg)](https://casuallymaintained.tech/)

# Djot.erlang

This a parser for John MacFarlane's lightweight markup language [Djot][],
written in native Erlang.
The included test cases are copied directly from [the reference
implementation][djot.js].

[Djot]: https://github.com/jgm/djot
[djot.js]: https://github.com/jgm/djot.js

This is my first (semi-)serious Erlang project, so the code is probably bad
and full of beginner's mistakes. I welcome all constructive comments,
no matter how harsh.

Even though I'm not sure I'll end up regularly using this library,
I'm committing to maintaining it, fixing discovered bugs, following
upstream evolutions of the specification, and improving the code as my
Erlang skill grows. However I will do so on my free time, possibly after
quite some time, as I have other priorities in my life.

## Known divergences from the reference

The reference tests suite passes 241 out of 249 test cases.
The remaining 8 cases are caused by the following issues:

 - The priority of emphasis over URLs is not implemented
   (see [jgm/jdot#247](https://github.com/jgm/djot/issues/247)).
 - The corner cases of tight vs loose lists don't all match the
   reference implementation
   (see [jgm/djot#249](https://github.com/jgm/djot/issues/249)).
 - The HTML renderer cannot output attributes with empty value.
 - When inline attributes start a paragraph (and are dropped),
   the subsequent spaces are trimmed.
 - The test files `filter.test` and `sourcepos.test` from the original
   test suite are excluded, because javascript filters and source position
   are beyond the scope of this project.
