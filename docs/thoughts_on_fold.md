Joy's Fold is in the opposite direction from every other functional
language. I'm trying to determine why that is.

Joy's fold is defined as a left fold, not a right fold. This means
that it is not general purpose enough to build the other standard
functional combinators.

`fold` in Joy is defined as `swapd step`. Using some rewriting, let's
see how this expands in a small program that adds some chars together
into a string.

`["b" "c" "d"] "a" [concat] fold`

`["b" "c" "d"] "a" [concat] swapd step` (expanded `fold`)

`[concat] ["b" "c" "d"] "a" step` (expanded `swapd`)

The `step` combinator peels off elements from the head of the array
and executes the contents of the quotation. I'm hiding the `["b" "c"
"d"]` array, it's not not available directly to your quotation, rather
it's elements are peeled off the head (the left side) and put on the
stack. the results of concat plus the next element are what's on top
of the stack for the quotation to work with.

Back to our rewriting

`"b" "a" concat`

`"ab" "c" concat`

`"abc" "d" concat`

the final result left on the stack is `"abcd"`
