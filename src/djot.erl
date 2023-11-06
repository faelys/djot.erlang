% Copyright (C) 2023 Natasha Kerensikova
%
% Permission is hereby granted, free of charge, to any person obtaining
% a copy of this software and associated documentation files (the
% "Software"), to deal in the Software without restriction, including
% without limitation the rights to use, copy, modify, merge, publish,
% distribute, sublicense, and/or sell copies of the Software, and to
% permit persons to whom the Software is furnished to do so, subject to
% the following conditions:
%
% The above copyright notice and this permission notice shall be included
% in all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-module(djot).
-export([parse/1, start_state/0, parse/2, finalize/1, walk_ast/4, update_ast/4, finish_doc/1]).

-include_lib("stdlib/include/assert.hrl").

-define(SPACE(C), (C =:= 9 orelse C =:= 10 orelse C =:= 13 orelse C =:= 32)).
-define(PUNCTUATION(C), ((C >= $! andalso C =< $/)
                  orelse (C >= $: andalso C =< $@)
                  orelse (C >= $[ andalso C =< $`)
                  orelse (C >= ${ andalso C =< $~))).
-define(ALPHA(C), ((C >= $A andalso C =< $Z)
            orelse (C >= $a andalso C =< $z))).
-define(NUM(C), (C >= $0 andalso C =< $9)).
-define(ALPHANUM(C), (?ALPHA(C) orelse ?NUM(C))).

-define(IS_BLOCK(Element), (Element =:= block_quote
                     orelse Element =:= caption
                     orelse Element =:= code_block
                     orelse Element =:= definition
                     orelse Element =:= definition_list
                     orelse Element =:= definition_list_item
                     orelse Element =:= doc
                     orelse Element =:= fenced_div
                     orelse Element =:= footnote
                     orelse Element =:= heading
                     orelse Element =:= reference_definition
                     orelse Element =:= list
                     orelse Element =:= list_item
                     orelse Element =:= para
                     orelse Element =:= reference
                     orelse Element =:= table
                     orelse Element =:= term
                     orelse Element =:= thematic_break)).

-define(CONTAINS_BLOCK(Element), (Element =:= block_quote
                           orelse Element =:= definition
                           orelse Element =:= definition_list
                           orelse Element =:= definition_list_item
                           orelse Element =:= doc
                           orelse Element =:= fenced_div
                           orelse Element =:= footnote
                           orelse Element =:= list
                           orelse Element =:= list_item)).

-define(CONTAINS_INLINE(Element), (Element =:= caption
                            orelse Element =:= cell
                            orelse Element =:= heading
                            orelse Element =:= para
                            orelse Element =:= term)).

-define(CONTAINS_TEXT(Element), (Element =:= code_block
                          orelse Element =:= display_math
                          orelse Element =:= footnote_ref
                          orelse Element =:= inline_math
                          orelse Element =:= raw_inline
                          orelse Element =:= reference_definition
                          orelse Element =:= reference
                          orelse Element =:= smart_punctuation
                          orelse Element =:= symbol
                          orelse Element =:= text
                          orelse Element =:= verbatim)).

% CONTAINS_OTHER: table, row, thematic_break

-define(SPAN_MARK(Mark), (Mark =:= $_ orelse Mark =:= $* orelse Mark =:= $^
                   orelse Mark =:= $~ orelse Mark =:= $= orelse Mark =:= $-
                   orelse Mark =:= $+ orelse Mark =:= $" orelse Mark =:= $')).

-define(LIGHT_SPAN_MARK(Mark), (Mark =:= $_ orelse Mark =:= $*
                         orelse Mark =:= $^ orelse Mark =:= $~
                         orelse Mark =:= $" orelse Mark =:= $')).

-define(NONQT_LIGHT_SPAN_MARK(Mark), (Mark =:= $_ orelse Mark =:= $*
                               orelse Mark =:= $^ orelse Mark =:= $~)).

-define(INLINE_STACK(Stack),
  [{_Block_Element, _Block_Attribute, Stack} | _Block_Stack]).
-define(INLINE_TOP(Element), ?INLINE_STACK([Element | _Inline_Stack])).
-define(INLINE_TOP(First, Second),
  ?INLINE_STACK([First, Second | _Inline_Stack])).

span_element($_) -> emphasis;
span_element($*) -> strong;
span_element($^) -> superscript;
span_element($~) -> subscript;
span_element($=) -> mark;
span_element($-) -> del;
span_element($+) -> add;
span_element($") -> double_quoted;
span_element($') -> single_quoted.

last_word([]) -> {[], []};
last_word(Reverse_Text = [Head | _]) when ?SPACE(Head) -> {Reverse_Text, []};
last_word(Reverse_Text) -> last_word(Reverse_Text, []).
last_word([], Word) -> {[], Word};
last_word(Reverse_Prefix = [Head | _], Word)
  when ?SPACE(Head) -> {Reverse_Prefix, Word};
last_word([Head | Tail], Word) -> last_word(Tail, [Head | Word]).

list_item_mark([Marker, Space | Tail])
  when ?SPACE(Space),
       Marker =:= $* orelse Marker =:= $+
         orelse Marker =:= $- orelse Marker =:= $: ->
  {[Marker], 2, 0, Tail};
list_item_mark([$( | Tail]) ->
  case list_item_mark(Tail, 0, 0, "") of
    {Mark = [_, $)], Level, Value, Rest} ->
      {[$( | Mark], Level + 1, Value, Rest};
    _ -> false
  end;
list_item_mark(Input) -> list_item_mark(Input, 0, 0, "").

roman_value($i) -> 1;
roman_value($v) -> 5;
roman_value($x) -> 10;
roman_value($l) -> 50;
roman_value($c) -> 100;
roman_value($d) -> 500;
roman_value($m) -> 1000;
roman_value($I) -> 1;
roman_value($V) -> 5;
roman_value($X) -> 10;
roman_value($L) -> 50;
roman_value($C) -> 100;
roman_value($D) -> 500;
roman_value($M) -> 1000.

list_item_mark([Head | Tail], 0, 0, "")
  when Head >= $0 andalso Head =< $9 ->
  list_item_mark(Tail, 1, Head - $0, "1");
list_item_mark([Head | Tail], 0, 0, "")
  when Head =:= $i orelse Head =:= $v orelse Head =:= $x orelse Head =:= $l
                   orelse Head =:= $c orelse Head =:= $d orelse Head =:= $m ->
  list_item_mark(Tail, 1, Head, "x");
list_item_mark([Head | Tail], 0, 0, "")
  when Head =:= $I orelse Head =:= $V orelse Head =:= $X orelse Head =:= $L
                   orelse Head =:= $C orelse Head =:= $D orelse Head =:= $M ->
  list_item_mark(Tail, 1, Head, "X");
list_item_mark([Head | Tail], 0, 0, "")
  when Head >= $a andalso Head =< $z ->
  list_item_mark(Tail, 1, Head - $a + 1, "a");
list_item_mark([Head | Tail], 0, 0, "")
  when Head >= $A andalso Head =< $Z ->
  list_item_mark(Tail, 1, Head - $A + 1, "A");
list_item_mark([Head | Tail], Level, Value, "1")
  when Head >= $0 andalso Head =< $9 ->
  list_item_mark(Tail, Level + 1, Value * 10 + Head - $0, "1");
list_item_mark(Input = [Head | _], Level, Value, "x")
  when Head =:= $i orelse Head =:= $v orelse Head =:= $x orelse Head =:= $l
                   orelse Head =:= $c orelse Head =:= $d orelse Head =:= $m ->
  list_item_mark(Input, Level, roman_value(Value), "i");
list_item_mark(Input = [Head | _], Level, Value, "X")
  when Head =:= $I orelse Head =:= $V orelse Head =:= $X orelse Head =:= $L
                   orelse Head =:= $C orelse Head =:= $D orelse Head =:= $M ->
  list_item_mark(Input, Level, roman_value(Value), "I");
list_item_mark([Head | Tail], Level, Value, "i")
  when Head =:= $i orelse Head =:= $v orelse Head =:= $x orelse Head =:= $l
                   orelse Head =:= $c orelse Head =:= $d orelse Head =:= $m ->
  Digit_Value = roman_value(Head),
  New_Value = if Digit_Value > Value -> Digit_Value - Value;
                 true -> Value + Digit_Value
              end,
  list_item_mark(Tail, Level + 1, New_Value, "i");
list_item_mark([Head | Tail], Level, Value, "I")
  when Head =:= $I orelse Head =:= $V orelse Head =:= $X orelse Head =:= $L
                   orelse Head =:= $C orelse Head =:= $D orelse Head =:= $M ->
  Digit_Value = roman_value(Head),
  New_Value = if Digit_Value > Value -> Digit_Value - Value;
                 true -> Value + Digit_Value
              end,
  list_item_mark(Tail, Level + 1, New_Value, "I");
list_item_mark([Head | Tail], Level, Value, [Symbol])
  when Head =:= $) orelse Head =:= $. ->
  list_item_mark(Tail, Level + 1, Value, [Symbol, Head]);
list_item_mark([Head | Tail], Level, Value, Mark = [_, _]) when ?SPACE(Head) ->
  {Mark, Level + 1, Value, Tail};
list_item_mark(_, _, _, _) -> false.

list_item_mark_match(Mark, Value, Mark) -> {Mark, Value};
list_item_mark_match([$x, Symbol], Value, [$i, Symbol]) ->
  {[$i, Symbol], roman_value(Value)};
list_item_mark_match([$x, Symbol], Value, [$a, Symbol]) ->
  {[$a, Symbol], Value - $a + 1};
list_item_mark_match([$X, Symbol], Value, [$I, Symbol]) ->
  {[$I, Symbol], roman_value(Value)};
list_item_mark_match([$X, Symbol], Value, [$A, Symbol]) ->
  {[$A, Symbol], Value - $A + 1};
list_item_mark_match([$i, Symbol], Value, [$x, Symbol]) ->
  {[$i, Symbol], Value};
list_item_mark_match([$a, Symbol], Value, [$x, Symbol]) ->
  {[$a, Symbol], Value};
list_item_mark_match([$I, Symbol], Value, [$X, Symbol]) ->
  {[$I, Symbol], Value};
list_item_mark_match([$A, Symbol], Value, [$X, Symbol]) ->
  {[$A, Symbol], Value};
list_item_mark_match("(x)", Value, "(i)") -> {"(i)", roman_value(Value)};
list_item_mark_match("(x)", Value, "(a)") -> {"(a)", Value - $a + 1};
list_item_mark_match("(X)", Value, "(I)") -> {"(I)", roman_value(Value)};
list_item_mark_match("(X)", Value, "(A)") -> {"(A)", Value - $A + 1};
list_item_mark_match("(i)", Value, "(x)") -> {"(i)", Value};
list_item_mark_match("(a)", Value, "(x)") -> {"(a)", Value};
list_item_mark_match("(I)", Value, "(X)") -> {"(I)", Value};
list_item_mark_match("(A)", Value, "(X)") -> {"(A)", Value};
list_item_mark_match(_, _, _) -> false.

list_item_match(Input, Mark, Start_Value) ->
  case list_item_mark(Input) of
    false -> false;
    {New_Mark, Level, Value, Tail} ->
       case list_item_mark_match(Mark, Start_Value, New_Mark) of
         false -> {false, New_Mark, Level, Value, Tail};
         {Merged_Mark, Merged_Start} ->
           {true, Merged_Mark, Merged_Start, Level, Value, Tail}
       end
  end.

is_footnote([]) -> false;
is_footnote([10 | _]) -> false;
is_footnote([$], $:, Space | _]) when ?SPACE(Space) -> true;
is_footnote([$] | _]) -> false;
is_footnote([_ | Tail]) -> is_footnote(Tail).

is_thematic_break([], Count) when Count >= 3 -> [];
is_thematic_break([10 | Tail], Count) when Count >= 3 -> Tail;
is_thematic_break([Head | Tail], Count) when Head =:= 32 orelse Head =:= 9 ->
  is_thematic_break(Tail, Count);
is_thematic_break([Head | Tail], Count) when Head =:= $* orelse Head =:= $- ->
  is_thematic_break(Tail, Count + 1);
is_thematic_break(_, _) -> false.

thematic_break_or_list_mark(Input) ->
  case is_thematic_break(Input, 0) of
    false -> list_item_mark(Input);
    Tail -> {thematic_break, Tail}
  end.

stack_has_list([]) -> false;
stack_has_list([{list, _, _} | _]) -> true;
stack_has_list([_ | Tail]) -> stack_has_list(Tail).

trim_leading_spaces([Head | Tail]) when ?SPACE(Head) ->
  trim_leading_spaces(Tail);
trim_leading_spaces(String) -> String.

trim_trailing_spaces(String) ->
  lists:reverse(trim_leading_spaces(lists:reverse(String))).

trim_top_spaces([{soft_break} | Tail]) ->
  trim_top_spaces(Tail);
trim_top_spaces([{text, Text} | Tail]) ->
  case trim_leading_spaces(Text) of
    [] -> trim_top_spaces(Tail);
    Trimmed_Text -> [{text, Trimmed_Text} | Tail]
  end;
trim_top_spaces(Inline_Stack) -> Inline_Stack.

trim_bottom_spaces(Inline_Stack) ->
  trim_bottom_spaces(lists:reverse(Inline_Stack), reversed).
trim_bottom_spaces([{soft_break} | Tail], reversed) ->
  trim_bottom_spaces(Tail, reversed);
trim_bottom_spaces([{text, Text} | Tail], reversed) ->
  case trim_trailing_spaces(Text) of
    [] -> trim_bottom_spaces(Tail, reversed);
    Trimmed_Text -> lists:reverse([{text, Trimmed_Text} | Tail])
  end;
trim_bottom_spaces(Inline_Stack, reversed) -> lists:reverse(Inline_Stack).

trim_inline_stack(Inline_Stack) ->
  trim_top_spaces(trim_bottom_spaces(Inline_Stack)).

spaces_to_eol([10 | Tail]) -> Tail;
spaces_to_eol([Head | Tail]) when ?SPACE(Head) -> spaces_to_eol(Tail);
spaces_to_eol(_) -> false.

bracketize([]) ->
  [{text, "]["}];
bracketize([{text, Text}]) ->
  [{text, "]" ++ lists:reverse("[" ++ Text)}];
bracketize([{text, Prefix} | Suffix]) ->
  bracketize({text, lists:reverse("[" ++ Prefix)}, lists:reverse(Suffix));
bracketize(Contents) -> bracketize({text, "["}, lists:reverse(Contents)).
bracketize(Prefix, [{text, Suffix} | Middle]) ->
  [{text, "]" ++ lists:reverse(Suffix)},
   {inline, lists:reverse(Middle)}, Prefix];
bracketize(Prefix, Middle) ->
  [{text, "]"}, {inline, lists:reverse(Middle)}, Prefix].

finalize_attributes([], Acc) -> lists:reverse(Acc);
finalize_attributes([{fence, _} | Tail], Acc) ->
  finalize_attributes(Tail, Acc);
finalize_attributes([{fence, _, _, _} | Tail], Acc) ->
  finalize_attributes(Tail, Acc);
finalize_attributes([{indent, _} | Tail], Acc) ->
  finalize_attributes(Tail, Acc);
finalize_attributes([{marker, [$X, Closer]}, {start, Value} | Tail], Acc) ->
  finalize_attributes(Tail,
                      [{start, roman_value(Value)}, {marker, [$I, Closer]}
                       | Acc]);
finalize_attributes([{marker, "(X)"}, {start, Value} | Tail], Acc) ->
  finalize_attributes(Tail,
                      [{start, roman_value(Value)}, {marker, "(I)"} | Acc]);
finalize_attributes([{marker, [$x, Closer]}, {start, Value} | Tail], Acc) ->
  finalize_attributes(Tail,
                      [{start, roman_value(Value)}, {marker, [$i, Closer]}
                       | Acc]);
finalize_attributes([{marker, "(x)"}, {start, Value} | Tail], Acc) ->
  finalize_attributes(Tail,
                      [{start, roman_value(Value)}, {marker, "(i)"} | Acc]);
finalize_attributes([Head | Tail], Acc) ->
  finalize_attributes(Tail, [Head | Acc]).

orphan_opener({opener, "{\""}) ->
  {smart_punctuation, [{"type", "left_double_quote"}], "\""};
orphan_opener({opener, "\""}) ->
  {smart_punctuation, [{"type", "left_double_quote"}], "\""};
orphan_opener({opener, "{'"}) ->
  {smart_punctuation, [{"type", "left_single_quote"}], "'"};
orphan_opener({opener, "'"}) ->
  {smart_punctuation, [{"type", "right_single_quote"}], "'"};
orphan_opener({opener, Text}) ->
  {text, lists:reverse(Text)};
orphan_opener({opener, _, Text}) ->
  {text, lists:reverse(Text)}.

em_dash(Acc) -> [{smart_punctuation, [{"type", "em_dash"}], "---"} | Acc].
en_dash(Acc) -> [{smart_punctuation, [{"type", "en_dash"}], "--"} | Acc].

em_dash(0, Acc) -> Acc;
em_dash(N, Acc) -> em_dash(N - 1, em_dash(Acc)).
en_dash(0, Acc) -> Acc;
en_dash(N, Acc) -> en_dash(N - 1, en_dash(Acc)).

smart_dashes(N) when N rem 3 == 0 -> em_dash(N div 3, []);
smart_dashes(N) when N rem 2 == 0 -> en_dash(N div 2, []);
smart_dashes(N) when N rem 3 == 2 -> em_dash(N div 3, en_dash(1, []));
smart_dashes(N) when N rem 3 == 1 -> em_dash(N div 3 - 1, en_dash(2, [])).

close_blocks(Input, Next,
             [{Element, Attributes,
               [{table, Table_Attributes, [{caption, _, _} | Rows]}
                | Contents]}
              | Block_Stack],
             [Block = {caption, _, _}],
             []) ->
  parse(Input,
        Next ++ [{Element, Attributes,
                  [{table, Table_Attributes, [Block | Rows]} | Contents]}
                 | Block_Stack]);
close_blocks(Input, Next,
             [{Element, Attributes,
               [{table, Table_Attributes, Rows} | Contents]}
              | Block_Stack],
             [Block = {caption, _, _}],
             []) ->
  parse(Input,
        Next ++ [{Element, Attributes,
                  [{table, Table_Attributes, [Block | Rows]} | Contents]}
                 | Block_Stack]);
close_blocks(Input, Next, Block_Stack, [{caption, _, _}], []) ->
  parse(Input, Next ++ Block_Stack);
close_blocks(Input, Next,
             [{Element, Attributes, Contents} | Block_Stack],
             [Block],
             []) ->
  parse(Input,
        Next ++ [{Element, Attributes, [Block | Contents]} | Block_Stack]);
close_blocks(Input, Next, Block_Stack,
             [Block, {Element, Attributes, Contents} | To_Close],
             []) ->
  close_blocks(Input, Next, Block_Stack,
               [{Element, Attributes, [Block | Contents]} | To_Close]);
close_blocks(_, _, _, _, [{attributes, _, {Input, Stack}} | _]) ->
  parse(Input, Stack);
close_blocks(_, _, _, _, [{autolink, _, _, Input, Stack} | _]) ->
  parse(Input, Stack);
close_blocks(Input, Next, Block_Stack,
             [{Block_Element, Block_Attributes, Block_Contents} | To_Close],
             [{inline, Contents} | Inline_Stack]) ->
  close_blocks(Input, Next, Block_Stack,
               [{Block_Element, Block_Attributes, Contents ++ Block_Contents}
                | To_Close],
               Inline_Stack);
close_blocks(Input, Next, Block_Stack, To_Close,
             [Opener = {opener, _} | Inline_Stack]) ->
  close_blocks(Input, Next, Block_Stack, To_Close,
               [orphan_opener(Opener) | Inline_Stack]);
close_blocks(Input, Next, Block_Stack, To_Close,
             [Opener = {opener, _, _} | Inline_Stack]) ->
  close_blocks(Input, Next, Block_Stack, To_Close,
               [orphan_opener(Opener) | Inline_Stack]);
close_blocks(_, _, _, _, [{raw_inline, _, _, {Input, Stack}} | _]) ->
  parse(Input, Stack);
close_blocks(_, _, _, _,
             [{ref, _, _, _, Contents, {Input, ?INLINE_STACK(Stack)}} | _]) ->
  parse(Input, ?INLINE_STACK(bracketize(Contents) ++ Stack));
close_blocks(_, _, _, _, [{symbol, _, Input, Stack} | _]) ->
  parse(Input, Stack);
close_blocks(Input, Next, Block_Stack, To_Close,
             [Text = {text, _}, Opener = {opener, _} | Inline_Stack]) ->
  close_blocks(Input, Next, Block_Stack, To_Close,
               [Text, orphan_opener(Opener) | Inline_Stack]);
close_blocks(Input, Next, Block_Stack, To_Close,
             [Text = {text, _}, Opener = {opener, _, _} | Inline_Stack]) ->
  close_blocks(Input, Next, Block_Stack, To_Close,
               [Text, orphan_opener(Opener) | Inline_Stack]);
close_blocks(Input, Next, Block_Stack, To_Close,
             [{text, Text_1}, {text, Text_2} | Inline_Stack]) ->
  close_blocks(Input, Next, Block_Stack, To_Close,
               [{text, Text_1 ++ Text_2} | Inline_Stack]);
close_blocks(Input, Next, Block_Stack,
             [{Block_Element, Block_Attributes, Block_Contents} | To_Close],
             [{text, Text} | Inline_Stack]) ->
  close_blocks(Input, Next, Block_Stack,
               [{Block_Element, Block_Attributes,
                  [{text, lists:reverse(Text)} | Block_Contents]}
                | To_Close],
               Inline_Stack);
close_blocks(Input, Next, Block_Stack,
             [{Block_Element, Block_Attributes, Block_Contents} | To_Close],
             [{verbatim, _, opener, _}, {opener, "$"} | Inline_Stack]) ->
  close_blocks(Input, Next, Block_Stack,
               [{Block_Element, Block_Attributes,
                  [{inline_math, []} | Block_Contents]}
                | To_Close],
               Inline_Stack);
close_blocks(Input, Next, Block_Stack,
             [{Block_Element, Block_Attributes, Block_Contents} | To_Close],
             [{verbatim, _, opener, _}, {opener, "$$"} | Inline_Stack]) ->
  close_blocks(Input, Next, Block_Stack,
               [{Block_Element, Block_Attributes,
                  [{display_math, []} | Block_Contents]}
                | To_Close],
               Inline_Stack);
close_blocks(Input, Next, Block_Stack,
             [{Block_Element, Block_Attributes, Block_Contents} | To_Close],
             [{verbatim, _, opener, _} | Inline_Stack]) ->
  close_blocks(Input, Next, Block_Stack,
               [{Block_Element, Block_Attributes,
                  [{verbatim, []} | Block_Contents]}
                | To_Close],
               Inline_Stack);
close_blocks(Input, Next, Block_Stack,
             [{Block_Element, Block_Attributes, Block_Contents} | To_Close],
             [{verbatim, _, Text, _}, {opener, "$"} | Inline_Stack]) ->
  close_blocks(Input, Next, Block_Stack,
               [{Block_Element, Block_Attributes,
                  [{inline_math, lists:reverse(Text)} | Block_Contents]}
                | To_Close],
               Inline_Stack);
close_blocks(Input, Next, Block_Stack,
             [{Block_Element, Block_Attributes, Block_Contents} | To_Close],
             [{verbatim, _, Text, _}, {opener, "$$"} | Inline_Stack]) ->
  close_blocks(Input, Next, Block_Stack,
               [{Block_Element, Block_Attributes,
                  [{display_math, lists:reverse(Text)} | Block_Contents]}
                | To_Close],
               Inline_Stack);
close_blocks(Input, Next, Block_Stack,
             [{Block_Element, Block_Attributes, Block_Contents} | To_Close],
             [{verbatim, _, Text, _} | Inline_Stack]) ->
  close_blocks(Input, Next, Block_Stack,
               [{Block_Element, Block_Attributes,
                  [{verbatim, lists:reverse(Text)} | Block_Contents]}
                | To_Close],
               Inline_Stack);
close_blocks(Input, Next, Block_Stack,
             [{Block_Element, Block_Attributes, Block_Contents} | To_Close],
             [Element | Inline_Stack]) ->
  close_blocks(Input, Next, Block_Stack,
               [{Block_Element, Block_Attributes, [Element | Block_Contents]}
                | To_Close],
               Inline_Stack).

close_blocks(Input, Next, Stack, []) ->
  parse(Input, Next ++ Stack);
close_blocks(Input, Next, Stack, [{Element, Attributes, Contents} | To_Close])
  when ?CONTAINS_INLINE(Element) ->
  close_blocks(Input, Next, Stack,
               [{Element, finalize_attributes(Attributes, []), []} | To_Close],
               trim_inline_stack(Contents));
close_blocks(_, _, _, [{block_attributes, _, Attribute_Stack} | _]) ->
  [{attributes, _, {Input, Stack}} | _] = lists:reverse(Attribute_Stack),
  parse(Input, Stack);
close_blocks(Input, Next,
             [{Block_Element, Block_Attributes, Block_Contents} | Stack],
             [{Element, Attributes, Contents}]) ->
  Next_Block = {Element,
                finalize_attributes(Attributes, []),
                lists:reverse(Contents)},
  parse(Input, Next ++
               [{Block_Element,
                 Block_Attributes,
                 [Next_Block | Block_Contents]}
                | Stack]);
close_blocks(Input, Next, Stack,
             [{Element, Attributes, Contents},
              {Block_Element, Block_Attributes, Block_Contents} | To_Close]) ->
  Next_Block = {Element,
                finalize_attributes(Attributes, []),
                lists:reverse(Contents)},
  close_blocks(Input, Next, Stack,
               [{Block_Element,
                 Block_Attributes,
                 [Next_Block | Block_Contents]}
                | To_Close]).

maybe_close_fence([10 | Tail], _, 0, _,
                  [{prefix, _, _, To_Match}, Block | Stack]) ->
  close_blocks(Tail,
               [{newline, []}], Stack, lists:reverse([Block | To_Match]));
maybe_close_fence([Head | Tail], Mark, 0, Orig_Input, Stack)
  when Head =:= Mark orelse Head =:= 32 orelse Head =:= 9 ->
  maybe_close_fence(Tail, Mark, 0, Orig_Input, Stack);
maybe_close_fence([Mark | Tail], Mark, Needed, Orig_Input, Stack) ->
  maybe_close_fence(Tail, Mark, Needed - 1, Orig_Input, Stack);
maybe_close_fence(_, _, _, Orig_Input, Stack) ->
  parse(Orig_Input, Stack).

maybe_close_fence_after_blanks([Head | Tail], Mark, Needed, Orig_Input, Stack)
  when Head =:= 32 orelse Head =:= 9 ->
  maybe_close_fence_after_blanks(Tail, Mark, Needed, Orig_Input, Stack);
maybe_close_fence_after_blanks(Input, Mark, Needed, Orig_Input, Stack) ->
  maybe_close_fence(Input, Mark, Needed, Orig_Input, Stack).

continue_reference_definition([10 | Tail], Text, _, _, _,
                              {reference_definition, Attributes, Target},
                              Stack) ->
  parse(Tail,
        [{newline, []},
         {reference_definition, Attributes, Text ++ Target}
         | Stack]);
continue_reference_definition([Head | _], _, Input, Indent_Level, Attributes,
                              Block, Stack)
  when ?SPACE(Head) ->
  close_blocks(Input,
               [{prefix, Indent_Level, Attributes, []}],
               Stack,
               [Block]);
continue_reference_definition([Head | Tail], Seen, Input, Indent_Level,
                              Attributes, Block, Stack) ->
  continue_reference_definition(Tail, [Head | Seen], Input, Indent_Level,
                                Attributes, Block, Stack).

reset_align([], Acc) -> lists:reverse(Acc);
reset_align([{align, _} | Tail], Acc) -> reset_align(Tail, Acc);
reset_align([Head | Tail], Acc) -> reset_align(Tail, [Head | Acc]).

align_cells([], _, _, Acc) -> lists:reverse(Acc);
align_cells([{cell, Attributes, Contents} | Cells], [], Extra, Acc) ->
  New_Attributes = Extra ++ reset_align(Attributes, []),
  align_cells(Cells, [], Extra, [{cell, New_Attributes, Contents} | Acc]);
align_cells([{cell, Attributes, Contents} | Cells], [Alignment | Rest],
            Extra, Acc) ->
  New_Attributes = Extra ++ [{align, Alignment} | reset_align(Attributes, [])],
  align_cells(Cells, Rest, Extra, [{cell, New_Attributes, Contents} | Acc]).


push_text([{text, Value} | Stack], Text) ->
  [{text, lists:reverse(Text) ++ Value} | Stack];
push_text(Stack, Text) ->
  [{text, lists:reverse(Text)} | Stack].

push_char([{text, Value} | Stack], C) ->
  [{text, [C | Value]} | Stack];
push_char(Stack, C) ->
  [{text, [C]} | Stack].

abort_openers([], Acc) -> Acc;
abort_openers([{text, Value} | Tail], Acc) ->
  abort_openers(Tail, push_text(Acc, Value));
abort_openers([Opener = {opener, _} | Tail], Acc) ->
  abort_openers([orphan_opener(Opener) | Tail], Acc);
abort_openers([Opener = {opener, _, _} | Tail], Acc) ->
  abort_openers([orphan_opener(Opener) | Tail], Acc);
abort_openers([Head | Tail], Acc) ->
  abort_openers(Tail, [Head | Acc]).

close(Rest, [{opener, Mark} | Tail], Seen,
      Mark, Element, _, ?INLINE_STACK([])) ->
  parse(Rest, ?INLINE_STACK([{Element,
                              abort_openers(lists:reverse(Seen), [])}
                             | Tail]));
close(Rest, [Head | Tail], Seen, Mark, Element, Alt, Blocks) ->
  close(Rest, Tail, [Head | Seen], Mark, Element, Alt, Blocks);
close(Rest, [], Seen, _, _, Alt, ?INLINE_STACK([])) ->
  case orphan_opener({opener, Alt}) of
    {text, Text} ->
      parse(Rest, ?INLINE_STACK(push_text(lists:reverse(Seen),
                                          lists:reverse(Text))));
    Element ->
      parse(Rest, ?INLINE_STACK([Element | lists:reverse(Seen)]))
  end.

close_or_open(Rest, [], Seen, Mark, _Element, ?INLINE_STACK([])) ->
  parse(Rest, ?INLINE_STACK([{opener, Mark} | lists:reverse(Seen)]));
close_or_open(Rest, [{opener, Mark} | Tail], Seen,
              Mark, Element, ?INLINE_STACK([])) ->
  parse(Rest, ?INLINE_STACK([{Element,
                              abort_openers(lists:reverse(Seen), [])}
                             | Tail]));
close_or_open(Rest, [Head | Tail], Seen, Mark, Element, Blocks) ->
  close_or_open(Rest, Tail, [Head | Seen], Mark, Element, Blocks).

%% Parse Termination

parse([], State) -> State;

%% Block continuation or termination

parse(Input, [{newline, Attributes} | Stack]) ->
  [Doc = {doc, _, _} | To_Match] = lists:reverse(Stack),
  parse(Input, [{prefix, 0, Attributes, To_Match}, Doc]);

parse(Input, [{prefix, Level, Attributes,
                       [Block = {Element, _, _} | To_Match]}
              | Stack])
  when Element =:= doc ->
  parse(Input, [{prefix, Level, Attributes, To_Match}, Block | Stack]);

parse(Input,
      [{prefix, Level, Attributes,
        [Block = {code_block, [{fence, Target, Mark, Max_Level} | _], _}]}
       | Stack])
  when Level >= Max_Level ->
  maybe_close_fence_after_blanks(Input, Mark, Target, Input,
                    [{prefix, Level, Attributes, []}, Block | Stack]);
parse(Input,
      [{prefix, Level, _, []}
       | Stack = [{code_block, [{fence, _, _, Max_Level} | _], _} | _]])
  when Level >= Max_Level ->
  parse(Input, Stack);

parse([32 | Tail], [{prefix, Level, Attributes, To_Match} | Stack]) ->
  parse(Tail, [{prefix, Level + 1, Attributes, To_Match} | Stack]);
parse([9 | Tail], [{prefix, Level, Attributes, To_Match} | Stack]) ->
  parse(Tail, [{prefix, Level + 1, Attributes, To_Match} | Stack]);

% continuation through empty lines
parse(Input = [10 | _],
      [{prefix, Level, Attributes,
        [{list, [{marker, Marker}, {start, Start}, {tight, true}
                 | List_Attributes], List_Contents}
         | To_Match]}
       | Stack]) ->
  Tight = case stack_has_list(To_Match) of true  -> true;
                                           false -> maybe end,
  parse(Input,
        [{prefix, Level, Attributes, To_Match},
         {list, [{marker, Marker}, {start, Start}, {tight, Tight}
                 | List_Attributes], List_Contents}
         | Stack]);
parse(Input = [10 | _],
      [{prefix, Level, Attributes, [Block = {Element, _, _} | To_Match]}
       | Stack])
  when        Element =:= fenced_div orelse Element =:= footnote
       orelse Element =:= list_item  orelse Element =:= list ->
  parse(Input,
        [{prefix, Level, Attributes, To_Match}, Block | Stack]);
parse([10 | Tail], [{prefix, _, _, To_Match} | Stack]) ->
  close_blocks(Tail, [{newline, []}], Stack, lists:reverse(To_Match));

% block attribute continuation
parse(Input,
      [{prefix, Indent_Level, Attributes,
         [Block = {block_attributes, [{indent, Block_Level} | _], _}
          | To_Match]}
       | Stack])
  when Indent_Level > Block_Level ->
  parse(Input,
        [{prefix, Indent_Level, Attributes, To_Match}, Block | Stack]);

% block quote continuation
parse([$>, 32 | Tail],
      [{prefix, Level, Attributes, [Block = {block_quote, _, _} | To_Match]}
       | Stack]) ->
  parse(Tail,
        [{prefix, Level + 2, Attributes, To_Match}, Block | Stack]);

parse([$> | Tail = [10 | _]],
      [{prefix, Level, Attributes, [Block = {block_quote, _, _} | To_Match]}
       | Stack]) ->
  parse(Tail,
        [{prefix, Level + 2, Attributes, To_Match}, Block | Stack]);

% paragraph continuation
parse(Input = [Next | _],
      [{prefix, Level, Attributes, [Block = {para, _, _} | To_Match]} | Stack])
  when not ?SPACE(Next) ->
  parse(Input,
        [{prefix, Level, Attributes, To_Match}, Block | Stack]);

% fenced block continuation
parse(Input,
      [{prefix, Level, Attributes,
        [Block = {fenced_div, [{fence, Target} | _], _}]}
       | Stack]) ->
  maybe_close_fence(Input, $:, Target, Input,
                    [{prefix, Level, Attributes, []}, Block | Stack]);
parse(Input,
      [{prefix, Level, Attributes,
        [Block = {fenced_div, [{fence, Target} | _], _} | To_Match]}
       | Stack]) ->
  case lists:last(To_Match) of   % Hack #109 to prevent code closing a div
    {code_block, _, _} -> parse(Input,
                                [{prefix, Level, Attributes, To_Match}, Block
                                 | Stack]);
    _ -> maybe_close_fence(Input, $:, Target, Input,
                           [{prefix, Level, Attributes, To_Match}, Block
                            | Stack])
  end;

parse(Input,
      [{prefix, Level, Attributes,
        [Block = {code_block, [{fence, Target, Mark, _} | _], _} | To_Match]}
       | Stack]) ->
  maybe_close_fence(Input, Mark, Target, Input,
                    [{prefix, Level, Attributes, To_Match}, Block | Stack]);


% header continuation
parse(Input = [$# | Tail],
      Stack = [{prefix, _, _, [{heading, [{level, Level}|_], _}]} | _]) ->
  parse(Tail,
        [{heading, 1, Level, Input} | Stack]);
parse(Input = [32 | _],
      [{heading, N, N, _},
       {prefix, Level, Attributes, [Block | To_Match]} | Stack]) ->
  parse(Input,
        [{prefix, Level + N + 1, Attributes, To_Match}, Block | Stack]);
parse([$# | Tail],
      [{heading, Current, Target, Backtrack} | Stack = [{prefix, _, _, _} | _]])
  when Current < Target ->
  parse(Tail,
        [{heading, Current + 1, Target, Backtrack} | Stack]);
parse(_, [{heading, _, _, Backtrack},
              {prefix, Level, Attributes, To_Match} | Stack]) ->
  close_blocks(Backtrack,
               [{prefix, Level, Attributes, []}],
               Stack,
               lists:reverse(To_Match));

% footnote continuation
parse(Input,
      [{prefix, Indent_Level, Attributes,
         [Block = {footnote, [{indent, Block_Level} | _], _} | To_Match]}
       | Stack])
  when Indent_Level > Block_Level ->
  parse(Input,
        [{prefix, Indent_Level, Attributes, To_Match}, Block | Stack]);
parse(Input = "[^" ++ Tail,
      [{prefix, Level, Attributes, [Block = {footnote, _, _} | To_Match]}
       | Stack]) ->
  case is_footnote(Tail) of
    true -> close_blocks(Input,
                         [{prefix, Level, Attributes, []}],
                         Stack,
                         lists:reverse([Block | To_Match]));
    false -> parse(Input,
                   [{prefix, Level, Attributes, To_Match}, Block | Stack])
  end;

% link reference continuation
parse(Input,
      [{prefix, Indent_Level, Attributes,
         [Block = {reference_definition, [{indent, Block_Level} | _], _}
          | To_Match]}
       | Stack])
  when Indent_Level > Block_Level ->
  ?assertEqual(To_Match, []),
  continue_reference_definition(Input, [], Input,
                                Indent_Level, Attributes, Block, Stack);

% list continuation
parse(Input,
      [{prefix, Indent_Level, Attributes,
         [{list, [{marker, Marker}, {start, Start}, {tight, maybe}
                  | List_Attributes], List_Contents},
          Item = {list_item, [{indent, Item_Level} | _], _}
          | To_Match]}
       | Stack])
  when Indent_Level > Item_Level ->
  parse(Input,
        [{prefix, Indent_Level, Attributes, To_Match},
         Item,
         {list, [{marker, Marker}, {start, Start}, {tight, true}
                  | List_Attributes], List_Contents}
         | Stack]);
parse(Input,
      [{prefix, Indent_Level, Attributes,
         [List = {list, _, _},
          Item = {list_item, [{indent, Item_Level} | _], _}
          | To_Match]}
       | Stack])
  when Indent_Level > Item_Level ->
  parse(Input,
        [{prefix, Indent_Level, Attributes, To_Match},
         Item, List
         | Stack]);

parse(Input,
      [{prefix, Level, Attributes, To_Match =
        [List = {list,
          [{marker, Old_Marker}, {start, Old_Start}, {tight, Tight}
           | List_Attributes],
          List_Contents}
         | Rest]}
       | Stack]) ->
  case list_item_match(Input, Old_Marker, Old_Start) of
    {true, New_Marker, New_Start, Item_Level, _, Tail} ->
      New_Tight = case Tight of true  -> true;
                                maybe -> false;
                                false -> false end,
      close_blocks(Tail,
                   [{newblock, Level + Item_Level, []},
                    {list_item, [{indent, Level} | Attributes], []}],
                   [{list, [{marker, New_Marker},
                            {start, New_Start},
                            {tight, New_Tight}
                            | List_Attributes],
                     List_Contents} | Stack],
                   lists:reverse(Rest));
    {false, New_Marker, Item_Level, Value, Tail} ->
      close_blocks(Tail,
                   [{newblock, Level + Item_Level, []},
                    {list_item, [{indent, Level}], []},
                    {list, [{marker, New_Marker},
                            {start, Value},
                            {tight, true}
                            | Attributes], []}],
                   Stack,
                   lists:reverse(To_Match));
    false ->
      parse(Input,
            [{prefix, Level, Attributes, Rest}, List | Stack])
%     close_blocks(Input,
%                  [{prefix, Level, Attributes, []}],
%                  Stack,
%                  lists:reverse(To_Match))
  end;

% table continuation
parse([$| | Tail],
      [{prefix, _, _, [Table = {table, _, _}]} | Stack]) ->
  parse(Tail, [{alignment_row, Tail, [empty]}, Table | Stack]);

% everything else is not matched
parse(Input,
      [{prefix, Level, Attributes, []} | Stack = [{Element, _, _} | _]])
  when ?CONTAINS_BLOCK(Element) ->
  parse(Input, [{newblock, Level, Attributes} | Stack]);
parse(Input, [{prefix, _, _, []} | Stack]) ->
  parse(Input, Stack);

parse(Input, [{prefix, Level, Attributes, To_Match} | Stack]) ->
  Unmatched = [{Element, _, _} | _] = lists:reverse(To_Match),
  if ?CONTAINS_INLINE(Element) -> parse(Input,
                                        [{prefix, Level, Attributes, []}
                                         | Unmatched ++ Stack]);
     true -> close_blocks(Input,
                          [{prefix, Level, Attributes, []}],
                          Stack,
                          Unmatched)
  end;

%% Block-level openings

% blanks
parse([Space | Tail], [{newblock, Level, Attributes} | Stack])
  when Space =:= 32 orelse Space =:= 9 ->
  parse(Tail, [{newblock, Level + 1, Attributes} | Stack]);

parse([10 | Tail], [{newblock, _, Attributes} | Stack]) ->
  parse(Tail, [{newline, Attributes} | Stack]);

% block attributes
parse(Input = [${ | Tail = [Next | _]],
      [{newblock, Level, Attributes} | Stack])
  when ?SPACE(Next) orelse ?ALPHANUM(Next) orelse Next =:= $}
       orelse Next =:= $: %orelse Next =:= $- orelse Next =:= $_
       orelse Next =:= $. orelse Next =:= $# orelse Next =:= $% ->
  parse(Tail,
        [{block_attributes,
          [{indent, Level} | Attributes],
          [{attributes, [], {Input, [{para, Attributes, []} | Stack]}}]}
         | Stack]);

% block quote
parse([$> | Tail = [Space | _]], [{newblock, Level, Attributes} | Stack])
  when ?SPACE(Space) ->
  parse(Tail,
        [{newblock, Level + 1, []}, {block_quote, Attributes, []} | Stack]);

% caption
parse([$^, 32 | Tail], [{newblock, _, Attributes} | Stack]) ->
  parse(Tail, [{caption, Attributes, []} | Stack]);
parse([$^, 9 | Tail], [{newblock, _, Attributes} | Stack]) ->
  parse(Tail, [{caption, Attributes, []} | Stack]);

% div
parse(Input = ":::" ++ Tail, Stack = [{newblock, _, _} | _]) ->
  parse(Tail, [{opening, fenced_div, 3, Input} | Stack]);
parse([$: | Tail], [{opening, fenced_div, Level, Backtrack} | Stack]) ->
  parse(Tail, [{opening, fenced_div, Level + 1, Backtrack} | Stack]);
parse([Head | Tail], [{opening, fenced_div, Level, Backtrack} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail, [{opening, fenced_div, Level, [], Backtrack} | Stack]);
parse([Head | Tail], [{opening, fenced_div, Level, [], Backtrack} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail, [{opening, fenced_div, Level, [], Backtrack} | Stack]);
parse([Head | Tail], [{opening, fenced_div, Level, Class, Backtrack} | Stack])
  when ?ALPHANUM(Head) orelse Head =:= $- orelse Head =:= $_ ->
  parse(Tail,
        [{opening, fenced_div, Level, [Head | Class], Backtrack} | Stack]);
parse([Head | Tail], [{_, fenced_div, Level, Class, Backtrack} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail, [{done, fenced_div, Level, Class, Backtrack} | Stack]);
parse([10 | Tail],
      [{_, fenced_div, Level, _}, {newblock, _, Attributes} | Stack]) ->
  parse(Tail,
        [{newline, []},
         {fenced_div, [{fence, Level} | Attributes], []}
         | Stack]);
parse([10 | Tail],
      [{_, fenced_div, Level, [], _}, {newblock, _, Attributes} | Stack]) ->
  parse(Tail,
        [{newline, []},
         {fenced_div, [{fence, Level} | Attributes], []}
         | Stack]);
parse([10 | Tail],
      [{_, fenced_div, Level, Class, _}, {newblock, _, Attributes} | Stack]) ->
  parse(Tail,
        [{newline, []},
         {fenced_div,
          [{fence, Level}, {class, lists:reverse(Class)} | Attributes],
          []}
         | Stack]);
parse(_, [{_, fenced_div, _, Backtrack}, {newblock, _, Attributes} | Stack]) ->
  parse(Backtrack, [{para, Attributes, []} | Stack]);
parse(_,
      [{_, fenced_div, _, _, Backtrack}, {newblock, _, Attributes} | Stack]) ->
  parse(Backtrack, [{para, Attributes, []} | Stack]);

% fenced code block
parse(Input = "```" ++ Tail, Stack = [{newblock, _, _} | _]) ->
  parse(Tail, [{opening, code_block, 3, $`, Input} | Stack]);
parse(Input = "\~\~\~" ++ Tail, Stack = [{newblock, _, _} | _]) ->
  parse(Tail, [{opening, code_block, 3, $~, Input} | Stack]);
parse([Mark | Tail],
      [{opening, code_block, Level, Mark, Backtrack} | Stack]) ->
  parse(Tail, [{opening, code_block, Level + 1, Mark, Backtrack} | Stack]);
parse([Head | Tail], [{opening, code_block, Level, Mark, Backtrack} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail, [{opening, code_block, Level, Mark, [], Backtrack} | Stack]);
parse([Head | Tail],
      [{opening, code_block, Level, Mark, [], Backtrack} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail, [{opening, code_block, Level, Mark, [], Backtrack} | Stack]);
parse([Head | Tail],
      [{opening, code_block, Level, Mark, Lang, Backtrack} | Stack])
  when (not ?SPACE(Head)) andalso (Head =/= $` orelse Mark =/= $`) ->
  parse(Tail,
        [{opening, code_block, Level, Mark, [Head | Lang], Backtrack}
         | Stack]);
parse([Head | Tail], [{_, code_block, Level, Mark, Lang, Backtrack} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail, [{done, code_block, Level, Mark, Lang, Backtrack} | Stack]);
parse([10 | Tail],
      [{_, code_block, Level, Mark, _}, {newblock, Indent, Attributes}
       | Stack]) ->
  parse(Tail,
        [{newline, []},
         {code_block, [{fence, Level, Mark, Indent} | Attributes], []}
         | Stack]);
parse([10 | Tail],
      [{_, code_block, Level, Mark, [], _}, {newblock, Indent, Attributes}
       | Stack]) ->
  parse(Tail,
        [{newline, []},
         {code_block, [{fence, Level, Mark, Indent} | Attributes], []}
         | Stack]);
parse([10 | Tail],
      [{_, code_block, Level, Mark, Lang, _}, {newblock, Indent, Attributes}
       | Stack]) ->
  parse(Tail,
        [{newline, []},
         {code_block,
          [{fence, Level, Mark, Indent}, {lang, lists:reverse(Lang)}
           | Attributes],
          []}
         | Stack]);
parse(_,
      [{_, code_block, _, _, Backtrack}, {newblock, _, Attributes} | Stack]) ->
  parse(Backtrack, [{para, Attributes, []} | Stack]);
parse(_,
      [{_, code_block, _, _, _, Backtrack}, {newblock, _, Attributes}
       | Stack]) ->
  parse(Backtrack, [{para, Attributes, []} | Stack]);

% footnote definition
parse(Input = "[^" ++ Tail, [{newblock, Level, Attributes} | Stack]) ->
  parse(Tail, [{footnote,
                [],
                Level,
                [{indent, Level} | Attributes],
                {Input, [{para, Attributes, []} | Stack]}}
               | Stack]);
parse("]:" ++ (Tail = [Space | _]),
      [{footnote, Name, Level, Attributes, {_, _}} | Stack])
  when ?SPACE(Space) ->
  parse(Tail,
        [{footnote, Name, Level + 2, Attributes, done} | Stack]);
parse([Space | Tail], [{footnote, Name, Level, Attributes, done} | Stack])
  when Space =:= 32 orelse Space =:= 9 ->
  parse(Tail, [{footnote, Name, Level + 1, Attributes, done} | Stack]);
parse([10 | Tail],
      [{footnote, Name, _, [{indent, Level} | Attributes], done} | Stack]) ->
  parse(Tail, [{newline, []},
               {footnote,
                [{indent, Level},
                 {"reference", lists:reverse(Name)}
                 | Attributes],
                []}
               | Stack]);
parse(Input,
      [{footnote, Name, Next_Level,[{indent, Level} | Attributes], done}
       | Stack]) ->
  parse(Input, [{newblock, Next_Level, []},
                {footnote, [{indent, Level},
                            {"reference", lists:reverse(Name)}
                            | Attributes], []}
                | Stack]);
parse([Head | _],
      [{footnote, _, _, _, {Input, Stack}} | _])
  when Head =:= 10 orelse Head =:= 13 orelse Head =:= $] ->
  parse(Input, Stack);
parse([Head | Tail],
      [{footnote, Name, Level, Attributes, Fallback} | Stack]) ->
  parse(Tail,
        [{footnote, [Head | Name], Level + 1, Attributes, Fallback} | Stack]);

% heading
parse(Input = [$# | Tail], [{newblock, _, Attributes} | Stack]) ->
  parse(Tail, [{heading, 1, Attributes, Input} | Stack]);
parse([$# | Tail], [{heading, Level, Attributes, Backtrack} | Stack]) ->
  parse(Tail, [{heading, Level + 1, Attributes, Backtrack} | Stack]);
parse([Space | Tail], [{heading, Level, Attributes, _} | Stack])
  when Space =:= 32 orelse Space =:= 9 ->
  parse(Tail, [{heading, [{level, Level} | Attributes], []} | Stack]);
parse([10 | Tail], [{heading, Level, Attributes, _} | Stack]) ->
  parse(Tail,
        [{newline, []}, {heading, [{level, Level} | Attributes], []} | Stack]);
parse(_, [{heading, _, Attributes, Backtrack} | Stack]) ->
  parse(Backtrack, [{para, Attributes, []} | Stack]);

% link reference definition
parse(Input = "[" ++ Tail, [{newblock, Level, Attributes} | Stack]) ->
  parse(Tail, [{reference_definition,
                [],
                [{indent, Level} | Attributes],
                {Input, [{para, Attributes, []} | Stack]}}
               | Stack]);
parse("]:" ++ [Space | Tail],
      [{reference_definition, Name, Attributes, Fallback} | Stack])
  when Space =:= 32 orelse Space =:= 9 ->
  parse(Tail,
        [{reference_definition, Name, [], Attributes, Fallback} | Stack]);
parse("]:" ++ [10 | Tail],
      [{reference_definition, Name, [{indent, Level} | Attr], _} | Stack]) ->
  parse(Tail, [{newline, []},
               {reference_definition,
                [{indent, Level},
                 {"reference", lists:reverse(Name)}
                 | Attr],
                []}
               | Stack]);
parse([Head | _],
      [{reference_definition, _, _, {Input, Stack}} | _])
  when Head =:= 10 orelse Head =:= 13 orelse Head =:= $] ->
  parse(Input, Stack);
parse([Head | Tail],
      [{reference_definition, Name, Attributes, Fallback} | Stack]) ->
  parse(Tail,
        [{reference_definition, [Head | Name], Attributes, Fallback} | Stack]);
parse([Head | Tail],
      Stack = [{reference_definition, _, [], _, _} | _])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail, Stack);
parse([10 | Tail],
      [{reference_definition, Name, Contents, [{indent, Level} | Attr], _}
       | Stack]) ->
  parse(Tail, [{newline, []},
               {reference_definition,
                [{indent, Level},
                 {"reference", lists:reverse(Name)}
                 | Attr],
                Contents}
               | Stack]);
parse([Head | Tail],
      [{reference_definition, Name, Contents, Attributes, Backtrack}
       | Stack]) ->
  parse(Tail,
        [{reference_definition, Name, [Head | Contents], Attributes, Backtrack}
         | Stack]);

%% list item
%parse([Marker | Tail = [Space | _]],
%      [{newblock, Level, Attributes}
%       | Stack = [{list, [{marker, [Marker]} | _], _} |_]])
%  when ?SPACE(Space) ->
%  parse(Tail,
%        [{newblock, Level + 1, []},
%         {list_item, [{indent, Level} | Attributes], []}
%         | Stack]);

%parse([Marker | Tail = [Space | _]],
%      [{newblock, Level, Attributes} | Stack])
%  when Space =:= 32 orelse Space =:= 9,
%       Marker =:= $* orelse Marker =:= $+
%         orelse Marker =:= $- orelse Marker =:= $: ->
%  parse(Tail,
%        [{newblock, Level + 1, []},
%         {list_item, [{indent, Level}], []},
%         {list, [{marker, [Marker]} | Attributes], []}
%         | Stack]);

% table

parse([$| | Tail], [{newblock, _, Attributes} | Stack]) ->
  parse(Tail, [{alignment_row, Tail, [empty]},
               {table, [{align, []} | Attributes], []} | Stack]);

% fall back on thematic break or list or paragraph
parse(Input, [{newblock, Level, Attributes} | Stack]) ->
  case thematic_break_or_list_mark(Input) of
    {thematic_break, Tail} ->
       parse(Tail, [{newline, []},
                    {thematic_break, Attributes, []}
                    | Stack]);
    {Marker, Item_Level, Value, Tail} ->
       parse(Tail, [{newblock, Level + Item_Level, []},
                    {list_item, [{indent, Level}], []},
                    {list, [{marker, Marker}, {start, Value}, {tight, true}
                            | Attributes], []}
                    | Stack]);
    false -> parse(Input, [{para, Attributes, []} | Stack])
  end;

%% Attributes

% attribute dispatching
parse([10 | Tail], Stack = ?INLINE_TOP({attributes, _, _})) ->
  parse(Tail, [{newline, []} | Stack]);
parse([Head | Tail], Stack = ?INLINE_TOP({attributes, _, _}))
  when ?SPACE(Head) ->
  parse(Tail, Stack);
parse([$% | Tail], ?INLINE_STACK(Stack = [{attributes, _, _} | _])) ->
  parse(Tail, ?INLINE_STACK([{comment, ""} | Stack]));
parse([$# | Tail], ?INLINE_STACK(Stack = [{attributes, _, _} | _])) ->
  parse(Tail, ?INLINE_STACK([{name, "identifier", ""} | Stack]));
parse([$. | Tail], ?INLINE_STACK(Stack = [{attributes, _, _} | _])) ->
  parse(Tail, ?INLINE_STACK([{name, "class", ""} | Stack]));
parse(Input = [Head|_], ?INLINE_STACK(Stack = [{attributes, _, _} | _]))
  when ?ALPHANUM(Head)
       orelse Head =:= $: orelse Head =:= $- orelse Head =:= $_ ->
  parse(Input, ?INLINE_STACK([{key, ""} | Stack]));

% end attribute parsing
parse([$} | Tail],
      [{block_attributes, Old_Attr, [{attributes, New_Attr, Backtrack}]}
       | Stack]) ->
  parse(Tail, [{block_attributes,
                Old_Attr ++ lists:reverse(New_Attr),
                done,
                Backtrack}
               | Stack]);
parse([$} | Tail], ?INLINE_STACK([{attributes, [], _} | Stack])) ->
  parse(Tail, ?INLINE_STACK(Stack));
parse([$} | Tail], ?INLINE_TOP({attributes, Acc, _}, {Element})) ->
  parse(Tail, ?INLINE_TOP({Element, lists:reverse(Acc), []}));
parse([$} | Tail], ?INLINE_TOP({attributes, Acc, _}, {text, Reverse_Text})) ->
  case last_word(Reverse_Text) of
    {_, []} -> parse(Tail, ?INLINE_TOP({text, Reverse_Text}));
    {[], Word} ->
      parse(Tail, ?INLINE_TOP({span, lists:reverse(Acc), [{text, Word}]}));
    {Reverse_Prefix, Word} ->
      parse(Tail,
            ?INLINE_TOP({span, lists:reverse(Acc), [{text, Word}]},
                        {text, Reverse_Prefix}))
  end;
parse([$} | Tail], ?INLINE_TOP({attributes, Acc, _}, {Element, Contents})) ->
  parse(Tail, ?INLINE_TOP({Element, lists:reverse(Acc), Contents}));
parse([$} | Tail],
      ?INLINE_TOP({attributes, Acc, _}, {Element, Attributes, Contents})) ->
  parse(Tail,
        ?INLINE_TOP({Element, Attributes ++ lists:reverse(Acc), Contents}));
parse([$} | Tail], ?INLINE_STACK([{attributes, _, _} | Stack])) ->
  parse(Tail, ?INLINE_STACK(Stack));

parse([10 | Tail],
      [{block_attributes, Attributes, done, _} | Stack]) ->
  parse(Tail, [{newline, Attributes} | Stack]);

parse([Head | Tail], Stack = [{block_attributes, _, done, _} | _])
  when ?SPACE(Head) ->
  parse(Tail, Stack);

parse(_, [{block_attributes, _, done, {Input, Stack}} | _]) ->
  parse(Input, Stack);

% name (identifier or class)
parse(Input = [Head | _],
      ?INLINE_TOP({name, Key, Value}, {attributes, Acc, Backtrack}))
  when ?SPACE(Head) orelse Head =:= $} ->
  parse(Input,
        ?INLINE_TOP({attributes,
                     [{Key, lists:reverse(Value)} | Acc],
                     Backtrack}));

parse([Head|_], ?INLINE_TOP({name, _, _}, {attributes, _, {Input, Stack}}))
  when ?PUNCTUATION(Head)
       andalso Head =/= $_ andalso Head =/= $- andalso Head =/= $: ->
  parse(Input, Stack);

parse([Head | Tail],
      ?INLINE_TOP({name, Key, Value}, Attr = {attributes, _, _})) ->
  parse(Tail, ?INLINE_TOP({name, Key, [Head | Value]}, Attr));

% key
parse([$= | Tail], ?INLINE_TOP({key, Key}, Attr = {attributes, _, _})) ->
  parse(Tail, ?INLINE_TOP({key, lists:reverse(Key), ""}, Attr));

parse([Head | Tail], ?INLINE_TOP({key, Key}, Attr = {attributes, _, _}))
  when ?ALPHANUM(Head)
       orelse Head =:= $: orelse Head =:= $- orelse Head =:= $_ ->
  parse(Tail, ?INLINE_TOP({key, [Head | Key]}, Attr));

parse(_, ?INLINE_TOP({key, _}, {attributes, _, {Input, Stack}})) ->
  parse(Input, Stack);

% value dispatch
parse([$" | Tail], ?INLINE_TOP({key, Key, ""}, Attr = {attributes, _, _})) ->
  parse(Tail, ?INLINE_TOP({quotedvalue, Key, ""}, Attr));

parse([Head | Tail], ?INLINE_TOP({key, Key, ""}, Attr = {attributes, _, _}))
  when ?ALPHANUM(Head)
       orelse Head =:= $: orelse Head =:= $- orelse Head =:= $_ ->
  parse(Tail, ?INLINE_TOP({barevalue, Key, [Head]}, Attr));

parse(_, ?INLINE_TOP({key, _, ""}, {attributes, _, {Input, Stack}})) ->
  parse(Input, Stack);

% quoted value
parse([$" | Tail],
      ?INLINE_TOP({quotedvalue, Key, Value}, {attributes, Acc, Backtrack})) ->
  parse(Tail,
        ?INLINE_TOP({attributes,
                     [{Key, lists:reverse(Value)} | Acc],
                     Backtrack}));

parse([$\\, Head | Tail],
      ?INLINE_TOP({quotedvalue, Key, Value}, Attr = {attributes, _, _})) ->
  parse(Tail, ?INLINE_TOP({quotedvalue, Key, [Head | Value]}, Attr));

parse([10 | Tail],
      ?INLINE_TOP({quotedvalue, Key, Value}, Attr = {attributes, _, _})) ->
  parse(Tail, [{newline, []}
               | ?INLINE_TOP({quotedvalue, Key, [32 | Value]}, Attr)]);

parse([Head | Tail],
      ?INLINE_TOP({quotedvalue, Key, Value}, Attr = {attributes, _, _})) ->
  parse(Tail, ?INLINE_TOP({quotedvalue, Key, [Head | Value]}, Attr));

% bare value
parse([Head | Tail],
      ?INLINE_TOP({barevalue, Key, Value}, Attr = {attributes, _, _}))
  when ?ALPHANUM(Head)
       orelse Head =:= $: orelse Head =:= $- orelse Head =:= $_ ->
  parse(Tail, ?INLINE_TOP({barevalue, Key, [Head | Value]}, Attr));

parse(Input = [Head | _],
      ?INLINE_TOP({barevalue, Key, Value}, {attributes, Acc, Backtrack}))
  when ?SPACE(Head) orelse Head =:= $} ->
  parse(Input,
        ?INLINE_TOP({attributes,
                     [{Key, lists:reverse(Value)} | Acc],
                     Backtrack}));

% comment
parse(Input = [$} | _],
      ?INLINE_TOP({comment, Value}, {attributes, Acc, Backtrack})) ->
  parse(Input,
        ?INLINE_TOP({attributes,
                     [{comment, lists:reverse(Value)} | Acc],
                     Backtrack}));

parse([$% | Tail],
      ?INLINE_TOP({comment, Value}, {attributes, Acc, Backtrack})) ->
  parse(Tail,
        ?INLINE_TOP({attributes,
                     [{comment, lists:reverse(Value)} | Acc],
                     Backtrack}));

parse([Head | Tail],
      ?INLINE_TOP({comment, Value}, Attr = {attributes, _, _})) ->
  parse(Tail, ?INLINE_TOP({comment, [Head | Value]}, Attr));

% raw inline element (opened with the attributes)

parse([$} | Tail], ?INLINE_TOP({raw_inline, Format = [_|_], Contents, _})) ->
  parse(Tail,
        ?INLINE_TOP({raw_inline,
                     [{"format", lists:reverse(Format)}],
                     Contents}));
parse([Head | Tail], ?INLINE_TOP({raw_inline, Format, Contents, Backtrack}))
  when not (?SPACE(Head)
            orelse Head =:= ${ orelse Head =:= $} orelse Head =:= $`) ->
  parse(Tail, ?INLINE_TOP({raw_inline, [Head | Format], Contents, Backtrack}));
parse(_, ?INLINE_TOP({raw_inline, _, _, {Input, Stack}})) ->
  parse(Input, Stack);

%% alignment row in table

parse([10 | Tail],
      [{alignment_row, _, [empty | Cells = [_|_]]},
       {table, [{align, _} | Attributes], []} | Stack]) ->
  parse(Tail,
        [{newline, []},
         {table, [{align, lists:reverse(Cells)} | Attributes], []}
         | Stack]);
parse([10 | Tail],
      [{alignment_row, _, [empty | Cells = [_|_]]},
       {table,
        [{align, _} | Attributes],
        [{row, [], Contents} | Rows]} | Stack]) ->
  Alignment = lists:reverse(Cells),
  parse(Tail,
        [{newline, []},
         {table,
          [{align, Alignment} | Attributes],
          [{row, [{head, true}],
            align_cells(Contents, Alignment, [{head, true}], [])} | Rows]}
         | Stack]);
parse([$: | Tail], [{alignment_row, Input, [empty | Cells]} | Stack]) ->
  parse(Tail, [{alignment_row, Input, [half_left | Cells]} | Stack]);
parse([$- | Tail], [{alignment_row, Input, [half_left | Cells]} | Stack]) ->
  parse(Tail, [{alignment_row, Input, [left | Cells]} | Stack]);
parse([$- | Tail], [{alignment_row, Input, [empty | Cells]} | Stack]) ->
  parse(Tail, [{alignment_row, Input, [default | Cells]} | Stack]);
parse([$- | Tail], [{alignment_row, Input, [default | Cells]} | Stack]) ->
  parse(Tail, [{alignment_row, Input, [default | Cells]} | Stack]);
parse([$: | Tail], [{alignment_row, Input, [default | Cells]} | Stack]) ->
  parse(Tail, [{alignment_row, Input, [right | Cells]} | Stack]);
parse([$: | Tail], [{alignment_row, Input, [left | Cells]} | Stack]) ->
  parse(Tail, [{alignment_row, Input, [center | Cells]} | Stack]);
parse([$| | _], [{alignment_row, Input, [empty | _]} | Stack]) ->
  parse(Input, [{cell, [], []}, {row, "|" ++ Input, []} | Stack]);
parse([$| | _], [{alignment_row, Input, [half_left | _]} | Stack]) ->
  parse(Input, [{cell, [], []}, {row, "|" ++ Input, []} | Stack]);
parse([$| | Tail], [{alignment_row, Input, Cells} | Stack]) ->
  parse(Tail, [{alignment_row, Input, [empty | Cells]} | Stack]);
parse(_,  [{alignment_row, Input, _} | Stack]) ->
  parse(Input, [{cell, [], []}, {row, "|" ++ Input, []} | Stack]);

%% code block contents

parse([10 | Tail], [{code_block, Attributes, Contents} | Stack]) ->
  parse(Tail,
        [{newline, []}, {code_block, Attributes, [10 | Contents]} | Stack]);
parse([Head | Tail], [{code_block, Attributes, Contents} | Stack]) ->
  parse(Tail, [{code_block, Attributes, [Head | Contents]} | Stack]);

%% continuations after line break

parse(Input, ?INLINE_TOP({soft_break}, {verbatim, N, opener, 0})) ->
  parse(Input, ?INLINE_TOP({verbatim, N, [10], 0}));
parse(Input, ?INLINE_TOP({soft_break}, {verbatim, N, Text, 0})) ->
  parse(Input, ?INLINE_TOP({verbatim, N, [10 | Text], 0}));
parse(Input, ?INLINE_TOP({soft_break}, Top = {ref, $), _, _, _, _})) ->
  parse(Input, ?INLINE_TOP(Top));
parse(Input, ?INLINE_TOP({soft_break},
                         {ref, $], Text, Element, Contents, Fallback})) ->
  parse(Input,
        ?INLINE_TOP({ref, $], [32 | Text], Element, Contents, Fallback}));

%% end of line

parse([10 | _], ?INLINE_TOP({autolink, _, _, Input, Stack})) ->
  parse(Input, Stack);

parse([10 | Tail],
      [{cell, _, []}, {row, _, Cells},
       {table, Attributes = [{align, Alignment} | _], Contents}
       | Stack]) ->
  New_Row = {row, [], align_cells(lists:reverse(Cells), Alignment, [], [])},
  parse(Tail,
        [{newline, []}, {table, Attributes, [New_Row | Contents]} | Stack]);
parse([10 | Tail],
      [{cell, _, []}, {row, _, Cells}, {table, Attributes, Contents}
       | Stack]) ->
  parse(Tail,
         [{newline, []},
          {table, Attributes, [{row, [], lists:reverse(Cells)} | Contents]}
          | Stack]);
parse([10 | _],
      [{cell, _, _}, {row, Input, _}, {table, Attributes, []} | Stack]) ->
  parse(Input, [{para, Attributes, []} | Stack]);
parse([10 | _],
      [{cell, _, _}, {row, Input, _}, Table = {table, _, _} | Stack]) ->
  close_blocks(Input, [], Stack, [Table]);

parse([10 | _], ?INLINE_TOP({symbol, _, Input, Stack})) ->
  parse(Input, Stack);

parse(Input = [10 | _],
      ?INLINE_TOP({verbatim, N, " `" ++ Text, N}, {opener, "$"})) ->
  parse(Input, ?INLINE_TOP({inline_math, lists:reverse("`" ++ Text)}));
parse(Input = [10 | _],
      ?INLINE_TOP({verbatim, N, " `" ++ Text, N}, {opener, "$$"})) ->
  parse(Input, ?INLINE_TOP({display_math, lists:reverse("`" ++ Text)}));
parse(Input = [10 | _], ?INLINE_TOP({verbatim, N, " `" ++ Text, N})) ->
  parse(Input, ?INLINE_TOP({verbatim, lists:reverse("`" ++ Text)}));
parse(Input = [10 | _], ?INLINE_TOP({verbatim, N, Text, N}, {opener, "$"})) ->
  parse(Input, ?INLINE_TOP({inline_math, lists:reverse(Text)}));
parse(Input = [10 | _], ?INLINE_TOP({verbatim, N, Text, N}, {opener, "$$"})) ->
  parse(Input, ?INLINE_TOP({display_math, lists:reverse(Text)}));
parse(Input = [10 | _], ?INLINE_TOP({verbatim, N, Text, N})) ->
  parse(Input, ?INLINE_TOP({verbatim, lists:reverse(Text)}));
parse(Input = [10 | _], ?INLINE_TOP({verbatim, N, Text, Catchup}))
  when Catchup > 0 ->
  parse(Input, ?INLINE_TOP({verbatim, N, [$` | Text], Catchup - 1}));

parse([10 | Tail], Stack = ?INLINE_TOP({hard_break})) ->
  parse(Tail, [{newline, []} | Stack]);
parse([10 | Tail], ?INLINE_STACK(Stack)) ->
  parse(Tail, [{newline, []} | ?INLINE_STACK([{soft_break} | Stack])]);

%% Inline elements

% escape
parse([$\\ | [Head | Tail]], ?INLINE_STACK(Stack)) when ?PUNCTUATION(Head) ->
  parse(Tail, ?INLINE_STACK(push_char(Stack, Head)));
parse([$\\ | Tail = [10 | _]], ?INLINE_STACK(Stack)) ->
  parse(Tail, ?INLINE_STACK([{hard_break} | trim_top_spaces(Stack)]));
parse([Head = $\\ | Tail = [9 | _]], ?INLINE_STACK(Stack)) ->
  case spaces_to_eol(Tail) of
    false ->
      parse(Tail, ?INLINE_STACK(push_char(Stack, Head)));
    Trimmed_Tail ->
      parse(Trimmed_Tail,
            ?INLINE_STACK([{hard_break} | trim_top_spaces(Stack)]))
  end;
parse("\\ " ++ Tail, ?INLINE_STACK(Stack)) ->
  case spaces_to_eol(Tail) of
    false ->
      parse(Tail, ?INLINE_STACK([{non_breaking_space} | Stack]));
    Trimmed_Tail ->
      parse(Trimmed_Tail,
            ?INLINE_STACK([{hard_break} | trim_top_spaces(Stack)]))
  end;

% references
parse(Input = "[" ++ Tail, ?INLINE_STACK([{brackets, Contents} | Stack])) ->
  parse(Tail, ?INLINE_STACK([{ref, $], "", "reference", Contents,
                              {Input, ?INLINE_STACK(Stack)}} | Stack]));
parse(Input = "(" ++ Tail, ?INLINE_STACK([{brackets, Contents} | Stack])) ->
  parse(Tail, ?INLINE_STACK([{ref, $), "", "target", Contents,
                              {Input, ?INLINE_STACK(Stack)}} | Stack]));
parse([Head | Tail], ?INLINE_TOP({ref, Head, Text, footnote, [], _})) ->
  parse(Tail, ?INLINE_TOP({footnote_ref, lists:reverse(Text)}));
parse([Head | Tail],
      ?INLINE_TOP({ref, Head, Text, Element, Contents, _}, {opener, "!"})) ->
  parse(Tail, ?INLINE_TOP({img, [{Element, lists:reverse(Text)}], Contents}));
parse([Head | Tail], ?INLINE_TOP({ref, Head, Text, Element, Contents, _})) ->
  parse(Tail, ?INLINE_TOP({link, [{Element, lists:reverse(Text)}], Contents}));
parse([Head | Tail],
      ?INLINE_TOP({ref, Mark, Text, Element, Contents, Fallback})) ->
  parse(Tail,
        ?INLINE_TOP({ref, Mark, [Head | Text], Element, Contents, Fallback}));

% verbatim span
parse("`" ++ Tail, ?INLINE_TOP({verbatim, N, opener, 0})) ->
  parse(Tail, ?INLINE_TOP({verbatim, N + 1, opener, 0}));
parse([Head | Tail], ?INLINE_TOP({verbatim, N, opener, 0})) ->
  parse(Tail, ?INLINE_TOP({verbatim, N, [Head], 0}));

parse("`" ++ Tail, ?INLINE_TOP({verbatim, N, " ", 0})) ->
  parse(Tail, ?INLINE_TOP({verbatim, N, "", 1}));

parse("`" ++ Tail, ?INLINE_TOP({verbatim, N, Text, Closing})) ->
  parse(Tail, ?INLINE_TOP({verbatim, N, Text, Closing + 1}));
parse(Input, ?INLINE_TOP({verbatim, N, " `" ++ Text, N}, {opener, "$"})) ->
  parse(Input, ?INLINE_TOP({inline_math, lists:reverse("`" ++ Text)}));
parse(Input, ?INLINE_TOP({verbatim, N, " `" ++ Text, N}, {opener, "$$"})) ->
  parse(Input, ?INLINE_TOP({display_math, lists:reverse("`" ++ Text)}));
parse(Input, ?INLINE_TOP({verbatim, N, " `" ++ Text, N})) ->
  parse(Input, ?INLINE_TOP({verbatim, lists:reverse("`" ++ Text)}));
parse(Input, ?INLINE_TOP({verbatim, N, Text, N}, {opener, "$"})) ->
  parse(Input, ?INLINE_TOP({inline_math, lists:reverse(Text)}));
parse(Input, ?INLINE_TOP({verbatim, N, Text, N}, {opener, "$$"})) ->
  parse(Input, ?INLINE_TOP({display_math, lists:reverse(Text)}));
parse(Input, ?INLINE_TOP({verbatim, N, Text, N})) ->
  parse(Input, ?INLINE_TOP({verbatim, lists:reverse(Text)}));

parse([Head | Tail], ?INLINE_TOP({verbatim, N, Text, 0})) ->
  parse(Tail, ?INLINE_TOP({verbatim, N, [Head | Text], 0}));
parse([Head | Tail], ?INLINE_TOP({verbatim, N, Text, catchup, 0})) ->
  parse(Tail, ?INLINE_TOP({verbatim, N, [Head | Text], 0}));

parse(Input = [_|_], ?INLINE_TOP({verbatim, N, Text, Catchup})) ->
  parse(Input, ?INLINE_TOP({verbatim, N, [$` | Text], catchup, Catchup - 1}));
parse(Input = [_|_], ?INLINE_TOP({verbatim, N, Text, catchup, Catchup})) ->
  parse(Input, ?INLINE_TOP({verbatim, N, [$` | Text], catchup, Catchup - 1}));

parse("`" ++ Tail, ?INLINE_STACK(Stack)) ->
  parse(Tail, ?INLINE_STACK([{verbatim, 1, opener, 0} | Stack]));
parse("$$" ++ (Tail = [$` | _]), ?INLINE_STACK(Stack)) ->
  parse(Tail, ?INLINE_STACK([{opener, "$$"} | Stack]));
parse("$" ++ (Tail = [$` | _]), ?INLINE_STACK(Stack)) ->
  parse(Tail, ?INLINE_STACK([{opener, "$"} | Stack]));

% inline attribute start
parse(Input = [${ | Tail = [Next | _]], Stack = ?INLINE_STACK(Inline_Stack))
  when ?SPACE(Next) orelse ?ALPHANUM(Next) orelse Next =:= $}
       orelse Next =:= $: %orelse Next =:= $- orelse Next =:= $_
       orelse Next =:= $. orelse Next =:= $# orelse Next =:= $% ->
  parse(Tail,
        ?INLINE_STACK([{attributes, [], {[$\\ | Input], Stack}}
                       | Inline_Stack]));
parse(Input = "{=" ++ Tail,
      Stack = ?INLINE_TOP({verbatim, Contents})) ->
  parse(Tail,
        ?INLINE_TOP({raw_inline, [], Contents, {[$\\ | Input], Stack}}));

% auto links
parse(Input = [$< | Tail], Old_Stack = ?INLINE_STACK(Stack)) ->
  parse(Tail,
        ?INLINE_STACK([{autolink, any, "", "\\" ++ Input, Old_Stack}
                       | Stack]));
parse([Head | _], ?INLINE_TOP({autolink, _, _, Input, Stack}))
  when ?SPACE(Head) orelse Head =:= $< ->
  parse(Input, Stack);
parse([Head | Tail], ?INLINE_TOP({autolink, Type, Text, Input, Fallback}))
  when (Head >= $A andalso Head =< $Z)
       orelse (Head >= $a andalso Head =< $z) ->
  parse(Tail, ?INLINE_TOP({autolink, Type, [Head | Text], Input, Fallback}));
parse([Head = $: | Tail],
      ?INLINE_TOP({autolink, any, Text, Input, Fallback})) ->
  parse(Tail, ?INLINE_TOP({autolink, url, [Head | Text], Input, Fallback}));
parse([Head = $@ | Tail], ?INLINE_TOP({autolink, Type, Text, Input, Fallback}))
  when Type =:= any orelse Type =:= maybe_email ->
  parse(Tail, ?INLINE_TOP({autolink, email, [Head | Text], Input, Fallback}));
parse([$> | Tail], ?INLINE_TOP({autolink, email, Reversed_Text, _, _})) ->
  Text = lists:reverse(Reversed_Text),
  parse(Tail,
        ?INLINE_TOP({link, [{"target", "mailto:" ++ Text}], [{text, Text}]}));
parse([$> | Tail], ?INLINE_TOP({autolink, url, Reversed_Text, _, _})) ->
  Text = lists:reverse(Reversed_Text),
  parse(Tail, ?INLINE_TOP({link, [{"target", Text}], [{text, Text}]}));
parse([$> | _], ?INLINE_TOP({autolink, _, _, Input, Stack})) ->
  parse(Input, Stack);
parse([Head | Tail], ?INLINE_TOP({autolink, any, Text, Input, Fallback})) ->
  parse(Tail,
        ?INLINE_TOP({autolink, maybe_email, [Head | Text], Input, Fallback}));
parse([Head | Tail], ?INLINE_TOP({autolink, Type, Text, Input, Fallback})) ->
  parse(Tail, ?INLINE_TOP({autolink, Type, [Head | Text], Input, Fallback}));

% symbols
parse([$: | Tail], ?INLINE_TOP({symbol, Text = [_|_], _, _})) ->
  parse(Tail, ?INLINE_TOP({symbol, lists:reverse(Text)}));
parse([Head | Tail], ?INLINE_TOP({symbol, Text, Fallback_I, Fallback_S}))
  when ?ALPHANUM(Head) orelse Head =:= $_ orelse Head =:= $-
                                          orelse Head =:= $+ ->
  parse(Tail, ?INLINE_TOP({symbol, [Head | Text], Fallback_I, Fallback_S}));
parse(_, ?INLINE_TOP({symbol, _, Input, Stack})) ->
  parse(Input, Stack);
parse([$: | Tail], ?INLINE_STACK(Stack)) ->
  parse(Tail,
        ?INLINE_STACK([{symbol, [], Tail, ?INLINE_STACK(push_char(Stack, $:))}
                       | Stack]));

% table cell
parse([$| | Tail], [Cell = {cell, _, _} | Stack]) ->
  close_blocks(Tail, [{cell, [], []}], Stack, [Cell]);
parse([Head | Tail], Stack = [{cell, _, []} | _]) when ?SPACE(Head) ->
  parse(Tail, Stack);

% forced inline elements
parse([${, Mark | Tail], ?INLINE_STACK(Stack))
  when ?SPAN_MARK(Mark) ->
  parse(Tail, ?INLINE_STACK([{opener, [${, Mark]} | Stack]));
parse([Mark, $} | Tail], ?INLINE_STACK(Stack))
  when ?SPAN_MARK(Mark) ->
  close(Tail, Stack, [],
        [${, Mark], span_element(Mark), [Mark, $}], ?INLINE_STACK([]));

% emphases and smart quotes
parse([Mark | Tail = [Next | _]],
      ?INLINE_STACK(Stack = [{text, [Prev | _]} | _]))
  when ?NONQT_LIGHT_SPAN_MARK(Mark)andalso ?SPACE(Next) andalso ?SPACE(Prev) ->
  parse(Tail, ?INLINE_STACK(push_char(Stack, Mark)));
parse([Mark | Tail = [Next | _]],
      ?INLINE_STACK(Stack = [{soft_break} | _]))
  when ?NONQT_LIGHT_SPAN_MARK(Mark) andalso ?SPACE(Next) ->
  parse(Tail, ?INLINE_STACK(push_char(Stack, Mark)));
parse([Mark | Tail = [Next | _]],
      ?INLINE_STACK(Stack = [{non_breaking_space} | _]))
  when ?NONQT_LIGHT_SPAN_MARK(Mark) andalso ?SPACE(Next) ->
  parse(Tail, ?INLINE_STACK(push_char(Stack, Mark)));

parse([Mark | Tail], ?INLINE_TOP(Top = {text, [Prev | _]}))
  when ?LIGHT_SPAN_MARK(Mark) andalso ?SPACE(Prev) ->
  parse(Tail, ?INLINE_TOP({opener, [Mark]}, Top));
parse([Mark | Tail], ?INLINE_TOP(Top = {soft_break}))
  when ?LIGHT_SPAN_MARK(Mark) ->
  parse(Tail, ?INLINE_TOP({opener, [Mark]}, Top));
parse([Mark | Tail], ?INLINE_TOP(Top = {non_breaking_space}))
  when ?LIGHT_SPAN_MARK(Mark) ->
  parse(Tail, ?INLINE_TOP({opener, [Mark]}, Top));
parse([Mark | Tail], ?INLINE_TOP(Top = {opener, [Mark]}))
  when ?LIGHT_SPAN_MARK(Mark) ->
  parse(Tail, ?INLINE_TOP({opener, [Mark]}, Top));

parse([Mark = $' | Tail], ?INLINE_STACK(Stack = [{text, [Prev | _]} | _]))
  when Prev =/= $- andalso Prev =/= $( ->
  close(Tail, Stack, [],
        [Mark], span_element(Mark), [Mark], ?INLINE_STACK([]));

parse([Mark | Tail = [Next | _]], ?INLINE_STACK(Stack))
  when ?LIGHT_SPAN_MARK(Mark) andalso ?SPACE(Next) ->
  close(Tail, Stack, [],
        [Mark], span_element(Mark), [Mark], ?INLINE_STACK([]));
parse([Mark | Tail], ?INLINE_STACK(Stack))
  when ?LIGHT_SPAN_MARK(Mark) ->
  close_or_open(Tail, Stack, [],
                [Mark], span_element(Mark), ?INLINE_STACK([]));

% smart punctuation
parse("..." ++ Tail, ?INLINE_STACK(Stack)) ->
  Element = {smart_punctuation, [{"type", "ellipsis"}], "..."},
  parse(Tail, ?INLINE_STACK([Element | Stack]));
parse([$- | Tail = [$- | _]], ?INLINE_TOP({dashes, N})) ->
  parse(Tail, ?INLINE_TOP({dashes, N + 1}));
parse([$- | Tail = [$-, Next | _]], ?INLINE_STACK(Stack)) when Next =/= $} ->
  parse(Tail, ?INLINE_STACK([{dashes, 1} | Stack]));
parse([$- | Tail = [Next | _]], ?INLINE_STACK([{dashes, N} | Stack])) ->
  Level = case Next of $} -> N; _ -> N + 1 end,
  parse(Tail, ?INLINE_STACK(lists:reverse(smart_dashes(Level)) ++ Stack));


% bracketed expressions
parse(Input = "[^" ++ Tail, ?INLINE_STACK(Stack)) ->
  parse(Tail, ?INLINE_STACK([{ref, $], "", footnote, [],
                              {"\\" ++ Input, ?INLINE_STACK(Stack)}}
                             | Stack]));
parse("![" ++ Tail, ?INLINE_STACK(Stack)) ->
  parse(Tail, ?INLINE_STACK([{opener, "["}, {opener, "!"} | Stack]));
parse("[" ++ Tail, ?INLINE_STACK(Stack)) ->
  parse(Tail, ?INLINE_STACK([{opener, "["} | Stack]));
parse("]{" ++ Tail, ?INLINE_STACK(Stack)) ->
  close("{" ++ Tail, Stack, [], "[", span, "]", ?INLINE_STACK([]));
parse("](" ++ Tail, ?INLINE_STACK(Stack)) ->
  close("(" ++ Tail, Stack, [], "[", brackets, "]", ?INLINE_STACK([]));
parse("][" ++ Tail, ?INLINE_STACK(Stack)) ->
  close("[" ++ Tail, Stack, [], "[", brackets, "]", ?INLINE_STACK([]));

parse([Head | Tail], ?INLINE_STACK(Stack)) ->
  parse(Tail, ?INLINE_STACK(push_char(Stack, Head))).

parse(Input) -> finalize(parse(Input, start_state())).

start_state() -> [{newline, []}, {doc, [], []}].
finalize(State) -> post_process(finish_doc(State)).

% AST post-processing

post_process(Doc_1) ->
  State_0 = {#{}, #{}, []},
  {Doc_2, State_1} = update_node(Doc_1, State_0,
                                 fun pre_gather/2, fun nop/2),
  {Doc_3, State_2} = update_node(Doc_2, State_1,
                                  fun pre_resolve/2, fun nop/2),
  {Doc_4, []} = update_node(Doc_3, [], fun pre_list/2, fun post_list/2),
  {add_sections(Doc_4), State_2}.

nop(Node, State) -> {Node, State}.

pre_gather(Node = {heading, _, _}, State) -> add_heading_id(Node, State);
pre_gather(Node = {reference_definition,
                   [{"reference", Name} | Attributes],
                   Target},
           State) ->
  {Node, record_ref(Name, [{"target", Target} | Attributes], State)};
pre_gather(Node = {_, Attributes, _}, State) ->
  {Node, record_attr_id(Attributes, State)};
pre_gather(Node, State) -> {Node, State}.

pre_resolve({Element, Attributes = [{"reference", ""} | _], Contents}, State)
  when Element =:= img orelse Element =:= link ->
  Ref = content_ref(Contents, [], []),
  resolve_link(Ref, Element, Attributes, Contents, State);
pre_resolve({Element, Attributes = [{"reference", Ref} | _], Contents}, State)
  when Element =:= img orelse Element =:= link ->
  resolve_link(Ref, Element, Attributes, Contents, State);
pre_resolve(Node, State) -> {Node, State}.

pre_list({list,
          [{marker, ":"}, {start, _}, {tight, _} | Attributes],
          Contents},
         State) ->
  {{definition_list, Attributes, Contents}, [definition_list | State]};
pre_list(Node = {list, [{marker, _}, {start, _}, Attr = {tight, _} | _], _},
         State) ->
  {Node, [Attr | State]};
pre_list({list_item, Attributes, [{para, Text} | Rest]},
         State = [definition_list | _]) ->
  {{definition_list_item, Attributes, [{term, Text}, {definition, Rest}]},
   State};
pre_list({list_item, Attributes, [{para, _, Text} | Rest]},
         State = [definition_list | _]) ->
  {{definition_list_item, Attributes, [{term, Text}, {definition, Rest}]},
   State};
pre_list({list_item, Attributes, Contents},
         State = [definition_list | _]) ->
  {{definition_list_item, Attributes, [{term, ""}, {definition, Contents}]},
   State};
pre_list({list_item, Attributes, Contents}, State = [Attr | _]) ->
  {{list_item, [Attr | Attributes], Contents}, State};
pre_list(Node, State) -> {Node, State}.

post_list(Node = {definition_list, _, _}, [_ | State]) -> {Node, State};
post_list(Node = {list, _, _}, [_ | State]) -> {Node, State};
post_list(Node, State) -> {Node, State}.

attribute_map([], Attr_Map) -> Attr_Map;
attribute_map([{Key, Value} | Tail], Attr_Map) ->
  attribute_map(Tail, maps:put(Key, Value, Attr_Map)).

resolve_link(Ref, Element, Attributes, Contents, State = {Id_Map, Ref_Map, Warnings}) ->
  case maps:find(Ref, Ref_Map) of
    error -> {{Element, Attributes, Contents},
              {Id_Map, Ref_Map,
               ["Unknown link reference \"" ++ Ref ++ "\"" | Warnings]}};
    {ok, Value} -> resolve_link(lists:reverse(Value), Attributes,
                                attribute_map(Attributes, #{}),
                                Element, Contents, State)
  end.
resolve_link([], Attributes, _, Element, Contents, State) ->
  {{Element, Attributes, Contents}, State};
resolve_link([{Key, Value} | Tail], Seen, Seen_Map, Element, Contents, State) ->
  case maps:is_key(Key, Seen_Map) of
    true -> resolve_link(Tail, Seen, Seen_Map, Element, Contents, State);
    false -> resolve_link(Tail, [{Key, Value} | Seen],
                          maps:put(Key, Value, Seen_Map), Element, Contents, State)
  end.


finish_doc([{terminated}, {doc, Attributes, Contents}]) ->
  {doc, Attributes, lists:reverse(Contents)};
finish_doc([{doc, Attributes, Contents}]) ->
  {doc, Attributes, lists:reverse(Contents)};
finish_doc([{newline, []} | Stack]) -> finish_doc(Stack);
finish_doc(Stack) ->
  [Doc = {doc, _, _} | Tail] = lists:reverse(Stack),
  finish_doc(close_blocks([], [{terminated}], [Doc], lists:reverse(Tail))).

content_id([], [$- | Acc]) -> lists:reverse(Acc);
content_id([], Acc) -> lists:reverse(Acc);
content_id([Head | Tail], Acc) when ?ALPHANUM(Head) ->
  content_id(Tail, [Head | Acc]);
content_id([_ | Tail], Acc = []) ->
  content_id(Tail, Acc);
content_id([_ | Tail], Acc = [$- | _]) ->
  content_id(Tail, Acc);
content_id([_ | Tail], Acc) ->
  content_id(Tail, [$- | Acc]).

content_ref([], [], [32 | Acc]) -> lists:reverse(Acc);
content_ref([], [], Acc) -> lists:reverse(Acc);
content_ref(Stack, [Head | Tail], Acc) when not ?SPACE(Head) ->
  content_ref(Stack, Tail, [Head | Acc]);
content_ref(Stack, [_ | Tail], Acc = []) ->
  content_ref(Stack, Tail, Acc);
content_ref(Stack, [_ | Tail], Acc = [32 | _]) ->
  content_ref(Stack, Tail, Acc);
content_ref(Stack, [_ | Tail], Acc) ->
  content_ref(Stack, Tail, [32 | Acc]);
content_ref([{text, Text} | Tail], [], Acc) ->
  content_ref(Tail, Text, Acc);
content_ref([{Element} | Tail], [], Acc = [32 | _])
   when Element =:= soft_break orelse Element =:= hard_break ->
  content_ref(Tail, [], Acc);
content_ref([{Element} | Tail], [], Acc)
   when Element =:= soft_break orelse Element =:= hard_break ->
  content_ref(Tail, [], [32 | Acc]);
content_ref([{_, Contents} | Tail], [], Acc) ->
  content_ref(Contents ++ Tail, [], Acc);
content_ref([{_, _, Contents} | Tail], [], Acc) ->
  content_ref(Contents ++ Tail, [], Acc).

unique_id([], State) -> unique_id("s", 1, State);
unique_id(Key, State = {Id_Map, _, _}) ->
  case maps:is_key(Key, Id_Map) of
    false -> {Key, record_id(Key, State)};
    true -> unique_id(Key, 1, State)
  end.
unique_id(Base, N, State = {Id_Map, _, _}) ->
  Key = lists:flatten(io_lib:format("~s-~p", [Base, N])),
  case maps:is_key(Key, Id_Map) of
     false -> {Key, record_id(Key, State)};
     true -> unique_id(Base, N + 1, State)
  end.

record_id(Id, {Id_Map, Ref_Map, Warnings}) ->
  New_Warnings = case maps:is_key(Id, Id_Map) of
    true -> ["Duplicate id \"" ++ Id ++ "\"" | Warnings];
    false -> Warnings
  end,
  {maps:put(Id, '', Id_Map), Ref_Map, New_Warnings}.

record_ref(Ref, Attributes, {Id_Map, Ref_Map, Warnings}) ->
  New_Warnings = case maps:is_key(Ref, Ref_Map) of
    true -> ["Duplicate reference \"" ++ Ref ++ "\"" | Warnings];
    false -> Warnings
  end,
  {Id_Map, maps:put(Ref, Attributes, Ref_Map), New_Warnings}.

record_attr_id([], State) -> State;
record_attr_id([{"identifier", Id} | Tail], State) ->
  record_attr_id(Tail, record_id(Id, State));
record_attr_id([_ | Tail], State) ->
  record_attr_id(Tail, State).

extract_heading_id(Node = {heading, [{level, _} | Attributes], _}) ->
  extract_heading_id(Attributes, [], Node).
%extract_heading_id([], _, Node) -> {content_id([Node], [], []), Node};
extract_heading_id([{"identifier", Id} | Tail],
                   Seen,
                   {heading, [{level, Level} | _], Contents}) ->
  {Id, {heading, [{level, Level} | lists:reverse(Seen) ++ Tail], Contents}};
extract_heading_id([Head | Tail], Seen, Node) ->
  extract_heading_id(Tail, [Head | Seen], Node).

add_heading_id(Node = {heading, [{level, _} | Attributes], _}, State) ->
  add_heading_id(Attributes, Node, State).
add_heading_id([],
               Node = {heading, [{level, Level} | Attributes], Contents},
               State) ->
  Content_Ref = content_ref([Node], [], []),
  {Id, Mid_State} = unique_id(content_id(Content_Ref, []), State),
  New_State = record_ref(Content_Ref, [{"target", "#" ++ Id}], Mid_State),
  {{heading, [{level, Level}, {"identifier", Id} | Attributes], Contents},
   New_State};
add_heading_id([{"identifier", Id} | _], Node, State) ->
  Content_Ref = content_ref([Node], [], []),
  {Node,
   record_ref(Content_Ref, [{"target", "#" ++ Id}], record_id(Id, State))};
add_heading_id([_ | Tail], Node, State) -> add_heading_id(Tail, Node, State).

add_sections({doc, Attributes, Contents}) ->
  {doc, Attributes, add_sections(Contents, [{0, [], []}])}.
add_sections([], [{0, [], Acc}]) -> lists:reverse(Acc);
add_sections([], [{Top_Level, Top_Id, Top_Contents},
                  {Next_Level, Next_Id, Next_Contents}
                  | Acc]) ->
  add_sections([],
               [{Next_Level, Next_Id,
                 [{section,
                   [{level, Top_Level}, {"identifier", Top_Id}],
                   lists:reverse(Top_Contents)} | Next_Contents]}
                | Acc]);
add_sections([Top = {heading, [{level, New_Level} | _], _} | Tail],
             Acc = [{Cur_Level, _, _} | _])
  when New_Level > Cur_Level ->
  {Id, Updated_Top} = extract_heading_id(Top),
  add_sections(Tail, [{New_Level, Id, [Updated_Top]} | Acc]);
add_sections(Stack = [{heading, [{level, _} | _], _} | _],
             [{Top_Level, Top_Id, Top_Contents},
              {Next_Level, Next_Id, Next_Contents} | Acc]) ->
  add_sections(Stack,
               [{Next_Level, Next_Id,
                 [{section,
                   [{level, Top_Level}, {"identifier", Top_Id}],
                   lists:reverse(Top_Contents)} | Next_Contents]}
                | Acc]);
add_sections([Head | Tail], [{Level, Id, Contents} | Acc]) ->
  add_sections(Tail, [{Level, Id, [Head | Contents]} | Acc]).

% AST query

walk_ast([], State, _, _) -> State;
walk_ast([Head | Tail], State, Pre, Post) ->
  walk_ast(Tail, walk_ast(Head, State, Pre, Post), Pre, Post);
walk_ast(Node = {_}, State, Pre, Post) ->
  Middle_State = Pre(Node, State),
  Post(Node, Middle_State);
walk_ast(Node = {Element, _}, State, Pre, Post)
  when ?CONTAINS_TEXT(Element) ->
  Middle_State = Pre(Node, State),
  Post(Node, Middle_State);
walk_ast(Node = {_, Contents}, State, Pre, Post) ->
  Middle_State = Pre(Node, State),
  Post_State = walk_ast(Contents, Middle_State, Pre, Post),
  Post(Node, Post_State);
walk_ast(Node = {Element, _, _}, State, Pre, Post)
  when ?CONTAINS_TEXT(Element) ->
  Middle_State = Pre(Node, State),
  Post(Node, Middle_State);
walk_ast(Node = {_, _, Contents}, State, Pre, Post) ->
  Middle_State = Pre(Node, State),
  Post_State = walk_ast(Contents, Middle_State, Pre, Post),
  Post(Node, Post_State).

% AST update

update_ast(Doc, State, Pre, Post) -> update_node(Doc, State, Pre, Post).

prepend(List, Acc) when is_list(List) -> List ++ Acc;
prepend(Item, Acc) -> [Item | Acc].

update_node(Node, State, Pre, Post) ->
  {Node_1, State_1} = Pre(Node, State),
  post_update_nodes(Node_1, [], State_1, Pre, Post).

post_update_nodes([], Done, State, _, _) -> {lists:reverse(Done), State};
post_update_nodes([Head | Tail], Done, State, Pre, Post) ->
  {New_Head, New_State} = update_children(Head, State, Pre, Post),
  {Post_Head, Post_State} = Post(New_Head, New_State),
  post_update_nodes(Tail, prepend(Post_Head, Done), Post_State, Pre, Post);
post_update_nodes(Node, [], State, Pre, Post) ->
  {New_Node, New_State} = update_children(Node, State, Pre, Post),
  Post(New_Node, New_State).

update_nodes([], Done, State, _, _) -> {lists:reverse(Done), State};
update_nodes([Head | Tail], Done, State, Pre, Post) ->
  {New_Head, New_State} = update_node(Head, State, Pre, Post),
  update_nodes(Tail, prepend(New_Head, Done), New_State, Pre, Post).

update_children(Node = {_}, State, _, _) -> {Node, State};
update_children(Node = {Element, _}, State, _, _)
  when ?CONTAINS_TEXT(Element) ->
  {Node, State};
update_children({Element, Contents}, State, Pre, Post) ->
  {New_Contents, New_State} = update_nodes(Contents, [], State, Pre, Post),
  {{Element, New_Contents}, New_State};
update_children(Node = {Element, _, _}, State, _, _)
  when ?CONTAINS_TEXT(Element) ->
  {Node, State};
update_children({Element, Attributes, Contents}, State, Pre, Post) ->
  {New_Contents, New_State} = update_nodes(Contents, [], State, Pre, Post),
  {{Element, Attributes, New_Contents}, New_State}.
