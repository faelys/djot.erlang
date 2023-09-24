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
-export([parse/1, parse/2]).

-define(SPACE(C), (C =:= 9 orelse C =:= 10 orelse C =:= 13 orelse C =:= 32)).
-define(PUNCTUATION(C), ((C >= $! andalso C =< $/)
                  orelse (C >= $: andalso C =< $@)
                  orelse (C >= $[ andalso C =< $`)
                  orelse (C >= ${ andalso C =< $~))).
-define(ALPHA(C), ((C >= $A andalso C =< $Z)
            orelse (C >= $a andalso C =< $z))).
-define(NUM(C), (C >= $0 andalso C =< $9)).
-define(ALPHANUM(C), (?ALPHA(C) orelse ?NUM(C))).

-define(IS_BLOCK(Element), (Element =:= blockquote
                     orelse Element =:= doc
                     orelse Element =:= fenced_div
                     orelse Element =:= footnote
                     orelse Element =:= heading
                     orelse Element =:= list
                     orelse Element =:= list_item
                     orelse Element =:= para)).

-define(CONTAINS_BLOCK(Element), (Element =:= blockquote
                           orelse Element =:= doc
                           orelse Element =:= fenced_div
                           orelse Element =:= footnote
                           orelse Element =:= list
                           orelse Element =:= list_item)).

-define(CONTAINS_INLINE(Element), (Element =:= heading
                            orelse Element =:= para)).

-define(SPAN_MARK(Mark), (Mark =:= $_ orelse Mark =:= $* orelse Mark =:= $^
                   orelse Mark =:= $~ orelse Mark =:= $= orelse Mark =:= $-
                   orelse Mark =:= $+ orelse Mark =:= $" orelse Mark =:= $')).

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
span_element($') -> simple_quoted.


finalize_attributes([], Acc) -> lists:reverse(Acc);
finalize_attributes([{fence, _} | Tail], Acc) ->
  finalize_attributes(Tail, Acc);
finalize_attributes([{indent, _} | Tail], Acc) ->
  finalize_attributes(Tail, Acc);
finalize_attributes([Head | Tail], Acc) ->
  finalize_attributes(Tail, [Head | Acc]).

close_blocks(Input, Next,
             [{Element, Attributes, Contents} | Stack],
             [Block | To_Close],
             []) ->
  close_blocks(Input, Next,
               [{Element, Attributes, [Block | Contents]} | Stack],
               To_Close);
close_blocks(Input, Next, Stack,
             [{Block_Element, Block_Attributes, Block_Contents} | To_Close],
             [{fenced_code, [{fence, _, _} | Attributes], Contents} | Stack]) ->
  close_blocks(Input, Next, Stack,
               [{Block_Element, Block_Attributes,
                [{fenced_code, Attributes, Contents} | Block_Contents]}
                | To_Close],
               Stack);
close_blocks(Input, Next, Stack,
             [{Block_Element, Block_Attributes, Block_Contents} | To_Close],
             [{text, Text} | Stack]) ->
  close_blocks(Input, Next, Stack,
               [{Block_Element, Block_Attributes,
                  [{text, lists:reverse(Text)} | Block_Contents]}
                | To_Close],
               Stack);
close_blocks(Input, Next, Stack,
             [{Block_Element, Block_Attributes, Block_Contents} | To_Close],
             [{verbatim, _, opener, _} | Stack]) ->
  close_blocks(Input, Next, Stack,
               [{Block_Element, Block_Attributes,
                  [{verbatim, [], []} | Block_Contents]}
                | To_Close],
               Stack);
close_blocks(Input, Next, Stack,
             [{Block_Element, Block_Attributes, Block_Contents} | To_Close],
             [{verbatim, _, Text, _} | Stack]) ->
  close_blocks(Input, Next, Stack,
               [{Block_Element, Block_Attributes,
                  [{verbatim, [], lists:reverse(Text)} | Block_Contents]}
                | To_Close],
               Stack);
close_blocks(Input, Next, Stack,
             [{Block_Element, Block_Attributes, Block_Contents} | To_Close],
             [Element | Stack]) ->
  close_blocks(Input, Next, Stack,
               [{Block_Element, Block_Attributes, [Element | Block_Contents]}
                | To_Close],
               Stack).

close_blocks(Input, Next, Stack, []) ->
  parse(Input, [Next | Stack]);
close_blocks(Input, Next, Stack, [{Element, Attributes, Contents} | To_Close])
  when ?CONTAINS_INLINE(Element) ->
  close_blocks(Input, Next, Stack,
               [{Element, finalize_attributes(Attributes, []), []} | To_Close],
               Contents);
close_blocks(Input, Next,
             [{Block_Element, Block_Attributes, Block_Contents} | Stack],
             [{Element, Attributes, Contents} | To_Close]) ->
  Next_Block = {Element,
                finalize_attributes(Attributes, []),
                lists:reverse(Contents)},
  close_blocks(Input, Next,
               [{Block_Element,
                 Block_Attributes,
                 [Next_Block | Block_Contents]}
                | Stack],
               To_Close).

maybe_close_fence([10 | Tail], _, 0, _,
                  [{prefix, _, _, To_Match}, Block | Stack]) ->
  close_blocks(Tail, {newline, []}, Stack, [Block | To_Match]);
maybe_close_fence([Head | Tail], Mark, 0, Orig_Input, Stack)
  when Head =:= Mark orelse Mark =:= 32 orelse Mark =:= 9 ->
  maybe_close_fence(Tail, Mark, 0, Orig_Input, Stack);
maybe_close_fence([Mark | Tail], Mark, Needed, Orig_Input, Stack) ->
  maybe_close_fence(Tail, Mark, Needed - 1, Orig_Input, Stack);
maybe_close_fence(_, _, _, Orig_Input, Stack) ->
  parse(Orig_Input, Stack).


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
abort_openers([{opener, Value} | Tail], Acc) ->
  abort_openers(Tail, push_text(Acc, Value));
abort_openers([Head | Tail], Acc) ->
  abort_openers(Tail, [Head | Acc]).

close(Rest, [{opener, Mark} | Tail], Seen,
      Mark, Element, _, ?INLINE_STACK([])) ->
  parse(Rest, ?INLINE_STACK([{Element,
                              attributes,
                              abort_openers(lists:reverse(Seen), [])}
                             | Tail]));
close(Rest, [Head | Tail], Seen, Mark, Element, Alt, Blocks) ->
  close(Rest, Tail, [Head | Seen], Mark, Element, Alt, Blocks);
close(Rest, [], Seen, _, _, Alt, ?INLINE_STACK([])) ->
  parse(Rest, ?INLINE_STACK(push_text(lists:reverse(Seen), Alt))).

close_or_open(Rest, [], Seen, Mark, _Element, ?INLINE_STACK([])) ->
  parse(Rest, ?INLINE_STACK([{opener, Mark} | lists:reverse(Seen)]));
close_or_open(Rest, [{opener, Mark} | Tail], Seen,
              Mark, Element, ?INLINE_STACK([])) ->
  parse(Rest, ?INLINE_STACK([{Element,
                              attributes,
                              abort_openers(lists:reverse(Seen), [])}
                             | Tail]));
close_or_open(Rest, [Head | Tail], Seen, Mark, Element, Blocks) ->
  close_or_open(Rest, Tail, [Head | Seen], Mark, Element, Blocks).

%% Block continuation or termination

parse(Input, [{newline, Attributes} | Stack]) ->
  parse(Input, [{prefix, 0, Attributes, lists:reverse(Stack)}]);

parse(Input, [{prefix, Level, Attributes,
                       [Block = {Element, _, _} | To_Match]}
              | Stack])
  when Element =:= doc ->
  parse(Input, [{prefix, Level, Attributes, To_Match}, Block | Stack]);

parse([32 | Tail], [{prefix, Level, Attributes, To_Match} | Stack]) ->
  parse(Tail, [{prefix, Level + 1, Attributes, To_Match} | Stack]);
parse([9 | Tail], [{prefix, Level, Attributes, To_Match} | Stack]) ->
  parse(Tail, [{prefix, Level + 1, Attributes, To_Match} | Stack]);

% blockquote continuation
parse([$>, 32 | Tail],
      [{prefix, Level, Attributes, [Block = {blockquote, _, _} | To_Match]}
       | Stack]) ->
  parse(Tail,
        [{prefix, Level + 2, Attributes, To_Match}, Block | Stack]);

parse([$> | Tail = [10 | _]],
      [{prefix, Level, Attributes, [Block = {blockquote, _, _} | To_Match]}
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
        [Block = {fenced_div, [{fence, Target} | _], _} | To_Match]}
       | Stack]) ->
  maybe_close_fence(Input, $:, Target, Input,
                    [{prefix, Level, Attributes, To_Match}, Block | Stack]);

parse(Input,
      [{prefix, Level, Attributes,
        [Block = {fenced_code, [{fence, Target, Mark} | _], _} | To_Match]}
       | Stack]) ->
  maybe_close_fence(Input, Mark, Target, Input,
                    [{prefix, Level, Attributes, To_Match}, Block | Stack]);


% header continuation
parse([$# | Tail],
      Stack = [{prefix, _, _, [{heading, [{level, Level}|_], _} | _]}, _]) ->
  parse(Tail,
        [{heading, 1, Level, "#"} | Stack]);
parse(Input = [32 | _],
      [Top = {heading, N, N, _},
       {prefix, Level, Attributes, [Block | To_Match]} | Stack]) ->
  parse(Input,
        [Top, {prefix, Level, Attributes, To_Match}, Block | Stack]);
parse([$# | Tail],
      [{heading, Current, Target, Text} | Stack = [{prefix, _, _, _} | _]])
  when Current < Target ->
  parse(Tail,
        [{heading, Current + 1, Target, [$# | Text]} | Stack]);
parse(Input, [{heading, _, _, Text},
              {prefix, Level, Attributes, To_Match} | Stack]) ->
  close_blocks(lists:reverse(Text) ++ Input,
               {old_newline, Level, Attributes},
               Stack,
               To_Match);

% footnote continuation
parse(Input,
      [{prefix, Indent_Level, Attributes,
         [Block = {footnote, [{indent, Block_Level} | _], _} | To_Match]}
       | Stack])
  when Indent_Level > Block_Level ->
  parse(Input,
        [{prefix, Indent_Level, Attributes, To_Match}, Block | Stack]);

% link reference continuation
parse(Input,
      [{prefix, Indent_Level, Attributes,
         [Block = {link_reference, [{indent, Block_Level} | _], _} | To_Match]}
       | Stack])
  when Indent_Level > Block_Level ->
  parse(Input,
        [{prefix, Indent_Level, Attributes, To_Match}, Block | Stack]);

% list continuation
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

parse(Input = [Marker, Space | _],
      [{prefix, Level, Attributes,
         [List = {list, [{marker, [Marker]} | _], _} | To_Match]}
       | Stack])
  when Space =:= 32 orelse Space =:= 9 ->
  close_blocks(Input,
               {newblock, Level, Attributes},
               [List | Stack],
               To_Match);

% continuation through empty lines
parse(Input = [10 | _],
      [{prefix, Level, Attributes, [Block = {Element, _, _} | To_Match]}
       | Stack])
  when        Element =:= footnote orelse Element =:= list_item
       orelse Element =:= list ->
  parse(Input,
        [{prefix, Level, Attributes, To_Match}, Block | Stack]);

% everything else is not matched
parse(Input, [{prefix, Level, Attributes, To_Match} | Stack]) ->
  parse(Input, [{matched, Level, Attributes, To_Match} | Stack]);

%% Block-level closings

parse([10 | Tail],
      [{matched, _, _, _},
       {link_reference,
        [{indent, Level}, {"target", Link} | Attributes],
        Contents = [_|_]}
       | Stack]) ->
  parse(Tail,
        [{newline, []},
         {link_reference, [{indent, Level},
                           {"target", Link ++ lists:reverse(Contents)}
                            | Attributes],  []}
         | Stack]);
parse([Head | Tail],
      [Prev = {matched, _, _, _},
       {link_reference, Attributes, Contents} | Stack])
  when Head =/= 32 andalso Head =/= 9 ->
  parse(Tail,
        [Prev, {link_reference, Attributes, [Head | Contents]} | Stack]);

parse(Input,
      [Prev = {matched, _, _, _},
       {link_reference, [{indent, _} | Inner_Attributes], Backtrack},
       {Element, Attributes, Contents} | Stack]) ->
  parse(lists:reverse(Backtrack) ++ Input,
        [Prev,
         {Element, Attributes,
          [{link_reference, Inner_Attributes, []} | Contents]}
         | Stack]);

% thematic break

parse(Input = [Head | _],
      [{old_newline, Level, Attributes} | Stack = [{Element, _, _} | _]])
  when Head =:= 32 orelse Head =:= 9 orelse Head =:= $* orelse Head =:= $-
       andalso ?CONTAINS_BLOCK(Element) ->
  parse(Input, [{thematic_break, Level, Attributes, [], 0} | Stack]);

parse([Head | Tail],
      [{thematic_break, Level, Attributes, Raw, Level} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail,
        [{thematic_break, Level, Attributes, [Head | Raw], Level} | Stack]);
parse([Head | Tail], [{thematic_break, Level, Attributes, Raw, Level} | Stack])
  when Head =:= $* orelse Head =:= $- ->
  parse(Tail,
        [{thematic_break, Level, Attributes, [Head | Raw], Level + 1} | Stack]);

parse([10 | Tail], [{thematic_break, Level, Attributes, _, Level} | Stack])
  when Level >= 3 ->
  parse(Tail,
        [{newline, []}, {thematic_break, Level, Attributes, []} | Stack]);

parse(Input, [{thematic_break, Level, Attributes, Raw, _} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input, [{newblock, Level, Attributes} | Stack]);

%% end of line: closing structures and priming indent

parse(Input = [10 | _],
      [{fenced_div, Level, _, Attributes} | Stack]) ->
  parse(Input,
        [{fenced_div, [{fence, Level} | Attributes], []} | Stack]);
parse(Input = [10 | _],
      [{fenced_div, Level, _, [], Attributes} | Stack]) ->
  parse(Input,
        [{fenced_div, [{fence, Level} | Attributes], []} | Stack]);
parse(Input = [10 | _],
      [{fenced_div, Level, _, Class, Attributes} | Stack]) ->
  parse(Input,
        [{fenced_div, [{fence, Level}, {class, Class} | Attributes], []}
        | Stack]);
parse(Input = [10 | _],
      [{fenced_div, Level, _, Class, Attributes, done} | Stack]) ->
  parse(Input,
        [{fenced_div, [{fence, Level}, {class, Class} | Attributes], []}
        | Stack]);

parse([10 | Tail],
      [{fenced_code, Level, Mark, _, Attributes} | Stack]) ->
  New = {fenced_code, [{fence, Level, Mark} | Attributes], []},
  parse(Tail, [{newline, []}, New | Stack]);
parse([10 | Tail],
      [{fenced_code, Level, Mark, _, [], Attributes} | Stack]) ->
  New = {fenced_code, [{fence, Level, Mark} | Attributes], []},
  parse(Tail, [{newline, []}, New | Stack]);
parse([10 | Tail],
      [{fenced_code, Level, Mark, _, Alt, Attributes} | Stack]) ->
  New = {fenced_code,
         [{fence, Level, Mark}, {mark, lists:reverse(Alt)} | Attributes],
         []},
  parse(Tail, [{newline, []}, New | Stack]);
parse([10 | Tail],
      [{fenced_code, Level, Mark, _, Alt, Attributes, done} | Stack]) ->
  New = {fenced_code,
         [{fence, Level, Mark}, {mark, lists:reverse(Alt)} | Attributes],
         []},
  parse(Tail, [{newline, []}, New | Stack]);

parse(Input = [10 | _],
      [{footnote, _, Raw, _, pending},
       {newblock, _, Attributes} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input,
        [{para, Attributes, []} | Stack]);
parse([10 | Tail],
      [{footnote, _, _, Label, done},
       {newblock, Level, Attributes} | Stack]) ->
  New = {footnote, [{indent, Level},
                    {"label", lists:reverse(Label)}
                    | Attributes], []},
  parse(Tail, [{newline, []}, New | Stack]);

parse(Input = [10 | _],
      [{link_reference, Raw, _, pending},
       {newblock, _, Attributes} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input,
        [{para, Attributes, []} | Stack]);
parse([10 | Tail],
      [{link_reference, _, Label, space},
       {newblock, Level, Attributes} | Stack]) ->
  New = {link_reference, [{indent, Level},
                          {"target", []},
                          {"label", lists:reverse(Label)}
                          | Attributes], []},
  parse(Tail, [{newline, []}, New | Stack]);
parse([10 | Tail],
      [{link_reference, _, Label, Link},
       {newblock, Level, Attributes} | Stack]) ->
  New = {link_reference, [{indent, Level},
                          {"target", lists:reverse(Link)},
                          {"label", lists:reverse(Label)}
                          | Attributes], []},
  parse(Tail, [{newline, []}, New | Stack]);

parse([10 | Tail],
      [{block_attributes, Attributes, done, _} | Stack]) ->
  parse(Tail, [{newline, Attributes} | Stack]);
parse([10 | Tail], [{old_newline, _, Attributes} | Stack]) ->
  parse(Tail, [{newline, Attributes} | Stack]);
parse([10 | Tail], Stack) ->
  parse(Tail, [{newline, []} | Stack]);

%% Block-level openings

% general processing
parse(Input,
      [{matched, Level, Attributes, To_Match} | Stack = [{Element, _, _} | _]])
  when ?CONTAINS_BLOCK(Element) ->
  close_blocks(Input, {newblock, Level, Attributes}, Stack, To_Match);
parse(Input, [{matched, _, _}
              | Stack = [{_, _, [{hard_break} | _]} | _]]) ->
  parse(Input, Stack);
parse(Input, [{matched, _, _}, {Element, Attributes, Contents} | Stack]) ->
  parse(Input, [{Element, Attributes, [{soft_break} | Contents]} | Stack]);

% blockquote
parse([$>, 32 | Tail], [{newblock, Level, Attributes} | Stack]) ->
  parse(Tail,
        [{newblock, Level + 2, []}, {blockquote, Attributes, []} | Stack]);

% heading
parse([$# | Tail], [{newblock, _, Attributes} | Stack]) ->
  parse(Tail, [{heading, 1, "#", Attributes} | Stack]);
parse([$# | Tail], [{heading, Level, Text, Attributes} | Stack]) ->
  parse(Tail, [{heading, Level + 1, [$# | Text], Attributes} | Stack]);
parse([32 | Tail], [{heading, Level, _, Attributes} | Stack]) ->
  parse(Tail, [{heading, [{level, Level} | Attributes], []} | Stack]);
parse(Input, [{heading, _, Text, Attributes} | Stack]) ->
  parse(lists:reverse(Text) ++ Input,
        [{para, Attributes, []} | Stack]);

% div
parse(":::" ++ Tail, [{newblock, _, Attributes} | Stack]) ->
  parse(Tail, [{fenced_div, 3, ":::", Attributes} | Stack]);
parse([$: | Tail],
      [{fenced_div, Level, Raw, Attributes} | Stack]) ->
  parse(Tail, [{fenced_div, Level + 1, [$: | Raw], Attributes} | Stack]);
parse([Head | Tail], [{fenced_div, Level, Raw, Attributes} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail,
        [{fenced_div, Level, [Head | Raw], [], Attributes} | Stack]);
parse([Head | Tail], [{fenced_div, Level, Raw, [], Attributes} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail,
        [{fenced_div, Level, [Head | Raw], [], Attributes} | Stack]);
parse([Head | Tail],
      [{fenced_div, Level, Raw, Class, Attributes} | Stack])
  when ?ALPHANUM(Head) orelse Head =:= $- orelse Head =:= $_ ->
  parse(Tail,
        [{fenced_div, Level, [Head|Raw], [Head|Class], Attributes} | Stack]);
parse([Head | Tail],
      [{fenced_div, Level, Raw, Class, Attributes} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail,
        [{fenced_div, Level, [Head | Raw], Class, Attributes, done}
         | Stack]);
parse([Head | Tail],
      [{fenced_div, Level, Raw, Class, Attributes, done} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail,
        [{fenced_div, Level, [Head | Raw], Class, Attributes, done}
         | Stack]);
parse(Input,
      [{fenced_div, _, Raw, Attributes} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input,
        [{para, Attributes, []} | Stack]);
parse(Input,
      [{fenced_div, _, Raw, _, Attributes} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input,
        [{para, Attributes, []} | Stack]);
parse(Input,
      [{fenced_div, _, Raw, _, Attributes, done} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input,
        [{para, Attributes, []} | Stack]);

% code block
parse("```" ++ Tail, [{newblock, _, Attributes} | Stack]) ->
  parse(Tail, [{fenced_code, 3, $`, "```", Attributes} | Stack]);
parse("\~\~\~" ++ Tail, [{newblock, _, Attributes} | Stack]) ->
  parse(Tail, [{fenced_code, 3, $~, "\~\~\~", Attributes} | Stack]);
parse([Head | Tail], [{fenced_code, Level, Head, Raw, Attr} | Stack]) ->
  parse(Tail, [{fenced_code, Level + 1, Head, [Head | Raw], Attr} | Stack]);
parse([Head | Tail], [{fenced_code, Level, Mark, Raw, Attr} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail, [{fenced_code, Level, Mark, [Head | Raw], [], Attr} | Stack]);
parse([Head | Tail], [{fenced_code, Level, Mark, Raw, [], Attr} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail, [{fenced_code, Level, Mark, [Head | Raw], [], Attr} | Stack]);
parse([Head | Tail], [{fenced_code, Level, Mark, Raw, Alt, Attr} | Stack])
  when Head =/= 32 andalso Head =/= 9
       andalso (Head =/= $` orelse Mark =/= $`) ->
  parse(Tail,
        [{fenced_code, Level, Mark, [Head | Raw], [Head | Alt], Attr} | Stack]);
parse([Head | Tail], [{fenced_code, Level, Mark, Raw, Alt, Attr} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail,
        [{fenced_code, Level, Mark, [Head | Raw], Alt, Attr, done} | Stack]);
parse([Head | Tail],
      [{fenced_code, Level, Mark, Raw, Alt, Attr, done} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail,
        [{fenced_code, Level, Mark, [Head | Raw], Alt, Attr, done} | Stack]);

parse(Input, [{fenced_code, _, _, Raw, Attributes} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input, [{para, Attributes, []} | Stack]);
parse(Input, [{fenced_code, _, _, Raw, _, Attributes} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input, [{para, Attributes, []} | Stack]);
parse(Input, [{fenced_code, _, _, Raw, _, Attributes, done} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input, [{para, Attributes, []} | Stack]);

% footnote
parse("[^" ++ Tail, Stack = [{newblock, _, _} | _]) ->
  parse(Tail, [{footnote, 2, "^[", "^", pending} | Stack]);
parse("]:" ++ Tail, [{footnote, Level, Raw, Label, pending} | Stack]) ->
  parse(Tail, [{footnote, Level + 2, ":]" ++ Raw, Label, link} | Stack]);
parse([Head | Tail], [{footnote, Level, Raw, Label, pending} | Stack]) ->
  parse(Tail,
        [{footnote, Level + 1, [Head | Raw], [Head | Label], pending}
         | Stack]);
parse([Head | Tail], [{footnote, Level, Raw, Label, _} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail, [{footnote, Level + 1, [Head | Raw], Label, done} | Stack]);
parse(Input, [{footnote, _, Raw, Label, link} | Stack]) ->
  parse(Input, [{link_reference, Raw, Label, space} | Stack]);
parse(Input,
      [{footnote, Next_Level, _, Label, done},
       {newblock, Level, Attributes} | Stack]) ->
  parse(Input,
        [{newblock, Next_Level, []},
         {footnote,
          [{indent, Level},{"label", lists:reverse(Label)} | Attributes],
          []}
         | Stack]);

% link reference
parse("[" ++ Tail, Stack = [{newblock, _, _} | _]) ->
  parse(Tail, [{link_reference, "[", [], pending} | Stack]);
parse("]:" ++ Tail, [{link_reference, Raw, Label, pending} | Stack]) ->
  parse(Tail, [{link_reference, ":]" ++ Raw, Label, space} | Stack]);
parse(Input = "]" ++ _,
      [{link_reference, Raw, _, pending},
       {newblock, _, Attributes} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input, [{para, Attributes, []} | Stack]);
parse([Head | Tail], [{link_reference, Raw, Label, pending} | Stack]) ->
  parse(Tail,
        [{link_reference, [Head | Raw], [Head | Label], pending} | Stack]);
parse([Head | Tail], [{link_reference, Raw, Label, space} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail, [{link_reference, [Head | Raw], Label, space} | Stack]);
parse([Head | Tail], [{link_reference, Raw, Label, space} | Stack]) ->
  parse(Tail, [{link_reference, [Head | Raw], Label, [Head]} | Stack]);
parse(Input = [Head | _],
      [{link_reference, Raw, _Label, _Target},
       {newblock, _, Attributes} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(lists:reverse(Raw) ++ Input, [{para, Attributes, []} | Stack]);
parse([Head | Tail], [{link_reference, Raw, Label, Link} | Stack]) ->
  parse(Tail, [{link_reference, [Head | Raw], Label, [Head | Link]} | Stack]);

% list item
parse([Marker | [Space | Tail]],
      [{newblock, Level, Attributes}
       | Stack = [{list, [{marker, [Marker]} | _], _} |_]])
  when Space =:= 32 orelse Space =:= 9 ->
  parse(Tail,
        [{newblock, Level + 2, []},
         {list_item, [{indent, Level} | Attributes], []}
         | Stack]);

parse([Marker | [Space | Tail]],
      [{newblock, Level, Attributes} | Stack])
  when Space =:= 32 orelse Space =:= 9,
       Marker =:= $* orelse Marker =:= $+
         orelse Marker =:= $- orelse Marker =:= $: ->
  parse(Tail,
        [{newblock, Level + 2, []},
         {list_item, [{indent, Level}], []},
         {list, [{marker, [Marker]} | Attributes], []}
         | Stack]);

% block attributes
parse([${ | Tail = [Next | _]], Stack = [{newblock, _, _} | _])
  when ?SPACE(Next) orelse ?ALPHANUM(Next)
       orelse Next =:= $: orelse Next =:= $- orelse Next =:= $_
       orelse Next =:= $. orelse Next =:= $# orelse Next =:= $% ->
  parse(Tail, [{attributes, "{", []} | Stack]); % TODO

% fall back on paragraph
parse(Input, [{newblock, _, Attributes} | Stack]) ->
  parse(Input, [{para, Attributes, []} | Stack]);

%% Attributes

% fill-in attribute placeholder
parse(Input = [${ | Tail = [Next | _]],
       ?INLINE_STACK([{Element, attributes, Contents} | Stack]))
  when ?SPACE(Next) orelse ?ALPHANUM(Next)
       orelse Next =:= $: orelse Next =:= $- orelse Next =:= $_
       orelse Next =:= $. orelse Next =:= $# orelse Next =:= $% ->
  parse(Tail,
        ?INLINE_STACK([{attributes, [],
                         {Input,
                          ?INLINE_STACK([{Element, Contents} | Stack])}},
                       {Element, attributes, Contents} | Stack]));

% attribute dispatching
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
      ?INLINE_STACK([{attributes, Acc, _},
                     {Element, attributes, Contents} | Stack])) ->
  parse(Tail, [{Element, lists:reverse(Acc), Contents} | Stack]);

parse([$} | Tail],
      [{block_attributes, Old_Attr, [{attributes, New_Attr, Backtrack}]}
       | Stack]) ->
  parse(Tail, [{block_attributes,
                Old_Attr ++ lists:reverse(New_Attr),
                done,
                Backtrack}
               | Stack]);

parse([Head | Tail], Stack = [{block_attributes, _, done, _} | _])
  when ?SPACE(Head) ->
  parse(Tail, Stack);

parse(_, [{block_attributes, _, done, {Input, Stack}} | _]) ->
  parse(Input, Stack);

% name (identifier or class)
parse(Input = [$}|_],
      ?INLINE_TOP({name, Key, Value}, {attributes, Acc, Backtrack})) ->
  parse(Input,
        ?INLINE_TOP({attributes,
                     [{Key, lists:reverse(Value)} | Acc],
                     Backtrack}));

parse([Head|Tail],
      ?INLINE_TOP({name, Key, Value}, {attributes, Acc, Backtrack}))
  when ?SPACE(Head) ->
  parse(Tail,
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

parse("\\\"" ++ Tail,
      ?INLINE_TOP({quotedvalue, Key, Value}, Attr = {attributes, _, _})) ->
  parse(Tail, ?INLINE_TOP({quotedvalue, Key, [$" | Value]}, Attr));

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

% escape
parse([$\\ | [Head | Tail]], ?INLINE_STACK(Stack)) when ?PUNCTUATION(Head) ->
  parse(Tail, ?INLINE_STACK(push_char(Stack, Head)));
parse([$\\ | Tail = [10 | _]], Stack) ->
  parse(Tail, [{hard_break} | Stack]);
parse([$\\ | [13 | Tail = [10 | _]]], Stack) ->
  parse(Tail, [{hard_break} | Stack]);

% references TODO
parse(Input = "[" ++ Tail, [{brackets, Contents} | Stack]) ->
  parse(Tail, [{ref, $], "", link_ref, Contents, Input} | Stack]);
parse(Input = "(" ++ Tail, [{brackets, Contents} | Stack]) ->
  parse(Tail, [{ref, $), "", link, Contents, Input} | Stack]);
parse([Head | Tail], [{ref, Head, Text, Element, Contents, _} | Stack]) ->
  parse(Tail, [{ref, attributes, {Element, Text, Contents}} | Stack]);
parse([Head | Tail], [{ref, Mark, Text, Element, Contents, Input} | Stack]) ->
  parse(Tail,
        [{ref, Mark, [Head | Text], Element, Contents, Input} | Stack]);

parse(Input, [{ref, Data} | Stack]) -> parse(Input, [{ref, [], Data} | Stack]);

parse(Input, [{ref, Attributes, {link_ref, Text, Contents}} | Stack]) ->
  parse(Input, [{link,
                 [{"reference", lists:reverse(Text)} | Attributes],
                 Contents}
                | Stack]);
parse(Input, [{ref, Attributes, {link, Text, Contents}} | Stack]) ->
  parse(Input, [{link,
                 [{"target", lists:reverse(Text)} | Attributes],
                 Contents}
                | Stack]);

% verbatim span
parse("`" ++ Tail, ?INLINE_TOP({verbatim, N, opener, 0})) ->
  parse(Tail, ?INLINE_TOP({verbatim, N + 1, opener, 0}));
parse([Head | Tail], ?INLINE_TOP({verbatim, N, opener, 0})) ->
  parse(Tail, ?INLINE_TOP({verbatim, N, [Head], 0}));

parse("`" ++ Tail, ?INLINE_TOP({verbatim, N, " ", 0})) when N > 1 ->
  parse(Tail, ?INLINE_TOP({verbatim, N, "`", 1}));

parse("`" ++ Tail, ?INLINE_TOP({verbatim, N, Text, Closing})) ->
  parse(Tail, ?INLINE_TOP({verbatim, N, Text, Closing + 1}));
parse(Input, ?INLINE_TOP({verbatim, N, " `" ++ Text, N})) ->
  parse(Input, ?INLINE_TOP({verbatim, attributes, lists:reverse("`" ++ Text)}));
parse(Input, ?INLINE_TOP({verbatim, N, Text, N})) ->
  parse(Input, ?INLINE_TOP({verbatim, attributes, lists:reverse(Text)}));

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

% forced inline elements
parse([${, Mark | Tail], ?INLINE_STACK(Stack))
  when ?SPAN_MARK(Mark) ->
  parse(Tail, ?INLINE_STACK([{opener, [${, Mark]} | Stack]));
parse([Mark, $} | Tail], ?INLINE_STACK(Stack))
  when ?SPAN_MARK(Mark) ->
  close(Tail, Stack, [],
        [${, Mark], span_element(Mark), [Mark, $}], ?INLINE_STACK([]));

parse("^" ++ Tail, ?INLINE_STACK(Stack)) ->
  close_or_open(Tail, Stack, [], "^", superscript, ?INLINE_STACK([]));

% bracketed expressions
parse("![" ++ Tail, Stack) ->
  parse(Tail, [{opener, "["}, {opener, "!"} | Stack]);
parse("[" ++ Tail, Stack) -> parse(Tail, [{opener, "["} | Stack]);
parse("]{" ++ Tail, Stack) -> close("{" ++ Tail, Stack, [], "[", span, "]", []);
parse("](" ++ Tail, Stack) -> close("(" ++ Tail, Stack, [], "[", brackets, "]", []);
parse("][" ++ Tail, Stack) -> close("[" ++ Tail, Stack, [], "[", brackets, "]", []);

parse([Head | Tail], ?INLINE_STACK(Stack)) ->
  parse(Tail, ?INLINE_STACK(push_char(Stack, Head)));

parse([], [{terminated}, Result = {doc, _, _}]) -> Result;
parse([], [Result = {doc, _, _}]) -> Result;
parse([], Stack) ->
  [Doc = {doc, _, _} | Tail] = lists:reverse(Stack),
  close_blocks([], {terminated}, [Doc], Tail).

parse(Input) -> parse(Input, [{newline, 0, []}, {doc, [], []}]).
