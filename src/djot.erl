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


finalize_attributes([], Acc) -> lists:reverse(Acc);
finalize_attributes([{fence, _} | Tail], Acc) ->
  finalize_attributes(Tail, Acc);
finalize_attributes([{indent, _} | Tail], Acc) ->
  finalize_attributes(Tail, Acc);
finalize_attributes([Head | Tail], Acc) ->
  finalize_attributes(Tail, [Head | Acc]).

finalize_block({Element, Attributes, Contents}) ->
  {Element, finalize_attributes(Attributes, []), lists:reverse(Contents)}.

close_blocks(Current = [_|_], []) -> Current;

close_blocks([{Element, Attributes, Contents} | Blocks],
             [Inner_Block = {Inner_Element, _, _}])
  when ?IS_BLOCK(Inner_Element) ->
  [{Element, Attributes, [finalize_block(Inner_Block) | Contents]} | Blocks];

close_blocks(Current = [_|_], [Block = {Element, _, _} | Tail])
  when ?IS_BLOCK(Element) ->
  close_blocks(Current, close_blocks([Block], Tail));

close_blocks([{Element, Attributes, Contents} | Blocks],
             [{fenced_code, [{fence, _, _} | Inner_Attributes], Inner_Contents}
              | Tail]) ->
  close_blocks([{Element,
                 Attributes,
                 [{fenced_code, Inner_Attributes, Inner_Contents} | Contents]}
                | Blocks],
               Tail);
close_blocks(_, [{ref, _Mark, _Text, _Element, Contents, Input} | Stack]) ->
  [{failed_ref, Input, Contents} | Stack];
close_blocks([{Element, Attributes, Contents} | Blocks],
             [{text, Text} | Tail]) ->
  close_blocks([{Element, Attributes, [{text, lists:reverse(Text)} | Contents]}
                | Blocks],
               Tail);
close_blocks([{Element, Attributes, Contents} | Blocks],
             [{verbatim, _, opener, _} | Tail]) ->
  close_blocks([{Element, Attributes, [{verbatim, [], []} | Contents]}
                | Blocks],
               Tail);
close_blocks([{Element, Attributes, Contents} | Blocks],
             [{verbatim, _, Text, _} | Tail]) ->
  close_blocks([{Element,
                 Attributes,
                 [{verbatim, [], lists:reverse(Text)} | Contents]}
                | Blocks],
               Tail);
close_blocks([{Element, Attributes, Contents} | Blocks], [Head | Tail]) ->
  close_blocks([{Element, Attributes, [Head | Contents]} | Blocks],
               Tail).

fence_not_closing(Input, _Raw, Level, Attributes,
                  Stack = [{Element, _, _} | _])
  when ?CONTAINS_BLOCK(Element) ->
  parse(Input,
        [{indent, 0, "", [], []}
         | lists:reverse([{fenced_div, [{fence, Level} | Attributes], []}
                          | Stack])]);

fence_not_closing(Input, Raw, _Level, _Attributes,
                  [{newline, _, _} | [Stack = [{hard_break} | _]]]) ->
  parse(lists:reverse(Raw) ++ Input, Stack);

fence_not_closing(Input, Raw, _Level, _Attributes, [{newline, _, _} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input, [{soft_break} | Stack]).

close_fenced_div(Input, Raw, Level, Attributes, [], Seen) ->
  fence_not_closing(Input, Raw, Level, Attributes, lists:reverse(Seen));
close_fenced_div(Input, _Raw, Level, _Attributes,
                 [Div = {fenced_div, [{fence, Level}|_], _} | Stack],
                 Seen) ->
  parse(Input,
        [{indent, 0, "", [], []}
         | lists:reverse(close_blocks(Stack, [Div | Seen]))]);
close_fenced_div(Input, Raw, Level, Attributes, [Head | Tail], Seen) ->
  close_fenced_div(Input, Raw, Level, Attributes, Tail, [Head | Seen]).

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

close(Rest, [{opener, Mark} | Tail], Seen, Mark, Element, _) ->
  parse(Rest, [{Element,
                attributes,
                abort_openers(lists:reverse(Seen), [])}
               | Tail]);
close(Rest, [Head | Tail], Seen, Mark, Element, Alt) ->
  close(Rest, Tail, [Head | Seen], Mark, Element, Alt);
close(Rest, [], Seen, _, _, Alt) ->
  parse(Rest, push_text(lists:reverse(Seen), Alt)).

close_or_open(Rest, [], Seen, Mark, _Element) ->
  parse(Rest, [{opener, Mark} | lists:reverse(Seen)]);
close_or_open(Rest, [{opener, Mark} | Tail], Seen, Mark, Element) ->
  parse(Rest, [{Element,
                attributes,
                abort_openers(lists:reverse(Seen), [])}
               | Tail]);
close_or_open(Rest, [Head | Tail], Seen, Mark, Element) ->
  close_or_open(Rest, Tail, [Head | Seen], Mark, Element).

%% Block continuation or termination

parse(Input, [{indent, Level, Text, Matched, Attributes},
              Block = {Element, _, _} | Stack])
  when Element =:= doc orelse Element =:= fenced_code
                       orelse Element =:= fenced_div ->
  parse(Input, [{indent, Level, Text, [Block | Matched], Attributes} | Stack]);

parse([32 | Tail], [{indent, Level, Text, Matched, Attributes} | Stack]) ->
  parse(Tail, [{indent, Level + 1, [32 | Text], Matched, Attributes} | Stack]);
parse([9 | Tail], [{indent, Level, Text, Matched, Attributes} | Stack]) ->
  parse(Tail, [{indent, Level + 1, [9 | Text], Matched, Attributes} | Stack]);

% blockquote continuation
parse([$>, 32 | Tail],
      [{indent, Level, Text, Matched, Attributes},
       Block = {blockquote, _, _}
       | Stack]) ->
  parse(Tail,
        [{indent, Level + 2, [32, $> | Text], [Block | Matched], Attributes} | Stack]);

parse([$> | Tail = [10 | _]],
      [{indent, Level, Text, Matched, Attributes},
       Block = {blockquote, _, _}
       | Stack]) ->
  parse(Tail,
        [{indent, Level + 2, [32, $> | Text], [Block | Matched], Attributes} | Stack]);

% paragraph continuation
parse(Input = [Next | _],
      [{indent, Level, Text, Matched, Attributes},
       Block = {para, _, _}
       | Stack])
  when not ?SPACE(Next) ->
  parse(Input,
        [{indent, Level, Text, [Block | Matched], Attributes} | Stack]);

% header continuation
parse([$# | Tail],
      Stack = [{indent, _, _, _, _}, {heading, [{level, Level}|_], _} | _]) ->
  parse(Tail,
        [{heading, 1, Level, "#"} | Stack]);
parse(Input = [32 | _],
      [Top = {heading, N, N, _},
       {indent, Level, Text, Matched, Attributes},
       Element | Stack]) ->
  parse(Input,
        [Top, {indent, Level, Text, [Element | Matched], Attributes} | Stack]);
parse([32 | Tail],
      [{heading, N, N, _},
       {indent, _Level, _Text, Matched, _Attributes}]) ->
  parse(Tail, Matched);
parse([$# | Tail],
      [{heading, Current, Target, Text} | Stack = [{indent, _, _, _, _} | _]])
  when Current < Target ->
  parse(Tail,
        [{heading, Current + 1, Target, [$# | Text]} | Stack]);
parse(Input, [{heading, _, _, Text},
              {indent, Level, _, Matched, Attributes} | Stack]) ->
  parse(lists:reverse(Text) ++ Input,
        [{newline, Level, Attributes} | close_blocks(Matched, Stack)]);

% footnote continuation
parse(Input,
      [{indent, Indent_Level, Text, Matched, Attributes},
       Block = {footnote, [{indent, Block_Level} | _], _} | Stack])
  when Indent_Level > Block_Level ->
  parse(Input,
        [{indent, Indent_Level, Text, [Block | Matched], Attributes} | Stack]);

% link reference continuation
parse(Input,
      [{indent, Indent_Level, Text, Matched, Attributes},
       Block = {link_reference, [{indent, Block_Level} | _], _} | Stack])
  when Indent_Level > Block_Level ->
  parse(Input,
        [{indent, Indent_Level, Text, [Block | Matched], Attributes} | Stack]);

% list continuation
parse(Input,
      [{indent, Indent_Level, Text, Matched, Attributes},
       List = {list, _, _},
       Item = {list_item, [{indent, Item_Level} | _], _}
       | Stack])
  when Indent_Level > Item_Level ->
  parse(Input,
        [{indent, Indent_Level, Text, [Item, List | Matched], Attributes}
         | Stack]);

parse(Input = [Marker, Space | _],
      [{indent, Level, _Text, Matched, Attributes},
       List = {list, [{marker, [Marker]} | _], _}
       | Stack])
  when Space =:= 32 orelse Space =:= 9 ->
  parse(Input,
        [{newline, Level, Attributes}
         | close_blocks([List | Matched], Stack)]);

% backtrack for failed reference
parse(_, [{newline, _, _}, {failed_ref, Input, Contents} | Stack]) ->
  parse("\\" ++ Input, [{brackets, Contents} | Stack]);

% continuation through empty lines
parse(Input = [10 | _],
      [{indent, Level, Text, Matched, Attributes},
       Block = {Element, _, _}
       | Stack])
  when        Element =:= footnote orelse Element =:= list_item
       orelse Element =:= list ->
  parse(Input,
        [{indent, Level, Text, [Block | Matched], Attributes} | Stack]);

% everything else is not matched
parse(Input, [{indent, Level, _, Matched, Attributes} | Stack]) ->
  parse(Input, [{newline, Level, Attributes} | close_blocks(Matched, Stack)]);

%% Block-level closings

parse(":::" ++ Tail, Stack = [{newline, _, _} | _]) ->
  parse(Tail, [{div_fence, 3, ":::", progress} | Stack]);
parse([$: | Tail], [{div_fence, Level, Raw, progress} | Stack]) ->
  parse(Tail, [{div_fence, Level + 1, [$: | Raw], progress} | Stack]);
parse([Head | Tail], [{div_fence, Level, Raw, _} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail, [{div_fence, Level, [Head | Raw], done} | Stack]);
parse([10 | Tail], [{div_fence, Level, Raw, _}, {newline, _, Attr} | Stack]) ->
  close_fenced_div(Tail, [10 | Raw], Level, Attr, Stack, []);

parse(Input, [{div_fence, _, Raw, _},
              {newline, Level, Attributes}
              | Stack = [{Element, _, _} | _]])
  when ?CONTAINS_BLOCK(Element) ->
  parse(lists:reverse(Raw) ++ Input, [{newblock, Level, Attributes} | Stack]);
parse(Input, [{div_fence, _, Raw, _}, {newline, _, _}, {hard_break} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input, Stack);
parse(Input, [{div_fence, _, Raw, _}, {newline, _, _} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input, [{soft_break} | Stack]);

parse([Mark | Tail],
      [{newline, _, _}
       | Stack = [{fenced_code, [{fence, Target, Mark} | _], _} | _]]) ->
  parse(Tail, [{code_fence, 1, Target, Mark, [Mark]} | Stack]);
parse([Mark | Tail], [{code_fence, Level, Target, Mark, Raw} | Stack])
  when Level < Target ->
  parse(Tail, [{code_fence, Level + 1, Target, Mark, [Mark | Raw]} | Stack]);
parse([Head | Tail], [{code_fence, Target, Target, Mark, Raw} | Stack])
  when Head =:= 32 orelse Head =:= 9 ->
  parse(Tail, [{code_fence, Target, Target, Mark, [Head | Raw]} | Stack]);
parse([10 | Tail],
      [{code_fence, Target, Target, _, _},
       {fenced_code, [{fence, _, _} | Inner_Attributes], Inner_Contents},
       {Elements, Outer_Attributes, Outer_Contents} | Stack]) ->
  parse(Tail,
        [{indent, 0, "", [], []}
         | lists:reverse([{Elements,
                           Outer_Attributes,
                           [{fenced_code,
                             Inner_Attributes,
                             lists:reverse(Inner_Contents)} | Outer_Contents]}
                          | Stack])]);

parse(Input, [{code_fence, _, _, _, Raw},
              {fenced_code, Attributes, Contents} | Stack]) ->
  parse(Input, [{fenced_code, Attributes, Raw ++ Contents} | Stack]);
parse(Input, [{newline, _, _} | Stack = [{fenced_code, _, _} | _]]) ->
  parse(Input, Stack);

parse([10 | Tail], [{fenced_code, Attributes, Contents} | Stack]) ->
  parse(Tail,
        [{indent, 0, "", [], []}
         | lists:reverse([{fenced_code, Attributes, [10 | Contents]}
                          | Stack])]);
parse([Head | Tail], [{fenced_code, Attributes, Contents} | Stack]) ->
  parse(Tail, [{fenced_code, Attributes, [Head | Contents]} | Stack]);


parse([10 | Tail],
      [{newline, _, _},
       {link_reference,
        [{indent, Level}, {"target", Link} | Attributes],
        Contents = [_|_]}
       | Stack]) ->
  parse(Tail,
        [{indent, 0, "", [], []}
         | lists:reverse([{link_reference,
                           [{indent, Level},
                            {"target", Link ++ lists:reverse(Contents)}
                            | Attributes],  []}
                          | Stack])]);
parse([Head | Tail],
      [Newline = {newline, _, _},
       {link_reference, Attributes, Contents} | Stack])
  when Head =/= 32 andalso Head =/= 9 ->
  parse(Tail,
        [Newline, {link_reference, Attributes, [Head | Contents]} | Stack]);

parse(Input,
      [Newline = {newline, _, _},
       {link_reference, [{indent, _} | Inner_Attributes], Backtrack},
       {Element, Attributes, Contents} | Stack]) ->
  parse(lists:reverse(Backtrack) ++ Input,
        [Newline,
         {Element, Attributes,
          [{link_reference, Inner_Attributes, []} | Contents]}
         | Stack]);

% thematic break

parse(Input = [Head | _],
      [{newline, Level, Attributes} | Stack = [{Element, _, _} | _]])
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
        [{indent, 0, "", [], []}
         | lists:reverse([{thematic_break, Level, Attributes, []} | Stack])]);

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
  parse(Tail, [{indent, 0, "", [], []} | lists:reverse([New | Stack])]);
parse([10 | Tail],
      [{fenced_code, Level, Mark, _, [], Attributes} | Stack]) ->
  New = {fenced_code, [{fence, Level, Mark} | Attributes], []},
  parse(Tail, [{indent, 0, "", [], []} | lists:reverse([New | Stack])]);
parse([10 | Tail],
      [{fenced_code, Level, Mark, _, Alt, Attributes} | Stack]) ->
  New = {fenced_code,
         [{fence, Level, Mark}, {mark, lists:reverse(Alt)} | Attributes],
         []},
  parse(Tail, [{indent, 0, "", [], []} | lists:reverse([New | Stack])]);
parse([10 | Tail],
      [{fenced_code, Level, Mark, _, Alt, Attributes, done} | Stack]) ->
  New = {fenced_code,
         [{fence, Level, Mark}, {mark, lists:reverse(Alt)} | Attributes],
         []},
  parse(Tail, [{indent, 0, "", [], []} | lists:reverse([New | Stack])]);

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
  parse(Tail, [{indent, 0, "", [], []} | lists:reverse([New | Stack])]);

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
  parse(Tail, [{indent, 0, "", [], []} | lists:reverse([New | Stack])]);
parse([10 | Tail],
      [{link_reference, _, Label, Link},
       {newblock, Level, Attributes} | Stack]) ->
  New = {link_reference, [{indent, Level},
                          {"target", lists:reverse(Link)},
                          {"label", lists:reverse(Label)}
                          | Attributes], []},
  parse(Tail, [{indent, 0, "", [], []} | lists:reverse([New | Stack])]);

parse([10 | Tail],
      [{block_attributes, _, Acc}, {newblock, _, Prev_Attr} | Stack]) ->
  parse(Tail, [{indent, 0, "", [], Prev_Attr ++ lists:reverse(Acc)}
               | lists:reverse(Stack)]);
parse([10 | Tail], [{newline, _, Attributes} | Stack]) ->
  parse(Tail, [{indent, 0, "", [], Attributes} | lists:reverse(Stack)]);
parse([10 | Tail], Stack) ->
  parse(Tail, [{indent, 0, "", [], []} | lists:reverse(Stack)]);

%% Block-level openings

% general processing
parse(Input, [{newline, Level, Attributes} | Stack = [{Element, _, _} | _]])
  when ?CONTAINS_BLOCK(Element) ->
  parse(Input, [{newblock, Level, Attributes} | Stack]);
parse(Input, [{newline, _, _}, {hard_break} | Stack]) -> parse(Input, Stack);
parse(Input, [{newline, _, _} | Stack]) ->
   parse(Input, [{soft_break} | Stack]);

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
  parse(Tail, [{attributes, "{", []} | Stack]);

% fall back on paragraph
parse(Input, [{newblock, _, Attributes} | Stack]) ->
  parse(Input, [{para, Attributes, []} | Stack]);

%% Attributes

% fill-in attribute placeholder
parse([${ | Tail = [Next | _]], Stack = [{_, attributes, _} | _])
  when ?SPACE(Next) orelse ?ALPHANUM(Next)
       orelse Next =:= $: orelse Next =:= $- orelse Next =:= $_
       orelse Next =:= $. orelse Next =:= $# orelse Next =:= $% ->
  parse(Tail,
        [{attributes, "{\\", []} | Stack]);
parse(Input, [{Mark, attributes, Contents} | Stack]) ->
  parse(Input, [{Mark, Contents} | Stack]);

% attribute dispatching
parse([Head = 32|Tail], [{attributes, Raw, Acc} | Stack]) ->
  parse(Tail, [{attributes, [Head|Raw], Acc} | Stack]);
parse([Head = $%|Tail], [{attributes, Raw, Acc} | Stack]) ->
  parse(Tail, [{comment, ""}, {attributes, [Head|Raw], Acc} | Stack]);
parse([Head = $#|Tail], [{attributes, Raw, Acc} | Stack]) ->
  parse(Tail, [{name, "identifier", ""},
               {attributes, [Head|Raw], Acc} | Stack]);
parse([Head = $.|Tail], [{attributes, Raw, Acc} | Stack]) ->
  parse(Tail, [{name, "class", ""},
               {attributes, [Head|Raw], Acc} | Stack]);
parse(Input = [Head|_], State = [{attributes, _, _} | _])
  when ?ALPHANUM(Head)
       orelse Head =:= $: orelse Head =:= $- orelse Head =:= $_ ->
  parse(Input, [{key, ""} | State]);

% end attribute parsing
parse([$}|Tail],
      [{attributes, _, Acc}, {Element, attributes, Contents} | Stack]) ->
  parse(Tail, [{Element, lists:reverse(Acc), Contents} | Stack]);

parse([$}|Tail],
      [{attributes, Raw, Acc} | Stack = [{newblock, _, _} | _]]) ->
  parse(Tail, [{block_attributes, [$} | Raw], Acc} | Stack]);

parse([Head | Tail], [{block_attributes, Raw, Acc} | Stack])
  when ?SPACE(Head) ->
  parse(Tail, [{block_attributes, [Head | Raw], Acc} | Stack]);

parse(Input,
      [{block_attributes, Raw, _Acc}, {newblock, _, Attributes} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input, [{para, Attributes, []} | Stack]);

% name (identifier or class)
parse(Input = [$}|_],
      [{name, Key, Value}, {attributes, Raw, Acc} | Stack]) ->
  parse(Input,
        [{attributes, Raw, [{Key, lists:reverse(Value)} | Acc]}
         | Stack]);

parse([Head|Tail], [{name, Key, Value}, {attributes, Raw, Acc} | Stack])
  when ?SPACE(Head) ->
  parse(Tail,
        [{attributes, [Head | Raw], [{Key, lists:reverse(Value)} | Acc]}
         | Stack]);

parse(Input = [Head|_], [{name, _, _}, {attributes, Raw, _} | Stack])
  when ?PUNCTUATION(Head)
       andalso Head =/= $_ andalso Head =/= $- andalso Head =/= $: ->
  parse(lists:reverse(Raw) ++ Input, Stack);

parse([Head|Tail], [{name, Key, Value}, {attributes, Raw, Acc} | Stack]) ->
  parse(Tail,
        [{name, Key, [Head | Value]},
         {attributes, [Head | Raw], Acc}
         | Stack]);

% key
parse([Head = $=|Tail], [{key, Key}, {attributes, Raw, Acc} | Stack]) ->
  parse(Tail,
        [{key, lists:reverse(Key), ""},
         {attributes, [Head | Raw], Acc}
         | Stack]);

parse([Head | Tail], [{key, Key}, {attributes, Raw, Acc} | Stack])
  when ?ALPHANUM(Head)
       orelse Head =:= $: orelse Head =:= $- orelse Head =:= $_ ->
  parse(Tail,
        [{key, [Head | Key]},
         {attributes, [Head | Raw], Acc}
         | Stack]);

parse(Input, [{key, _}, {attributes, Raw, _} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input, Stack);

% value dispatch
parse([Head = $"|Tail], [{key, Key, ""}, {attributes, Raw, Acc} | Stack]) ->
  parse(Tail,
        [{quotedvalue, Key, ""},
         {attributes, [Head | Raw], Acc}
         | Stack]);

parse([Head | Tail], [{key, Key, ""}, {attributes, Raw, Acc} | Stack])
  when ?ALPHANUM(Head)
       orelse Head =:= $: orelse Head =:= $- orelse Head =:= $_ ->
  parse(Tail,
        [{barevalue, Key, [Head]},
         {attributes, [Head | Raw], Acc}
         | Stack]);

parse(Input, [{key, _, ""}, {attributes, Raw, _} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input, Stack);

% quoted value
parse([Head = $" | Tail],
      [{quotedvalue, Key, Value}, {attributes, Raw, Acc} | Stack]) ->
  parse(Tail,
        [{attributes, [Head | Raw], [{Key, lists:reverse(Value)} | Acc]}
         | Stack]);

parse([H1 = $\\ | [H2 = $" | Tail]],
      [{quotedvalue, Key, Value}, {attributes, Raw, Acc} | Stack]) ->
  parse(Tail,
        [{quotedvalue, Key, [H2 | Value]},
         {attributes, [H1, H2 | Raw], Acc}
         | Stack]);

parse([Head | Tail],
      [{quotedvalue, Key, Value}, {attributes, Raw, Acc} | Stack]) ->
  parse(Tail,
        [{quotedvalue, Key, [Head | Value]},
         {attributes, [Head | Raw], Acc}
         | Stack]);

% bare value
parse([Head | Tail], [{barevalue, Key, Value}, {attributes, Raw, Acc} | Stack])
  when ?ALPHANUM(Head)
       orelse Head =:= $: orelse Head =:= $- orelse Head =:= $_ ->
  parse(Tail,
        [{barevalue, Key, [Head | Value]},
         {attributes, [Head | Raw], Acc}
         | Stack]);

parse([Head | Tail], [{barevalue, Key, Value}, {attributes, Raw, Acc} | Stack])
  when ?SPACE(Head) ->
  parse(Tail,
        [{attributes, [Head | Raw], [{Key, lists:reverse(Value)} | Acc]}
         | Stack]);

parse(Input = [$} | _],
      [{barevalue, Key, Value}, {attributes, Raw, Acc} | Stack]) ->
  parse(Input,
        [{attributes, Raw, [{Key, lists:reverse(Value)} | Acc]}
         | Stack]);

% comment
parse(Input = [$}|_],
      [{comment, Value}, {attributes, Raw, Acc} | Stack]) ->
  parse(Input,
        [{attributes, Raw, [{comment, lists:reverse(Value)} | Acc]} | Stack]);

parse([Head = $%|Tail],
      [{comment, Value}, {attributes, Raw, Acc} | Stack]) ->
  parse(Tail,
        [{attributes, [Head | Raw], [{comment, lists:reverse(Value)} | Acc]}
         | Stack]);

parse([Head = _|Tail],
      [{comment, Value}, {attributes, Raw, Acc} | Stack]) ->
  parse(Tail, [{ comment, [Head | Value] },
               { attributes, [Head | Raw], Acc }
               | Stack]);

% escape
parse([$\\ | [Head | Tail]], Stack) when ?PUNCTUATION(Head) ->
  parse(Tail, push_char(Stack, Head));
parse([$\\ | Tail = [10 | _]], Stack) ->
  parse(Tail, [{hard_break} | Stack]);
parse([$\\ | [13 | Tail = [10 | _]]], Stack) ->
  parse(Tail, [{hard_break} | Stack]);

% references
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
parse("`" ++ Tail, [{verbatim, N, opener, 0} | Stack]) ->
  parse(Tail, [{verbatim, N + 1, opener, 0} | Stack]);
parse([Head | Tail], [{verbatim, N, opener, 0} | Stack]) ->
  parse(Tail, [{verbatim, N, [Head], 0} | Stack]);

parse("`" ++ Tail, [{verbatim, N, " ", 0} | Stack]) when N > 1 ->
  parse(Tail, [{verbatim, N, "`", 1} | Stack]);

parse("`" ++ Tail, [{verbatim, N, Text, Closing} | Stack]) ->
  parse(Tail, [{verbatim, N, Text, Closing + 1} | Stack]);
parse(Input, [{verbatim, N, " `" ++ Text, N} | Stack]) ->
  parse(Input, [{verbatim, attributes, lists:reverse("`" ++ Text)} | Stack]);
parse(Input, [{verbatim, N, Text, N} | Stack]) ->
  parse(Input, [{verbatim, attributes, lists:reverse(Text)} | Stack]);

parse([Head | Tail], [{verbatim, N, Text, 0} | Stack]) ->
  parse(Tail, [{verbatim, N, [Head | Text], 0} | Stack]);
parse([Head | Tail], [{verbatim, N, Text, catchup, 0} | Stack]) ->
  parse(Tail, [{verbatim, N, [Head | Text], 0} | Stack]);

parse(Input = [_|_], [{verbatim, N, Text, Catchup} | Stack]) ->
  parse(Input, [{verbatim, N, [$` | Text], catchup, Catchup - 1} | Stack]);
parse(Input = [_|_], [{verbatim, N, Text, catchup, Catchup} | Stack]) ->
  parse(Input, [{verbatim, N, [$` | Text], catchup, Catchup - 1} | Stack]);

parse("`" ++ Tail, Stack) ->
  parse(Tail, [{verbatim, 1, opener, 0} | Stack]);

% forced inline elements
parse("{_" ++ Tail, Stack) -> parse(Tail, [{opener, "{_"} | Stack]);
parse("_}" ++ Tail, Stack) -> close(Tail, Stack, [], "{_", emphasis, "_}");
parse("{*" ++ Tail, Stack) -> parse(Tail, [{opener, "{*"} | Stack]);
parse("*}" ++ Tail, Stack) -> close(Tail, Stack, [], "{*", strong, "*}");
parse("{^" ++ Tail, Stack) -> parse(Tail, [{opener, "{^"} | Stack]);
parse("^}" ++ Tail, Stack) -> close(Tail, Stack, [], "{^", superscript, "^}");
parse("{~" ++ Tail, Stack) -> parse(Tail, [{opener, "{~"} | Stack]);
parse("~}" ++ Tail, Stack) -> close(Tail, Stack, [], "{~", subscript, "~}");
parse("{=" ++ Tail, Stack) -> parse(Tail, [{opener, "{="} | Stack]);
parse("=}" ++ Tail, Stack) -> close(Tail, Stack, [], "{=", mark, "=}");
parse("{-" ++ Tail, Stack) -> parse(Tail, [{opener, "{-"} | Stack]);
parse("-}" ++ Tail, Stack) -> close(Tail, Stack, [], "{-", del, "-}");
parse("{+" ++ Tail, Stack) -> parse(Tail, [{opener, "{+"} | Stack]);
parse("+}" ++ Tail, Stack) -> close(Tail, Stack, [], "{+", add, "+}");

parse("^" ++ Tail, Stack) ->
  close_or_open(Tail, Stack, [], "^", superscript);

% bracketed expressions
parse("![" ++ Tail, Stack) ->
  parse(Tail, [{opener, "["}, {opener, "!"} | Stack]);
parse("[" ++ Tail, Stack) -> parse(Tail, [{opener, "["} | Stack]);
parse("]{" ++ Tail, Stack) -> close("{" ++ Tail, Stack, [], "[", span, "]");
parse("](" ++ Tail, Stack) -> close("(" ++ Tail, Stack, [], "[", brackets, "]");
parse("][" ++ Tail, Stack) -> close("[" ++ Tail, Stack, [], "[", brackets, "]");

parse([Head | Tail], Stack) ->
  parse(Tail, push_char(Stack, Head));

parse([], State) ->
  [Doc | Tail] = lists:reverse(State),
  finalize_block(hd(close_blocks([Doc], Tail))).

parse(Input) -> parse(Input, [{indent, 0, "", [{doc, [], []}], []}]).
