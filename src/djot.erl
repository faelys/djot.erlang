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
                     orelse Element =:= heading
                     orelse Element =:= para)).

-define(CONTAINS_BLOCK(Element), (Element =:= blockquote
                           orelse Element =:= doc
                           orelse Element =:= fenced_div
                           orelse Element =:= footnote
                           orelse Element =:= list_item)).


finalize_attributes([], Acc) -> lists:reverse(Acc);
finalize_attributes([{indent, _} | Tail], Acc) ->
  finalize_attributes(Tail, Acc);
finalize_attributes([Head | Tail], Acc) ->
  finalize_attributes(Tail, [Head | Acc]).

finalize_block({Element, Attributes, Contents}) ->
  {Element, finalize_attributes(Attributes, []), lists:reverse(Contents)}.

close_blocks(Current, []) -> Current;

close_blocks([{Element, Attributes, Contents} | Blocks],
             [Inner_Block = {Inner_Element, _, _}])
  when ?IS_BLOCK(Inner_Element) ->
  [{Element, Attributes, [finalize_block(Inner_Block) | Contents]} | Blocks];

close_blocks(Current, [Block = {Element, _, _} | Tail])
  when ?IS_BLOCK(Element) ->
  close_blocks(Current, close_blocks([Block], Tail));

close_blocks([{Element, Attributes, Contents} | Blocks],
             [{text, Text} | Tail]) ->
  close_blocks([{Element, Attributes, [{text, lists:reverse(Text)} | Contents]}
                | Blocks],
               Tail);
close_blocks([{Element, Attributes, Contents} | Blocks], [Head | Tail]) ->
  close_blocks([{Element, Attributes, [Head | Contents]} | Blocks],
               Tail).

maybe_add_para(Stack = [{Element, _, _} | _]) when ?CONTAINS_BLOCK(Element) ->
  [{para, [], []} | Stack];
maybe_add_para(Stack) -> Stack.

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

%% Implicit block continuation

parse(Input, [{indent, Level, Text, Matched}, Doc = {doc, _, _} | Stack]) ->
  parse(Input, [{indent, Level, Text, [Doc | Matched]} | Stack]);

parse([32 | Tail], [{indent, Level, Text, Matched} | Stack]) ->
  parse(Tail, [{indent, Level + 1, [32 | Text], Matched} | Stack]);
parse([9 | Tail], [{indent, Level, Text, Matched} | Stack]) ->
  parse(Tail, [{indent, Level + 1, [9 | Text], Matched} | Stack]);

parse([$>, 32 | Tail],
      [{indent, Level, Text, Matched},
       Block = {blockquote, _, _}
       | Stack]) ->
  parse(Tail,
        [{indent, Level + 2, [32, $> | Text], [Block | Matched]} | Stack]);

parse([$> | Tail = [10 | _]],
      [{indent, Level, Text, Matched},
       Block = {blockquote, _, _}
       | Stack]) ->
  parse(Tail,
        [{indent, Level + 2, [32, $> | Text], [Block | Matched]} | Stack]);

parse(Input = [Next | _],
      [{indent, Level, Text, Matched},
       Block = {para, _, _}
       | Stack])
  when not ?SPACE(Next) ->
  parse(Input,
        [{indent, Level, Text, [Block | Matched]} | Stack]);

parse([$# | Tail],
      Stack = [{indent, _, _, _}, {heading, [{level, Level}|_], _} | _]) ->
  parse(Tail,
        [{heading, 1, Level, "#"} | Stack]);
parse(Input = [32 | _],
      [Top = {heading, N, N, _},
       {indent, Level, Text, Matched},
       Element | Stack]) ->
  parse(Input,
        [Top, {indent, Level, Text, [Element | Matched]} | Stack]);
parse([32 | Tail],
      [{heading, N, N, _},
       {indent, _Level, _Text, Matched}]) ->
  parse(Tail, Matched);
parse([$# | Tail],
      [{heading, Current, Target, Text} | Stack = [{indent, _, _, _} | _]])
  when Current < Target ->
  parse(Tail,
        [{heading, Current + 1, Target, [$# | Text]} | Stack]);
parse(Input, [{heading, _, _, Text}, {indent, _, _, Matched} | Stack]) ->
  parse(lists:reverse(Text) ++ Input,
        [{newline, 0} | close_blocks(Matched, Stack)]);

parse(Input, [{indent, _, _, Matched} | Stack]) ->
  parse(Input, [{newline, 0} | close_blocks(Matched, Stack)]);

parse([10 | Tail], [{newline, _} | Stack]) ->
  parse(Tail, [{indent, 0, "", []} | lists:reverse(Stack)]);
parse([10 | Tail], Stack) ->
  parse(Tail, [{indent, 0, "", []} | lists:reverse(Stack)]);

%% Block-level elements


%% Attributes

% fill-in attribute placeholder
parse([${ | [Head | Tail]], Stack = [{_, attributes, _} | _])
  when ?SPACE(Head) orelse ?ALPHANUM(Head)
       orelse Head =:= $: orelse Head =:= $- orelse Head =:= $_
       orelse Head =:= $. orelse Head =:= $# orelse Head =:= $% ->
  parse([Head | Tail],
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

parse([$}|Tail], [{attributes, Raw, Acc} | Stack]) ->
  parse(Tail, [{block_attributes, [$} | Raw], Acc} | Stack]);

parse([Head | Tail], [{block_attributes, Raw, Acc} | Stack])
  when ?SPACE(Head) ->
  parse(Tail, [{block_attributes, [Head | Raw], Acc} | Stack]);

parse(Input, [{block_attributes, Raw, _Acc} | Stack]) ->
  parse(lists:reverse(Raw) ++ Input, Stack);

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

parse([Head | Tail], Stack) ->
  parse(Tail, push_char(Stack, Head));

parse([], State) ->
  [Doc | Tail] = lists:reverse(State),
  finalize_block(hd(close_blocks([Doc], Tail))).

parse(Input) -> parse(Input, [{indent, 0, "", [{doc, [], []}]}]).
