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

-module(djot_tests).
-export([render_ast/1, render_html/1, read_test_file/1, split_lines/1]).
-include_lib("eunit/include/eunit.hrl").

indent(0, Acc) -> Acc;
indent(Level, Acc) -> indent(Level - 1, "  " ++ Acc).
indent(Level) -> indent(Level, "").

quote(Text) -> "\"" ++ Text ++ "\"".  % TODO: escapes

render_ast_attr(Attributes) -> render_ast_attr(lists:reverse(Attributes), "").
render_ast_attr([], Acc) -> Acc;
render_ast_attr([{Key, Value} | Tail], Acc) ->
  render_ast_attr(Tail, " " ++ Key ++ "=" ++ quote(Value) ++ Acc).

render_ast({Doc, _}) -> lists:reverse(render_ast(Doc, 0, [])).

render_ast([], _, Acc) -> Acc;
render_ast([Head | Tail], Depth, Acc) ->
  render_ast(Tail, Depth, render_ast(Head, Depth, Acc));
render_ast({Element}, Depth, Acc) ->
  [indent(Depth) ++ atom_to_list(Element) ++ "\n" | Acc];
render_ast({symbol, Text}, Depth, Acc) ->
  [indent(Depth) ++ "symb alias=" ++ quote(Text) ++ "\n" | Acc];
render_ast({text, Text}, Depth, Acc) ->
  [indent(Depth) ++ "str text=" ++ quote(Text) ++ "\n" | Acc];
render_ast({Element, Contents}, Depth, Acc) ->
  render_ast(Contents,
             Depth + 1,
             [indent(Depth) ++ atom_to_list(Element) ++ "\n" | Acc]);
render_ast({raw_inline, [{"format", Format}], Contents}, Depth, Acc) ->
  [indent(Depth) ++ "raw_inline text="
                 ++ quote(Contents)
                 ++ " format="
                 ++ quote(Format) ++ "\n"
   | Acc];
render_ast({Element, Attributes, Contents}, Depth, Acc) ->
  render_ast(Contents,
             Depth + 1,
             [indent(Depth) ++ atom_to_list(Element)
                            ++ render_ast_attr(Attributes) ++ "\n"
              | Acc]).

html_body_quote(Text) -> html_body_quote(lists:reverse(Text), []).
html_body_quote([], Acc) -> Acc;
html_body_quote("<" ++ Tail, Acc) -> html_body_quote(Tail, "&lt;" ++ Acc);
html_body_quote(">" ++ Tail, Acc) -> html_body_quote(Tail, "&gt;" ++ Acc);
html_body_quote("&" ++ Tail, Acc) -> html_body_quote(Tail, "&amp;" ++ Acc);
html_body_quote([Head | Tail], Acc) -> html_body_quote(Tail, [Head | Acc]).

html_attr_quote(Text) -> [$" | html_attr_quote(lists:reverse(Text), [$"])].
html_attr_quote([], Acc) -> Acc;
html_attr_quote("<"  ++ Tail, Acc) -> html_attr_quote(Tail, "&lt;" ++ Acc);
html_attr_quote(">"  ++ Tail, Acc) -> html_attr_quote(Tail, "&gt;" ++ Acc);
html_attr_quote("&"  ++ Tail, Acc) -> html_attr_quote(Tail, "&amp;" ++ Acc);
html_attr_quote("\"" ++ Tail, Acc) -> html_attr_quote(Tail, "&quot;" ++ Acc);
html_attr_quote([Head | Tail], Acc) -> html_attr_quote(Tail, [Head | Acc]).

html_attr_map(align) -> "style";
html_attr_map(comment) -> '';
html_attr_map(class) -> "class";
html_attr_map(head) -> '';
html_attr_map(level) -> '';
html_attr_map(start) -> "start";
html_attr_map(tight) -> '';
html_attr_map("identifier") -> "id";
html_attr_map("reference") -> '';
html_attr_map(Key) -> Key.

add_attr_to_map(align, default, Attr_Map) -> Attr_Map;
add_attr_to_map(align, Value, Attr_Map) when is_atom(Value) ->
  add_attr_to_map("style",
                  "text-align: " ++ atom_to_list(Value) ++ ";",
                  Attr_Map);
add_attr_to_map(align, _, Attr_Map) -> Attr_Map;
add_attr_to_map(start, 0, Attr_Map) -> Attr_Map;
add_attr_to_map(start, 1, Attr_Map) -> Attr_Map;
add_attr_to_map(start, Value, Attr_Map) ->
  add_attr_to_map("start", integer_to_list(Value), Attr_Map);
add_attr_to_map(Raw_Key, Value, Attr_Map) ->
  Key = html_attr_map(Raw_Key),
  add_attr_to_map(Key, Value, maps:is_key(Key, Attr_Map), Attr_Map).
add_attr_to_map('', _, _, Attr_Map) -> Attr_Map;
add_attr_to_map(Key = "class", Value, true, Attr_Map) ->
  Prev_Class = maps:get(Key, Attr_Map),
  Sep = case Prev_Class of "" -> ""; _ -> " " end,
  maps:put(Key, Prev_Class ++ Sep ++ Value, Attr_Map);
add_attr_to_map(Key, Value, _, Attr_Map) ->
  maps:put(Key, Value, Attr_Map).

html_attr_list([], State) -> State;
html_attr_list([{Key, Value} | Tail], State) ->
  html_attr_list(Tail,
                 prepend(" " ++ Key ++ "=" ++ html_attr_quote(Value), State)).


% html_attr_list([], _, Attr_Map, State) ->
%   html_attr_list(lists:sort(maps:to_list(Attr_Map)), State);
% html_attr_list([{"target", Value} | Tail], Key, Attr_Map, State) ->
%   html_attr_list(Tail, Key, add_attr_to_map(Key, Value, Attr_Map), State);
% html_attr_list([{Key, Value} | Tail], Target_Key, Attr_Map, State) ->
%   html_attr_list(Tail,
%                  Target_Key,
%                  add_attr_to_map(Key, Value, Attr_Map),
%                  State).
%
% html_attr_list(Attributes, Target_Key, State) ->
%   html_attr_list(Attributes, Target_Key, #{}, State).


build_attr_map([], _, Attr_Map) -> Attr_Map;
build_attr_map([{"target", Value} | Tail], Key, Attr_Map) ->
  build_attr_map(Tail, Key, add_attr_to_map(Key, Value, Attr_Map));
build_attr_map([{Key, Value} | Tail], Target_Key, Attr_Map) ->
  build_attr_map(Tail,
                 Target_Key,
                 add_attr_to_map(Key, Value, Attr_Map)).

fix_attr_list([], Fixed_List, _, _) -> lists:reverse(Fixed_List);
fix_attr_list([{"target", Val} | Tail], Fixed_List, Target_Key, Attr_Map) ->
  fix_attr_list([{Target_Key, Val} | Tail], Fixed_List, Target_Key, Attr_Map);
fix_attr_list([{Raw_Key, _} | Tail], Fixed_List, Target_Key, Attr_Map) ->
  Key = html_attr_map(Raw_Key),
  case maps:get(Key, Attr_Map, "") of
    ""    -> fix_attr_list(Tail, Fixed_List, Target_Key, Attr_Map);
    Value -> fix_attr_list(Tail,
                           [{Key, Value} | Fixed_List],
                           Target_Key,
                           maps:remove(Key, Attr_Map))
  end.

html_attr_list(Attributes, Target_Key, State) ->
  Attr_Map = build_attr_map(Attributes, Target_Key, #{}),
  html_attr_list(fix_attr_list(Attributes, [], Target_Key, Attr_Map), State).

html_open(Tag, Suffix, [], State) ->
  prepend("<" ++ Tag ++ ">" ++ Suffix, State);
html_open(Tag, Suffix, Attributes, State) ->
  prepend(">" ++ Suffix,
          html_attr_list(Attributes, "href", prepend("<" ++ Tag, State))).

render_html({Doc, _}) ->
  {[], Footnotes, Strings} = djot:walk_ast(Doc, {[], {#{}, #{}}, []},
                                           fun pre_html/2, fun post_html/2),
  lists:foldl(fun (X, Y) -> Y ++ X end, "",
              lists:reverse(render_html_fn(Footnotes, Strings))).

render_html_fn({Refs, Defs}, Acc) ->
  case maps:size(Refs) of
    0 -> Acc;
    _ ->
      ["</ol>\n</section>\n" |
       render_html_fn(lists:keysort(2, maps:to_list(Refs)),
                      Defs,
                      ["<section role=\"doc-endnotes\">\n<hr>\n<ol>\n" | Acc])]
  end.
render_html_fn([], _, Acc) -> Acc;
render_html_fn([{Name, Index} | Tail], Defs, Acc) ->
  Text = integer_to_list(Index),
  Back_Link = ["<a href=\"#fnref" ++ Text ++ "\" role=\"doc-backlink\">"
               ++ [16#21A9, 16#FE0E, 16#FE0E]
%              ++ [226,134,169,239,184,142,239,184,142]
               ++ "</a></p>\n"],
  Contents = case maps:get(Name, Defs, []) of
               ["</p>\n" | Rest] -> Back_Link ++ Rest;
               Rest -> Back_Link ++ ["<p>" | Rest]
             end,
  render_html_fn(Tail, Defs,
                 ["</li>\n" | Contents] ++
                 ["<li id=\"fn" ++ Text ++ "\">\n" | Acc]).

smart_punctuation("ellipsis", _) -> "…";
smart_punctuation("em_dash", _) -> "—";
smart_punctuation("en_dash", _) -> "–";
smart_punctuation("left_double_quote", _) -> "“";
smart_punctuation("left_single_quote", _) -> "‘";
smart_punctuation("right_double_quote", _) -> "”";
smart_punctuation("right_single_quote", _) -> "’";
smart_punctuation(_, Fallback) -> Fallback.

tag(add) -> {"ins", "", ""};
tag(block_quote) -> {"blockquote", "\n", "\n"};
tag(caption) -> {"caption", "", "\n"};
tag(cell) -> {"td", "", "\n"};
tag(del) -> {"del", "", ""};
tag(emphasis) -> {"em", "", ""};
tag(fenced_div) -> {"div", "\n", "\n"};
tag(link) -> {"a", "", ""};
tag(list_item) -> {"li", "\n", "\n"};
tag(mark) -> {"mark", "", ""};
tag(para) -> {"p", "", "\n"};
tag(row) -> {"tr", "\n", "\n"};
tag(section) -> {"section", "\n", "\n"};
tag(span) -> {"span", "", ""};
tag(strong) -> {"strong", "", ""};
tag(subscript) -> {"sub", "", ""};
tag(superscript) -> {"sup", "", ""};
tag(table) -> {"table", "\n", "\n"};
tag(thematic_break) -> {"hr", "\n", "\n"}.

prepend(Text, {Context, Footnotes, Output}) ->
  {Context, Footnotes, [Text | Output]}.
prepend(Text_1, Text_2, {Context, Footnotes, Output}) ->
  {Context, Footnotes, [Text_1, Text_2 | Output]}.
push(Atom, {Context, Footnotes, Output}) ->
  {[Atom | Context], Footnotes, Output}.
pop(Atom, {[Atom | Context], Footnotes, Output}) ->
  {Context, Footnotes, Output}.

push_footnote(Name, {Context, Footnotes, Output}) ->
  {[{footnote, Name, Output} | Context], Footnotes, []}.
pop_footnote({[{footnote, Name, Output} | Context],
              {Refs, Defs},
              Contents}) ->
  {Context,
   {Refs, maps:put(Name, Contents, Defs)},
   Output}.
push_footnote_ref(Name, {Context, {Refs, Defs}, Output}) ->
  {New_Refs, Index} = case maps:is_key(Name, Refs) of
                        true  -> {Refs, maps:get(Name, Refs)};
                        false -> New_Index = maps:size(Refs) + 1,
                                 {maps:put(Name, New_Index, Refs), New_Index}
                      end,
  {integer_to_list(Index), {Context, {New_Refs, Defs}, Output}}.

pre_html({text, Text}, State = {[img | _], _, _}) ->
  prepend(html_attr_quote(lists:reverse(Text), []), State);
pre_html(_, State = {[img | _], _, _}) -> State;

pre_html({para, _, _}, State = {[tight_li | _], _, _}) -> State;

pre_html({display_math, Text}, State) ->
  prepend(html_body_quote(Text), "<span class=\"math display\">\\[", State);
pre_html({double_quoted, _}, State) ->
  prepend("“", State);
pre_html({footnote_ref, Name}, State) ->
  pre_html({footnote_ref, [], Name}, State);
pre_html({footnote_ref, Attributes, Name}, State) ->
  {Text, Mid_State} = push_footnote_ref(Name, State),
  prepend("><sup>" ++ Text ++ "</sup></a>",
          html_attr_list(Attributes, "###",
                         prepend("<a id=\"fnref" ++ Text ++ "\" href=\"#fn"
                                  ++ Text ++ "\" role=\"doc-noteref\"",
                                 Mid_State)));
pre_html({hard_break}, State) -> prepend("<br>\n", State);
pre_html({img, _, _}, State) -> push(img, prepend("<img alt=\"", State));
pre_html({inline_math, Text}, State) ->
  prepend(html_body_quote(Text), "<span class=\"math inline\">\\(", State);
pre_html({non_breaking_space}, State) -> prepend("&nbsp;", State);
pre_html({raw_inline, [{"format", "html"}], Text}, State) ->
  prepend(Text, State);
pre_html({raw_inline, _, _}, State) -> State;
pre_html({single_quoted, _}, State) ->
  prepend("‘", State);
pre_html({smart_punctuation, [{"type", Type} | _], Fallback}, State) ->
  prepend(smart_punctuation(Type, Fallback), State);
pre_html({soft_break}, State) -> prepend("\n", State);
pre_html({soft_break, _, _}, State) -> prepend("\n", State);
pre_html({symbol, Text}, State) ->
  prepend(":" ++ html_body_quote(Text) ++ ":", State);
pre_html({text, Text}, State) -> prepend(html_body_quote(Text), State);
pre_html({verbatim, _}, State) -> prepend("<code>", State);
pre_html({verbatim, _, _}, State) -> prepend("<code>", State);

pre_html({cell, [{head, true} | Attributes], _}, State) ->
  html_open("th", "", Attributes, State);
pre_html({code_block, [{lang, "=html"} | _], Text}, State) ->
  prepend(Text, State);
pre_html({code_block, [{lang, "=" ++ _} | _], _}, State) -> State;
pre_html({code_block, [{lang, Lang} | Attributes], Text}, State) ->
  prepend(html_body_quote(Text),
          html_open("code", "", [{class, "language-" ++ Lang}],
          html_open("pre", "", Attributes, State)));
pre_html({code_block, Attributes, Text}, State) ->
  prepend(html_body_quote(Text),
          prepend("<code>",
                  html_open("pre", "", Attributes, State)));
pre_html({footnote, [{"reference", Name} | _], _}, State) ->
  push_footnote(Name, State);
pre_html({fenced_div, Attributes, _}, State) ->
  html_open("div", "\n", [{"class", ""} | Attributes], State);
pre_html({heading, [{level, Level} | Attributes], _}, State) ->
  html_open(lists:flatten(io_lib:format("h~p", [Level])), "",
            Attributes, State);
pre_html({list, [{marker, [_]} | Attributes], _}, State) ->
  html_open("ul", "\n", Attributes, State);
pre_html({list, [{marker, [$1, _]} | Attributes], _}, State) ->
  html_open("ol", "\n", Attributes, State);
pre_html({list, [{marker, "(1)"} | Attributes], _}, State) ->
  html_open("ol", "\n", Attributes, State);
pre_html({list, [{marker, [Marker, _]} | Attributes], _}, State) ->
  html_open("ol", "\n",
            [{"start", ""}, {"type", [Marker]} | Attributes],
            State);
pre_html({list, [{marker, [_, Marker, _]} | Attributes], _}, State) ->
  html_open("ol", "\n",
            [{"start", ""}, {"type", [Marker]} | Attributes],
            State);
pre_html({list_item, [{tight, true}], _}, State) ->
  prepend("<li>\n", push(tight_li, State));
pre_html({list_item, [{tight, false}], _}, State) ->
  prepend("<li>\n", push(loose_li, State));

pre_html({doc, _, _}, State) -> State;
pre_html({reference_definition, _, _}, State) -> State;

pre_html({Element, _}, State) ->
  {Tag, Suffix, _} = tag(Element),
  prepend("<" ++ Tag ++ ">" ++ Suffix, State);
pre_html({Element, Attributes, _}, State) ->
  {Tag, Suffix, _} = tag(Element),
  html_open(Tag, Suffix, Attributes, State).

post_html({img, Attributes, _}, State = {[img | _], _, _}) ->
  prepend(">",
          html_attr_list(Attributes, "src", prepend("\"", pop(img, State))));
post_html(_, State = {[img | _], _, _}) -> State;

post_html({para, _, _}, State = {[tight_li | _], _, _}) ->
  prepend("\n", State);

post_html({display_math, _}, State) -> prepend("\\]</span>", State);
post_html({double_quoted, _}, State) -> prepend("”", State);
post_html({footnote_ref, _}, State) -> State;
post_html({footnote_ref, _, _}, State) -> State;
post_html({hard_break}, State) -> State;
post_html({inline_math, _}, State) -> prepend("\\)</span>", State);
post_html({non_breaking_space}, State) -> State;
post_html({raw_inline, _, _}, State) -> State;
post_html({single_quoted, _}, State) -> prepend("’", State);
post_html({smart_punctuation, _, _}, State) -> State;
post_html({soft_break}, State) -> State;
post_html({soft_break, _, _}, State) -> State;
post_html({symbol, _}, State) -> State;
post_html({text, _}, State) -> State;
post_html({verbatim, Text}, State) ->
  prepend("</code>", html_body_quote(Text), State);
post_html({verbatim, _, Text}, State) ->
  prepend("</code>", html_body_quote(Text), State);

post_html({cell, [{head, true} | _], _}, State) -> prepend("</th>\n", State);
post_html({code_block, [{lang, "=" ++ _} | _], _}, State) -> State;
post_html({code_block, _, _}, State) -> prepend("</code></pre>\n", State);
post_html({footnote, _, _}, State) -> pop_footnote(State);
post_html({heading, [{level, Level} | _], _}, State) ->
  prepend(lists:flatten(io_lib:format("</h~p>\n", [Level])), State);
post_html({list, [{marker, [_]} | _], _}, State) -> prepend("</ul>\n", State);
post_html({list, [{marker, _} | _], _}, State) -> prepend("</ol>\n", State);
post_html({list_item, [{tight, true}], _}, State) ->
  prepend("</li>\n", pop(tight_li, State));
post_html({list_item, [{tight, false}], _}, State) ->
  prepend("</li>\n", pop(loose_li, State));
post_html({thematic_break, _, _}, State) -> State;

post_html({doc, _, _}, State) -> State;
post_html({reference_definition, _, _}, State) -> State;

post_html({Element, _}, State) ->
  {Tag, _, Suffix} = tag(Element),
  prepend("</" ++ Tag ++ ">" ++ Suffix, State);
post_html({Element, _, _}, State) ->
  {Tag, _, Suffix} = tag(Element),
  prepend("</" ++ Tag ++ ">" ++ Suffix, State).

split_lines(Text) -> split_lines(Text, [], []).
split_lines([], [], Acc) -> lists:reverse(Acc);
split_lines([], Line, Acc) -> split_lines([], [], [lists:reverse(Line) | Acc]);
split_lines([10 | Tail], Line, Acc) ->
  split_lines(Tail, [], [lists:reverse([10 | Line]) | Acc]);
split_lines([Head | Tail], Line, Acc) ->
  split_lines(Tail, [Head | Line], Acc).

parse_test_line([$` | Tail], Fence) -> parse_test_line(Tail, Fence + 1);
parse_test_line("\n", Fence)        -> {fence, Fence};
parse_test_line(" a\n",  Fence)     -> {ast, Fence};
parse_test_line(" ap\n", Fence)     -> {unsupported, Fence};
parse_test_line(" m\n", Fence)      -> {unsupported, Fence};
parse_test_line(_, _)               -> text.

parse_test_line(".\n")              -> middle;
parse_test_line(Line = [$` | _])    -> parse_test_line(Line, 0);
parse_test_line(_)                  -> text.

add_test_line(State, Line) ->
  add_test_line(State, Line, parse_test_line(Line)).

add_test_line({comment, Acc}, _, {fence, Fence}) -> {html, Fence, [], Acc};
add_test_line({comment, Acc}, _, {Type,  Fence}) -> {Type, Fence, [], Acc};
add_test_line(State = {comment, _}, _, _) -> State;

add_test_line({Type, Fence, Input, Acc}, _, middle) ->
  {Type, Fence, Input, [], Acc};
add_test_line({Type, Fence, Input, Acc}, Line, _) ->
  {Type, Fence, Input ++ Line, Acc};

add_test_line({Type, Fence, Input, Expected, Acc}, _, {fence, Fence}) ->
  {comment, [{Type, Input, lists:reverse(Expected)} | Acc]};
add_test_line({Type, Fence, Input, Expected, Acc}, Line, _) ->
  {Type, Fence, Input, [Line | Expected], Acc}.

read_test_file(Name) ->
  {ok, File} = file:open(Name, [read, {encoding, utf8}]),
  read_test_file(File, {comment, []}, io:get_line(File, '')).

read_test_file(_, {comment, Acc}, eof) -> lists:reverse(Acc);
read_test_file(_, State, eof) -> {bad_state, State};
read_test_file(File, State, Line) ->
  read_test_file(File, add_test_line(State, Line), io:get_line(File, '')).

make_test([], Acc) -> Acc;
make_test([{unsupported, _, _} | Tail], Acc) ->
  make_test(Tail, Acc);
make_test([{ast, Input, Output} | Tail], Acc) ->
  make_test(Tail,
            [?_assertEqual(Output, render_ast(djot:parse(Input))) | Acc]);
make_test([{html, Input, Output} | Tail], Acc) ->
  make_test(Tail,
            [?_assertEqual(Output, split_lines(render_html(djot:parse(Input))))
             | Acc]).

%import_test_file(Name) -> make_test(read_test_file(Name), []).

maybe_import_test_file(Name, Acc)
  when length(Name) > 5 ->
  L = length(Name),
  Suffix = string:sub_string(Name, L - 4, L),
  if Suffix == ".test" -> make_test(read_test_file(Name), Acc);
     true -> Acc
  end;
maybe_import_test_file(_, Acc) -> Acc.

import_test_files([], Acc) -> lists:reverse(Acc);
import_test_files([Name | Rest], Acc) ->
  Updated_Acc = maybe_import_test_file(Name, Acc),
  import_test_files(Rest, Updated_Acc).

external_test_() ->
  {ok, Filenames} = file:list_dir("."),
  import_test_files(Filenames, []).
