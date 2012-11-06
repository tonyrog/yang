-module(yang_xpath_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    [?_test(parse(S)) || S <- exprs()].

parse(S) ->
    try
	{S,{ok,_}} = {S,yang_xpath_parse:parse(yang_xpath_scan:tokens(S))},
	ok
    catch
	error:E ->
	    io:fwrite("ERROR ~p~n~p", [E,erlang:get_stacktrace()]),
	    error(E)
    end.

exprs() ->
     ["/AAA",
      "/AAA/CCC",
      "/AAA/DDD/BBB",
      "//BBB",
      "//DDD/BBB",
      "/AAA/CCC/DDD/*",
      "/*/*/*/BBB",
      "//*",
      "/AAA/BBB[1]",
      "/AAA/BBB[last()]",
      "//@id",
      "//BBB[@id]",
      "//BBB[@name]",
      "//BBB[@*]",
      "//BBB[not(@*)]",
      "//BBB[@id='b1']",
      "//BBB[@name='bbb']",
      "//BBB[normalize-space(@name)='bbb']",
      "//*[count(BBB)=2]",
      "//*[count(*)=2]",
      "//*[count(*)=3]",
      "//*[name()='BBB']",
      "//*[starts-with(name(),'B')]",
      "//*[contains(name(),'C')]",
      "//*[string-length(name()) = 3]",
      "//*[string-length(name()) < 3]",
      "//*[string-length(name()) > 3]",
      "//CCC | //BBB",
      "/AAA/EEE | //BBB",
      "/AAA/EEE | //DDD/CCC | /AAA | //BBB",
      "/AAA",
      "/child::AAA",
      "/AAA/BBB",
      "/child::AAA/child::BBB",
      "/child::AAA/BBB",
      "/descendant::*",
      "/AAA/BBB/descendant::*",
      "//CCC/descendant::*",
      "//DDD/parent::*",
      "/AAA/BBB/DDD/CCC/EEE/ancestor::*",
      "//FFF/ancestor::*",
      "/AAA/BBB/following-sibling::*",
      "//CCC/following-sibling::*",
      "/AAA/XXX/preceding-sibling::*",
      "//CCC/preceding-sibling::*",
      "/AAA/XXX/following::*",
      "//ZZZ/following::*",
      "/AAA/XXX/preceding::*",
      "//GGG/preceding::*",
      "/AAA/XXX/descendant-or-self::*",
      "//CCC/descendant-or-self::*",
      "/AAA/XXX/DDD/EEE/ancestor-or-self::*",
      "//GGG/ancestor-or-self::*",
      "//GGG/ancestor::*",
      "//GGG/descendant::*",
      "//GGG/following::*",
      "//GGG/preceding::*",
      "//GGG/self::*",
      ("//GGG/ancestor::* | //GGG/descendant::* | //GGG/following::* |"
       " //GGG/preceding::* | //GGG/self::*"),
      "//BBB[position() mod 2 = 0 ]",
      ("//BBB[ position() = floor(last() div 2 + 0.5) or"
       " position() = ceiling(last() div 2 + 0.5) ]"),
      ("//CCC[ position() = floor(last() div 2 + 0.5) or"
       " position() = ceiling(last() div 2 + 0.5) ]")
     ].

-endif.
