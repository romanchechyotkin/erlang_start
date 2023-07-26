-module(short_url).

-export([start/0, stop/0, restart/0, short/1, long/1]).
% -export([rand_char/0, rand_string/1]).


start() ->
    io:format("start called ~n"),
    Pid = spawn(fun() -> loop(dict:new()) end),
    register(short_url_server, Pid),
    Pid.

restart() ->
    stop(),
    start().

stop() ->
    io:format("stop called ~n"),
    short_url_server ! stop,
    ok.

short(LongURL) ->
    io:format("short for ~p called ~n", [LongURL]),
    Uniq = make_ref(),
    short_url_server ! {short, LongURL, self(), Uniq},
    receive 
        {Uniq, Ans} -> Ans
    end.

long(ShortURL) ->
    io:format("long for ~p called ~n", [ShortURL]),
    Uniq = make_ref(),
    short_url_server ! {long, ShortURL, self(), Uniq},
    receive
        {Uniq, Ans} -> Ans
    end.

loop(State) ->
    io:format("~p wait for messages ~n", [self()]),
    receive
        {short, LongURL, From, Uniq} ->
            io:format("short message received ~n"),
            {Res, NewState} = case dict:is_key(LongURL, State) of
                true -> {dict:fetch(LongURL, State), State};
                false -> 
                        ShortURL = "http:/short.by/" ++ rand_string(7),
                        {ShortURL, dict:store(LongURL, ShortURL, State)}
            end,
            From ! {Uniq, Res},
            loop(NewState);
        {long, ShortURL, From, Uniq} ->
            io:format("long message received ~n"),
            FDict = dict:filter(fun(_Key, Val) -> Val =:= ShortURL end, State),
            FList = dict:to_list(FDict),
            Res = case FList of
                [] -> "";
                [{LongURL, _Val} | _] -> LongURL
            end,
            From ! {Uniq, Res},
            loop(State);
        stop -> ok;
        Msg -> io:format("error: unknown message ~p ~n", [Msg]),
            loop(State)
    end.

rand_string(Length) -> 
    L = lists:seq(1, Length),
    lists:flatten([rand_char() || _Index <- L]).

rand_char() -> 
    Chars = "qwertyuiopasdfghjklzxcvbnm",
    Index = rand:uniform(length(Chars)),
    lists:nth(Index, Chars).