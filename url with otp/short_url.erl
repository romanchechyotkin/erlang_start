-module(short_url).

-behaviour(gen_server).

-export([start/0, stop/0, restart/0, short/1, long/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% API METHODS
start() ->
    gen_server:start_link({local, shurl}, short_url, [], []).

stop() ->
    gen_server:cast(shurl, stop).

restart() ->
    stop(),
    start().

short(LongURL) ->
    gen_server:call(shurl, {short, LongURL}).

long(ShortURL) ->
    gen_server:call(shurl, {long, ShortURL}).

%% gen_server API
init([]) -> 
    io:format("start server ~n"),
    {ok, dict:new()}.

handle_call({short, LongURL}, _From, State) -> 
    {Res, NewState} = case dict:is_key(LongURL, State) of 
        true -> {dict:fetch(LongURL, State), State};
        false -> 
                ShortURL = "http:/short.by/" ++ rand_string(7),
                {ShortURL, dict:store(LongURL, ShortURL, State)}
    end,
    {reply, Res, NewState};

handle_call({long, ShortURL}, _From, State) ->
    FDict = dict:filter(fun(_Key, Val) -> Val =:= ShortURL end, State),
    FList = dict:to_list(FDict),
    Res = case FList of
        [] -> "";
        [{LongURL, _Val} | _] -> LongURL
    end,
    {reply, Res, State};

handle_call(Msg, From, State) -> 
    error_logger:error_msg("unknown message ~p from ~p ~n", [Msg, From]),
    {noreply, State}.

handle_cast(stop, State) -> 
    io:format("normal stop ~n"),
    {stop, normal, State};

handle_cast(Msg, State) -> 
    error_logger:error_msg("unknown message ~p ~n", [Msg]),
    {noreply, State}.

handle_info(Msg, State) -> 
    error_logger:error_msg("unknown message ~p ~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) -> 
    ok.

code_change(_OldVersion, State, _Extra) -> 
    {ok, State}.


%% INTERNAL METHODS
rand_string(Length) -> 
    L = lists:seq(1, Length),
    lists:flatten([rand_char() || _Index <- L]).

rand_char() -> 
    Chars = "qwertyuiopasdfghjklzxcvbnm",
    Index = rand:uniform(length(Chars)),
    lists:nth(Index, Chars).