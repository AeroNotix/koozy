-module(koozy_zk).


-behaviour(gen_server).

-export([register_name/2]).
-export([unregister_name/1]).
-export([whereis_name/1]).

-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([start_link/0]).
-export([terminate/2]).

-export([init/1]).


-record(state, { zk_conn :: pid(),
                 service_prefix = "/koozy/" :: string() }).


register_name(Name, Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {register_name, Name, Pid}).

unregister_name(Name) ->
    gen_server:call(?MODULE, {unregister_name, Name}).

whereis_name(Name) ->
    gen_server:call(?MODULE, {whereis, Name}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> %% TODO, expose setting ZK endpoints
    {ok, Pid} = erlzk:connect([{"localhost", 2181}], 30000),
    {ok, #state{zk_conn=Pid}}.

handle_call({register_name, Name, Pid}, _From,
            State=#state{zk_conn=ZK, service_prefix=Prefix}) ->
    case register_name(ZK, Prefix ++ Name, Pid) of
        {ok, _} ->
            {reply, yes, State};
        {error, already_registered} ->
            {reply, no, State};
        {error, _} = E ->
            lager:warning("Error registering name with ZK: ~p", [E]),
            {reply, no, State}
    end;
handle_call({unregister_name, Name}, _From,
            State=#state{zk_conn=ZK, service_prefix=Prefix}) ->
    case unregister_name(ZK, Prefix ++ Name) of
        ok ->
            {reply, true, State};
        {error, _} = E ->
            lager:warning("Error unregistering name from ZK: ~p", [E]),
            {reply, true, State}
    end;
handle_call({whereis, Name}, _From,
            State=#state{zk_conn=ZK, service_prefix=Prefix}) ->
    case whereis_name(ZK, Prefix ++ Name) of
        {ok, Pid} when is_pid(Pid) ->
            {reply, Pid, State};
        {error, _} = E ->
            lager:warning("Error looking up name with ZK: ~p", [E]),
            {reply, undefined, State}
    end.

register_name(ZKConn, Name, Pid) when is_pid(ZKConn) andalso is_pid(Pid) ->
    case erlzk:create(ZKConn, Name, ephemeral) of
        {ok, Name} ->
            register_name1(ZKConn, Name, Pid);
        {error, node_exists} = E ->
            E
    end.

register_name1(ZKConn, Name, Pid) ->
    BPid = term_to_binary(Pid),
    {ok, _} = erlzk:set_data(ZKConn, Name, BPid).

whereis_name(ZKConn, Name) ->
    case erlzk:get_data(ZKConn, Name) of
        {ok, {Data, _}} ->
            {ok, binary_to_term(Data)};
        {error, _} = E ->
            E
    end.

unregister_name(ZKConn, Name) ->
    case erlzk:delete(ZKConn, Name) of
        ok ->
            ok;
        {error, no_node} ->
            ok;
        {error, _} = E ->
            E
    end.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, State) ->
    {stop, normal, State}.
