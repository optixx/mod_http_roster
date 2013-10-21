-module(mod_global_roster_http).

-behavior(gen_mod).

-include("ejabberd.hrl").
-export([start/2, stop/1, on_presence_joined/4, on_presence_left/4]).


start(Host, _Opts) ->
  ?INFO_MSG("mod_global_roster_http starting", []),
  ibrowse:start(),
  ejabberd_hooks:add(set_presence_hook, Host, ?MODULE, on_presence_joined, 50),
  ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, on_presence_left, 50),
  ok.

stop(Host) ->
  ?INFO_MSG("mod_global_roster_http stopping", []),
  ejabberd_hooks:remove(set_presence_hook, Host, ?MODULE, on_presence_joined, 50),
  ejabberd_hooks:remove(unset_presence_hook, Host, ?MODULE, on_presence_left, 50),
  ok.
  
on_presence_joined(User, Server, _Resource, _Packet) ->
  ?INFO_MSG("mod_global_roster_http joined user=~s resource=~p", [User,_Resource]),
  ibrowse:send_req(lists:concat([url(Server),"/api/chat/operator/presence/?user=",User,"&resource=",_Resource,"&action=joined"]), [], get),
  none.

on_presence_left(User, Server, _Resource, _Status) ->
  ?INFO_MSG("mod_global_roster_http left user=~s resource=~p", [User,_Resource]),
  ibrowse:send_req(lists:concat([url(Server),"/api/chat/operator/presence/?user=",User,"&resource=",_Resource,"&action=left"]), [], get),
  none.


url(Server) ->
  gen_mod:get_module_opt(Server, ?MODULE, url, "http://127.0.0.1:5000").

