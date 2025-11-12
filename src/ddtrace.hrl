-define(LOG_INDENT_SIZE, '$log_indent_size').

-define(RECV_INFO(MsgInfo), {'$ddt_recv', MsgInfo}).
-define(SEND_INFO(To, MsgInfo), {'$ddt_send', To, MsgInfo}).
-define(PROBE(Probe, Vis), {'$ddt_probe', Probe, Vis}).
-define(QUERY_INFO(ReqId), {'$ddt_query', ReqId}).
-define(RESP_INFO(ReqId), {'$ddt_reply', ReqId}).
-define(NOTIFY(From, MsgInfo), {'$ddt_notif', From, MsgInfo}).
-define(DEADLOCK_PROP(DL), {'$ddt_deadlock_prop', DL}).

%% -define(GS_CALL_FROM(From, ReqId), {'$gen_call', {From, [alias|ReqId]}, _}).
-define(GS_CALL_FROM(From, ReqId), {'$gen_call', {From, ReqId}, _}).
-define(GS_CALL(ReqId), ?GS_CALL_FROM(_, ReqId)).
-define(GS_RESP_ALIAS(ReqId), {[alias|ReqId], _Msg}).
-define(GS_RESP(ReqId), {ReqId, _Msg}).

%% Internal states
-define(synced, {wait, []}).
-define(wait(L), {wait, L}).
-define(wait_proc_obj(From, MsgInfo), {proc, From, MsgInfo}).
-define(wait_proc(From, MsgInfo, Rest), ?wait([?wait_proc_obj(From, MsgInfo) | Rest])).
-define(wait_mon_obj(MsgInfo), {mon, MsgInfo}).
-define(wait_mon(MsgInfo, Rest), ?wait([?wait_mon_obj(MsgInfo) | Rest])).
