-define(LOG_INDENT_SIZE, '$log_indent_size').

-define(RECV_INFO(MsgInfo), {'$ddt_recv', MsgInfo}).
-define(SEND_INFO(To, MsgInfo), {'$ddt_send', To, MsgInfo}).
-define(PROBE(Probe, Vis), {'$ddt_probe', Probe, Vis}).
-define(QUERY_INFO(ReqId), {'$ddt_query', ReqId}).
-define(RESP_INFO(ReqId), {'$ddt_reply', ReqId}).
-define(HERALD(From, MsgInfo), {'$ddt_herald', From, MsgInfo}).
-define(DEADLOCK_PROP(DL), {'$ddt_deadlock_prop', DL}).

%% -define(GS_CALL_FROM(From, ReqId), {'$gen_call', {From, [alias|ReqId]}, _}).
-define(GS_CALL_FROM_MSG(From, ReqId, Msg), {'$gen_call', {From, ReqId}, Msg}).
-define(GS_CALL_FROM(From, ReqId), ?GS_CALL_FROM_MSG(From, ReqId, _)).
-define(GS_CALL(ReqId), ?GS_CALL_FROM(_, ReqId)).
-define(GS_CALL_MSG(ReqId, Msg), ?GS_CALL_FROM_MSG(_, ReqId, Msg)).

-define(GS_RESP_MSG(ReqId, Msg), {ReqId, Msg}).
-define(GS_RESP_ALIAS_MSG(ReqId, Msg), ?GS_RESP_MSG([alias|ReqId], Msg)).
-define(GS_RESP_ALIAS(ReqId), ?GS_RESP_ALIAS_MSG(ReqId, _)).
-define(GS_RESP(ReqId), ?GS_RESP_MSG(ReqId, _)).

%% Internal states
-define(synced, synced).
-define(wait_proc(From, MsgInfo), {wait_proc, From, MsgInfo}).
-define(wait_mon(MsgInfo), {wait_mon, MsgInfo}).
-define(wait_mon_proc(MsgInfoMon, FromProc, MsgInfoProc), {wait_mon_proc, MsgInfoMon, FromProc, MsgInfoProc}). 
