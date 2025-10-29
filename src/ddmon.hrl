-define(LOG_INDENT_SIZE, '$log_indent_size').

-define(RECV_INFO(MsgInfo), {'receive', MsgInfo}).
-define(SEND_INFO(To, MsgInfo), {send, To, MsgInfo}).
-define(PROBE(Probe, Vis), {probe, Probe, Vis}).
-define(QUERY_INFO(ReqId), {query, ReqId}).
-define(RESP_INFO(ReqId), {response, ReqId}).
-define(NOTIFY(From, MsgInfo), {notify, From, MsgInfo}).
-define(HANDLE_RECV(From, MsgInfo), {'receive', From, MsgInfo}).

%% -define(GS_CALL_FROM(From, ReqId), {'$gen_call', {From, [alias|ReqId]}, _}).
-define(GS_CALL_FROM(From, ReqId), {'$gen_call', {From, ReqId}, _}).
-define(GS_CALL(ReqId), ?GS_CALL_FROM(_, ReqId)).
-define(GS_RESP_ALIAS(ReqId), {[alias|ReqId], _Msg}).
-define(GS_RESP(ReqId), {ReqId, _Msg}).

-define(DEADLOCK_PROP(DL), {'$ddmon_deadlock_prop', DL}).
